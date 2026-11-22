//! TLS record protection over SPARK AES-GCM (SPARKTLSCrypto) and
//! ChaCha20-Poly1305 (SPARKTLSCrypto seal, SPARKNaCl open): the cipher
//! suites, in rustls's (aws-lc's) order of preference.
use rustls::crypto::cipher::{
    AeadKey, InboundOpaqueMessage, InboundPlainMessage, Iv, KeyBlockShape, MessageDecrypter,
    MessageEncrypter, Nonce, OutboundOpaqueMessage, OutboundPlainMessage, PrefixedPayload,
    Tls12AeadAlgorithm, Tls13AeadAlgorithm, UnsupportedOperationError, make_tls12_aad,
    make_tls13_aad, NONCE_LEN,
};
use rustls::crypto::tls12::PrfUsingHmac;
use rustls::crypto::tls13::HkdfUsingHmac;
use rustls::crypto::{CipherSuiteCommon, KeyExchangeAlgorithm};
use rustls::{
    CipherSuite, ConnectionTrafficSecrets, ContentType, Error, ProtocolVersion, SignatureScheme,
    SupportedCipherSuite, Tls12CipherSuite, Tls13CipherSuite,
};

use crate::{ffi, hash, hmac};

const TAG_LEN: usize = 16;
const GCM_EXPLICIT_NONCE_LEN: usize = 8;
/// rustls's MAX_FRAGMENT_LEN: a TLS 1.2 record's plaintext.
const MAX_FRAGMENT_LEN: usize = 16384;
/// AES-GCM's usage limit (RFC 8446 5.5); ChaCha20-Poly1305 has none.
const GCM_CONFIDENTIALITY_LIMIT: u64 = 1 << 24;

/// A prepared AES-GCM key (penny_aes_gcm_prepare): the key schedule and the
/// GHASH powers are computed once, so each record is sealed or opened in one
/// fused pass. Cleared and freed on drop.
struct GcmContext(core::ptr::NonNull<core::ffi::c_void>);

// SAFETY: each context belongs to exactly one cipher; seal and open only
// read it (SPARKTLSCrypto takes the prepared key as an `in` parameter).
unsafe impl Send for GcmContext {}
unsafe impl Sync for GcmContext {}

impl GcmContext {
    fn new(aead: Aead, key: &[u8]) -> Option<Self> {
        if matches!(aead, Aead::ChaCha20Poly1305) {
            return None;
        }
        crate::init();
        // SAFETY: key has the AES key length (16 or 32 bytes).
        core::ptr::NonNull::new(unsafe { ffi::penny_aes_gcm_prepare(key.as_ptr(), key.len() as u32) }).map(Self)
    }
}

impl Drop for GcmContext {
    fn drop(&mut self) {
        // SAFETY: the pointer came from penny_aes_gcm_prepare and is freed once.
        unsafe { ffi::penny_aes_gcm_free(self.0.as_ptr()) }
    }
}

/// A cipher's key: the raw key (ChaCha20, and the fallback if preparing
/// failed) and, for AES-GCM, its prepared context.
struct CipherKey {
    key: AeadKey,
    gcm: Option<GcmContext>,
}

impl CipherKey {
    fn new(aead: Aead, key: AeadKey) -> Self {
        let gcm = GcmContext::new(aead, key.as_ref());
        Self { key, gcm }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Aead {
    Aes128Gcm,
    Aes256Gcm,
    ChaCha20Poly1305,
}

impl Aead {
    fn key_len(self) -> usize {
        match self {
            Self::Aes128Gcm => 16,
            _ => 32,
        }
    }

    /// Encrypt `buf` in place; the tag goes to `tag`.
    fn seal(self, ck: &CipherKey, nonce: &[u8; NONCE_LEN], aad: &[u8], buf: &mut [u8], tag: &mut [u8; TAG_LEN])
        -> Result<(), Error> {
        let key: &[u8] = ck.key.as_ref();
        crate::init();
        let (alen, len) = (crate::len32(aad).ok_or(Error::EncryptError)?, crate::len32(buf).ok_or(Error::EncryptError)?);
        // SAFETY: every buffer has the documented size (key by key_len).
        let ok = unsafe {
            match self {
                Self::ChaCha20Poly1305 => ffi::penny_chacha20_poly1305_seal(
                    key.as_ptr(), nonce.as_ptr(), aad.as_ptr(), alen, buf.as_mut_ptr(), len, tag.as_mut_ptr()),
                _ => match &ck.gcm {
                    Some(g) => ffi::penny_aes_gcm_seal_prepared(g.0.as_ptr(), nonce.as_ptr(), aad.as_ptr(), alen,
                                                                buf.as_mut_ptr(), len, tag.as_mut_ptr()),
                    None => ffi::penny_aes_gcm_seal(key.as_ptr(), key.len() as u32, nonce.as_ptr(), aad.as_ptr(), alen,
                                                    buf.as_mut_ptr(), len, tag.as_mut_ptr()),
                },
            }
        };
        if ok == 1 { Ok(()) } else { Err(Error::EncryptError) }
    }

    /// Decrypt `buf` in place if `tag` authenticates it.
    fn open(self, ck: &CipherKey, nonce: &[u8; NONCE_LEN], aad: &[u8], buf: &mut [u8], tag: &[u8])
        -> Result<(), Error> {
        let key: &[u8] = ck.key.as_ref();
        crate::init();
        let (alen, len) = (crate::len32(aad).ok_or(Error::DecryptError)?, crate::len32(buf).ok_or(Error::DecryptError)?);
        if tag.len() != TAG_LEN {
            return Err(Error::DecryptError);
        }
        // SAFETY: as seal; tag is TAG_LEN bytes.
        let ok = unsafe {
            match self {
                Self::ChaCha20Poly1305 => ffi::penny_chacha20_poly1305_open(
                    key.as_ptr(), nonce.as_ptr(), aad.as_ptr(), alen, buf.as_mut_ptr(), len, tag.as_ptr()),
                _ => match &ck.gcm {
                    Some(g) => ffi::penny_aes_gcm_open_prepared(g.0.as_ptr(), nonce.as_ptr(), aad.as_ptr(), alen,
                                                                buf.as_mut_ptr(), len, tag.as_ptr()),
                    None => ffi::penny_aes_gcm_open(key.as_ptr(), key.len() as u32, nonce.as_ptr(), aad.as_ptr(), alen,
                                                    buf.as_mut_ptr(), len, tag.as_ptr()),
                },
            }
        };
        if ok == 1 { Ok(()) } else { Err(Error::DecryptError) }
    }

    fn secrets(self, key: AeadKey, iv: Iv) -> ConnectionTrafficSecrets {
        match self {
            Self::Aes128Gcm => ConnectionTrafficSecrets::Aes128Gcm { key, iv },
            Self::Aes256Gcm => ConnectionTrafficSecrets::Aes256Gcm { key, iv },
            Self::ChaCha20Poly1305 => ConnectionTrafficSecrets::Chacha20Poly1305 { key, iv },
        }
    }
}

// TLS 1.3 ------------------------------------------------------------------

impl Tls13AeadAlgorithm for Aead {
    fn encrypter(&self, key: AeadKey, iv: Iv) -> Box<dyn MessageEncrypter> {
        Box::new(Tls13Cipher { aead: *self, key: CipherKey::new(*self, key), iv })
    }
    fn decrypter(&self, key: AeadKey, iv: Iv) -> Box<dyn MessageDecrypter> {
        Box::new(Tls13Cipher { aead: *self, key: CipherKey::new(*self, key), iv })
    }
    fn key_len(&self) -> usize {
        Aead::key_len(*self)
    }
    fn extract_keys(&self, key: AeadKey, iv: Iv) -> Result<ConnectionTrafficSecrets, UnsupportedOperationError> {
        Ok(self.secrets(key, iv))
    }
}

struct Tls13Cipher {
    aead: Aead,
    key: CipherKey,
    iv: Iv,
}

impl MessageEncrypter for Tls13Cipher {
    fn encrypt(&mut self, m: OutboundPlainMessage<'_>, seq: u64) -> Result<OutboundOpaqueMessage, Error> {
        let total_len = self.encrypted_payload_len(m.payload.len());
        let mut payload = PrefixedPayload::with_capacity(total_len);
        payload.extend_from_chunks(&m.payload);
        payload.extend_from_slice(&m.typ.to_array());
        let nonce = Nonce::new(&self.iv, seq).0;
        let aad = make_tls13_aad(total_len);
        let mut tag = [0u8; TAG_LEN];
        self.aead.seal(&self.key, &nonce, &aad, payload.as_mut(), &mut tag)?;
        payload.extend_from_slice(&tag);
        Ok(OutboundOpaqueMessage::new(ContentType::ApplicationData, ProtocolVersion::TLSv1_2, payload))
    }
    fn encrypted_payload_len(&self, payload_len: usize) -> usize {
        payload_len + 1 + TAG_LEN
    }
}

impl MessageDecrypter for Tls13Cipher {
    fn decrypt<'a>(&mut self, mut m: InboundOpaqueMessage<'a>, seq: u64) -> Result<InboundPlainMessage<'a>, Error> {
        let payload = &mut m.payload;
        if payload.len() < TAG_LEN {
            return Err(Error::DecryptError);
        }
        let nonce = Nonce::new(&self.iv, seq).0;
        let aad = make_tls13_aad(payload.len());
        let plain_len = payload.len() - TAG_LEN;
        let (body, tag) = payload.split_at_mut(plain_len);
        self.aead.open(&self.key, &nonce, &aad, body, tag)?;
        payload.truncate(plain_len);
        m.into_tls13_unpadded_message()
    }
}

// TLS 1.2 ------------------------------------------------------------------

impl Tls12AeadAlgorithm for Aead {
    fn encrypter(&self, key: AeadKey, iv: &[u8], explicit: &[u8]) -> Box<dyn MessageEncrypter> {
        Box::new(Tls12Cipher { aead: *self, key: CipherKey::new(*self, key), iv: self.record_iv(iv, explicit) })
    }
    fn decrypter(&self, key: AeadKey, iv: &[u8]) -> Box<dyn MessageDecrypter> {
        // A GCM decrypter's nonce takes its explicit part from each record.
        Box::new(Tls12Cipher { aead: *self, key: CipherKey::new(*self, key), iv: self.record_iv(iv, &[0u8; GCM_EXPLICIT_NONCE_LEN]) })
    }
    fn key_block_shape(&self) -> KeyBlockShape {
        match self {
            Self::ChaCha20Poly1305 => KeyBlockShape { enc_key_len: 32, fixed_iv_len: 12, explicit_nonce_len: 0 },
            _ => KeyBlockShape { enc_key_len: self.key_len(), fixed_iv_len: 4, explicit_nonce_len: GCM_EXPLICIT_NONCE_LEN },
        }
    }
    fn extract_keys(&self, key: AeadKey, iv: &[u8], explicit: &[u8])
        -> Result<ConnectionTrafficSecrets, UnsupportedOperationError> {
        Ok(self.secrets(key, self.record_iv(iv, explicit)))
    }
}

impl Aead {
    /// The 12-byte starting point of a TLS 1.2 record nonce: ChaCha20's fixed
    /// IV, or GCM's 4-byte salt and the key block's 8 explicit bytes (as
    /// rustls's other providers construct it).
    fn record_iv(self, iv: &[u8], explicit: &[u8]) -> Iv {
        match self {
            Self::ChaCha20Poly1305 => Iv::copy(iv),
            _ => {
                let mut bytes = [0u8; NONCE_LEN];
                bytes[..4].copy_from_slice(&iv[..4]);
                bytes[4..].copy_from_slice(&explicit[..GCM_EXPLICIT_NONCE_LEN]);
                Iv::new(bytes)
            },
        }
    }
}

struct Tls12Cipher {
    aead: Aead,
    key: CipherKey,
    iv: Iv,
}

impl MessageEncrypter for Tls12Cipher {
    fn encrypt(&mut self, m: OutboundPlainMessage<'_>, seq: u64) -> Result<OutboundOpaqueMessage, Error> {
        let total_len = self.encrypted_payload_len(m.payload.len());
        let mut payload = PrefixedPayload::with_capacity(total_len);
        let nonce = Nonce::new(&self.iv, seq).0;
        let aad = make_tls12_aad(seq, m.typ, m.version, m.payload.len());
        let explicit = match self.aead {
            Aead::ChaCha20Poly1305 => 0,
            _ => GCM_EXPLICIT_NONCE_LEN,
        };
        payload.extend_from_slice(&nonce[NONCE_LEN - explicit..]);
        payload.extend_from_chunks(&m.payload);
        let mut tag = [0u8; TAG_LEN];
        self.aead.seal(&self.key, &nonce, &aad, &mut payload.as_mut()[explicit..], &mut tag)?;
        payload.extend_from_slice(&tag);
        Ok(OutboundOpaqueMessage::new(m.typ, m.version, payload))
    }
    fn encrypted_payload_len(&self, payload_len: usize) -> usize {
        match self.aead {
            Aead::ChaCha20Poly1305 => payload_len + TAG_LEN,
            _ => payload_len + GCM_EXPLICIT_NONCE_LEN + TAG_LEN,
        }
    }
}

impl MessageDecrypter for Tls12Cipher {
    fn decrypt<'a>(&mut self, mut m: InboundOpaqueMessage<'a>, seq: u64) -> Result<InboundPlainMessage<'a>, Error> {
        let explicit = match self.aead {
            Aead::ChaCha20Poly1305 => 0,
            _ => GCM_EXPLICIT_NONCE_LEN,
        };
        let payload = &mut m.payload;
        if payload.len() < explicit + TAG_LEN {
            return Err(Error::DecryptError);
        }
        let nonce = match self.aead {
            Aead::ChaCha20Poly1305 => Nonce::new(&self.iv, seq).0,
            _ => {
                let mut nonce = [0u8; NONCE_LEN];
                nonce[..4].copy_from_slice(&self.iv.as_ref()[..4]);
                nonce[4..].copy_from_slice(&payload[..GCM_EXPLICIT_NONCE_LEN]);
                nonce
            },
        };
        let plain_len = payload.len() - explicit - TAG_LEN;
        let aad = make_tls12_aad(seq, m.typ, m.version, plain_len);
        let (body, tag) = payload[explicit..].split_at_mut(plain_len);
        self.aead.open(&self.key, &nonce, &aad, body, tag)?;
        if plain_len > MAX_FRAGMENT_LEN {
            return Err(Error::PeerSentOversizedRecord);
        }
        Ok(m.into_plain_message_range(explicit..explicit + plain_len))
    }
}

// Cipher suites ------------------------------------------------------------

static SHA256: hash::Sha256 = hash::Sha256;
static SHA384: hash::Sha384 = hash::Sha384;
static HKDF256: HkdfUsingHmac<'static> = HkdfUsingHmac(&hmac::Sha256Hmac);
static HKDF384: HkdfUsingHmac<'static> = HkdfUsingHmac(&hmac::Sha384Hmac);
static PRF256: PrfUsingHmac<'static> = PrfUsingHmac(&hmac::Sha256Hmac);
static PRF384: PrfUsingHmac<'static> = PrfUsingHmac(&hmac::Sha384Hmac);

const fn tls13(suite: CipherSuite, aead: &'static Aead, sha384: bool) -> Tls13CipherSuite {
    Tls13CipherSuite {
        common: CipherSuiteCommon {
            suite,
            hash_provider: if sha384 { &SHA384 } else { &SHA256 },
            confidentiality_limit: match aead {
                Aead::ChaCha20Poly1305 => u64::MAX,
                _ => GCM_CONFIDENTIALITY_LIMIT,
            },
        },
        hkdf_provider: if sha384 { &HKDF384 } else { &HKDF256 },
        aead_alg: aead,
        quic: None,
    }
}

static AES128: Aead = Aead::Aes128Gcm;
static AES256: Aead = Aead::Aes256Gcm;
static CHACHA: Aead = Aead::ChaCha20Poly1305;

static TLS13_AES_256_GCM_SHA384: Tls13CipherSuite = tls13(CipherSuite::TLS13_AES_256_GCM_SHA384, &AES256, true);
static TLS13_AES_128_GCM_SHA256: Tls13CipherSuite = tls13(CipherSuite::TLS13_AES_128_GCM_SHA256, &AES128, false);
static TLS13_CHACHA20_POLY1305_SHA256: Tls13CipherSuite =
    tls13(CipherSuite::TLS13_CHACHA20_POLY1305_SHA256, &CHACHA, false);

static ECDSA_SCHEMES: &[SignatureScheme] = &[
    SignatureScheme::ED25519,
    SignatureScheme::ECDSA_NISTP384_SHA384,
    SignatureScheme::ECDSA_NISTP256_SHA256,
];
static RSA_SCHEMES: &[SignatureScheme] = &[
    SignatureScheme::RSA_PSS_SHA512,
    SignatureScheme::RSA_PSS_SHA384,
    SignatureScheme::RSA_PSS_SHA256,
    SignatureScheme::RSA_PKCS1_SHA512,
    SignatureScheme::RSA_PKCS1_SHA384,
    SignatureScheme::RSA_PKCS1_SHA256,
];

const fn tls12(suite: CipherSuite, aead: &'static Aead, sha384: bool, sign: &'static [SignatureScheme])
    -> Tls12CipherSuite {
    Tls12CipherSuite {
        common: CipherSuiteCommon {
            suite,
            hash_provider: if sha384 { &SHA384 } else { &SHA256 },
            confidentiality_limit: match aead {
                Aead::ChaCha20Poly1305 => u64::MAX,
                _ => GCM_CONFIDENTIALITY_LIMIT,
            },
        },
        kx: KeyExchangeAlgorithm::ECDHE,
        sign,
        prf_provider: if sha384 { &PRF384 } else { &PRF256 },
        aead_alg: aead,
    }
}

static ECDSA_AES256: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384, &AES256, true, ECDSA_SCHEMES);
static ECDSA_AES128: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256, &AES128, false, ECDSA_SCHEMES);
static ECDSA_CHACHA: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256, &CHACHA, false, ECDSA_SCHEMES);
static RSA_AES256: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384, &AES256, true, RSA_SCHEMES);
static RSA_AES128: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256, &AES128, false, RSA_SCHEMES);
static RSA_CHACHA: Tls12CipherSuite =
    tls12(CipherSuite::TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256, &CHACHA, false, RSA_SCHEMES);

pub(crate) static ALL_CIPHER_SUITES: &[SupportedCipherSuite] = &[
    SupportedCipherSuite::Tls13(&TLS13_AES_256_GCM_SHA384),
    SupportedCipherSuite::Tls13(&TLS13_AES_128_GCM_SHA256),
    SupportedCipherSuite::Tls13(&TLS13_CHACHA20_POLY1305_SHA256),
    SupportedCipherSuite::Tls12(&ECDSA_AES256),
    SupportedCipherSuite::Tls12(&ECDSA_AES128),
    SupportedCipherSuite::Tls12(&ECDSA_CHACHA),
    SupportedCipherSuite::Tls12(&RSA_AES256),
    SupportedCipherSuite::Tls12(&RSA_AES128),
    SupportedCipherSuite::Tls12(&RSA_CHACHA),
];
