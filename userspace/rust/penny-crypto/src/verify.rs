//! Certificate and handshake signature verification over SPARKTLSCrypto:
//! the webpki algorithms rustls's aws-lc provider offers, less P-521 and
//! ML-DSA (not in SPARKTLSCrypto yet: such a chain is refused as an
//! unsupported signature algorithm).
use rustls::SignatureScheme;
use rustls::crypto::WebPkiSupportedAlgorithms;
use rustls::pki_types::{AlgorithmIdentifier, InvalidSignature, SignatureVerificationAlgorithm, alg_id};

use crate::{der, ffi};

#[derive(Clone, Copy, Debug)]
enum Digest {
    Sha256,
    Sha384,
    Sha512,
}

impl Digest {
    fn of(self, message: &[u8]) -> Option<Vec<u8>> {
        Some(match self {
            Self::Sha256 => crate::sha256(message)?.to_vec(),
            Self::Sha384 => crate::sha384(message)?.to_vec(),
            Self::Sha512 => crate::sha512(message)?.to_vec(),
        })
    }
}

#[derive(Clone, Copy, Debug)]
enum Curve {
    P256,
    P384,
}

#[derive(Debug)]
enum Scheme {
    Ecdsa(Curve, Digest),
    RsaPkcs1(Digest),
    RsaPss(Digest),
    Ed25519,
}

#[derive(Debug)]
struct Algorithm {
    public_key: AlgorithmIdentifier,
    signature: AlgorithmIdentifier,
    scheme: Scheme,
}

/// RSA keys webpki's aws-lc algorithms accept: 2048 to 8192 bits.
const RSA_MIN_BYTES: usize = 2048 / 8;
const RSA_MAX_BYTES: usize = 8192 / 8;

impl SignatureVerificationAlgorithm for Algorithm {
    fn public_key_alg_id(&self) -> AlgorithmIdentifier {
        self.public_key
    }

    fn signature_alg_id(&self) -> AlgorithmIdentifier {
        self.signature
    }

    fn verify_signature(&self, public_key: &[u8], message: &[u8], signature: &[u8])
        -> Result<(), InvalidSignature> {
        crate::init();
        let valid = match self.scheme {
            Scheme::Ecdsa(curve, digest) => {
                let size = match curve {
                    Curve::P256 => 32,
                    Curve::P384 => 48,
                };
                // Uncompressed points only (all a CA issues).
                if public_key.len() != 1 + 2 * size || public_key[0] != 0x04 {
                    return Err(InvalidSignature);
                }
                let (x, y) = public_key[1..].split_at(size);
                let (r, s) = der::ecdsa_signature(signature, size).ok_or(InvalidSignature)?;
                let h = digest.of(message).ok_or(InvalidSignature)?;
                // SAFETY: digest h.len(), coordinates and r, s of `size` bytes.
                unsafe {
                    match curve {
                        Curve::P256 => ffi::penny_ecdsa_p256_verify(h.as_ptr(), h.len() as u32, x.as_ptr(), y.as_ptr(),
                                                                    r.as_ptr(), s.as_ptr()),
                        Curve::P384 => ffi::penny_ecdsa_p384_verify(h.as_ptr(), h.len() as u32, x.as_ptr(), y.as_ptr(),
                                                                    r.as_ptr(), s.as_ptr()),
                    }
                }
            },
            Scheme::RsaPkcs1(digest) | Scheme::RsaPss(digest) => {
                let (n, e) = der::rsa_public_key(public_key).ok_or(InvalidSignature)?;
                if !(RSA_MIN_BYTES..=RSA_MAX_BYTES).contains(&n.len()) || e < 3 || e % 2 == 0
                    || signature.len() != n.len() {
                    return Err(InvalidSignature);
                }
                let h = digest.of(message).ok_or(InvalidSignature)?;
                let verify = match self.scheme {
                    Scheme::RsaPss(_) => ffi::penny_rsa_pss_verify,
                    _ => ffi::penny_rsa_pkcs1_verify,
                };
                // SAFETY: digest, modulus and signature by their lengths.
                unsafe { verify(h.as_ptr(), h.len() as u32, n.as_ptr(), n.len() as u32, e,
                                signature.as_ptr(), signature.len() as u32) }
            },
            Scheme::Ed25519 => {
                if public_key.len() != 32 || signature.len() != 64 {
                    return Err(InvalidSignature);
                }
                let len = crate::len32(message).ok_or(InvalidSignature)?;
                // SAFETY: key 32, message len, signature 64.
                unsafe { ffi::penny_ed25519_verify(public_key.as_ptr(), message.as_ptr(), len, signature.as_ptr()) }
            },
        };
        if valid == 1 { Ok(()) } else { Err(InvalidSignature) }
    }
}

/// sha{256,384,512}WithRSAEncryption without the NULL parameter, which some
/// certificates use (webpki's *_ABSENT_PARAMS).
const RSA_PKCS1_SHA256_ABSENT_PARAMS: AlgorithmIdentifier =
    AlgorithmIdentifier::from_slice(&[0x06, 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]);
const RSA_PKCS1_SHA384_ABSENT_PARAMS: AlgorithmIdentifier =
    AlgorithmIdentifier::from_slice(&[0x06, 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0c]);
const RSA_PKCS1_SHA512_ABSENT_PARAMS: AlgorithmIdentifier =
    AlgorithmIdentifier::from_slice(&[0x06, 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0d]);

macro_rules! algorithm {
    ($name:ident, $key:expr, $signature:expr, $scheme:expr) => {
        static $name: &dyn SignatureVerificationAlgorithm =
            &Algorithm { public_key: $key, signature: $signature, scheme: $scheme };
    };
}
algorithm!(ECDSA_P256_SHA256, alg_id::ECDSA_P256, alg_id::ECDSA_SHA256, Scheme::Ecdsa(Curve::P256, Digest::Sha256));
algorithm!(ECDSA_P256_SHA384, alg_id::ECDSA_P256, alg_id::ECDSA_SHA384, Scheme::Ecdsa(Curve::P256, Digest::Sha384));
algorithm!(ECDSA_P256_SHA512, alg_id::ECDSA_P256, alg_id::ECDSA_SHA512, Scheme::Ecdsa(Curve::P256, Digest::Sha512));
algorithm!(ECDSA_P384_SHA256, alg_id::ECDSA_P384, alg_id::ECDSA_SHA256, Scheme::Ecdsa(Curve::P384, Digest::Sha256));
algorithm!(ECDSA_P384_SHA384, alg_id::ECDSA_P384, alg_id::ECDSA_SHA384, Scheme::Ecdsa(Curve::P384, Digest::Sha384));
algorithm!(ECDSA_P384_SHA512, alg_id::ECDSA_P384, alg_id::ECDSA_SHA512, Scheme::Ecdsa(Curve::P384, Digest::Sha512));
algorithm!(ED25519, alg_id::ED25519, alg_id::ED25519, Scheme::Ed25519);
algorithm!(RSA_PSS_SHA256, alg_id::RSA_ENCRYPTION, alg_id::RSA_PSS_SHA256, Scheme::RsaPss(Digest::Sha256));
algorithm!(RSA_PSS_SHA384, alg_id::RSA_ENCRYPTION, alg_id::RSA_PSS_SHA384, Scheme::RsaPss(Digest::Sha384));
algorithm!(RSA_PSS_SHA512, alg_id::RSA_ENCRYPTION, alg_id::RSA_PSS_SHA512, Scheme::RsaPss(Digest::Sha512));
algorithm!(RSA_PKCS1_SHA256, alg_id::RSA_ENCRYPTION, alg_id::RSA_PKCS1_SHA256, Scheme::RsaPkcs1(Digest::Sha256));
algorithm!(RSA_PKCS1_SHA384, alg_id::RSA_ENCRYPTION, alg_id::RSA_PKCS1_SHA384, Scheme::RsaPkcs1(Digest::Sha384));
algorithm!(RSA_PKCS1_SHA512, alg_id::RSA_ENCRYPTION, alg_id::RSA_PKCS1_SHA512, Scheme::RsaPkcs1(Digest::Sha512));
algorithm!(RSA_PKCS1_SHA256_ABSENT, alg_id::RSA_ENCRYPTION, RSA_PKCS1_SHA256_ABSENT_PARAMS, Scheme::RsaPkcs1(Digest::Sha256));
algorithm!(RSA_PKCS1_SHA384_ABSENT, alg_id::RSA_ENCRYPTION, RSA_PKCS1_SHA384_ABSENT_PARAMS, Scheme::RsaPkcs1(Digest::Sha384));
algorithm!(RSA_PKCS1_SHA512_ABSENT, alg_id::RSA_ENCRYPTION, RSA_PKCS1_SHA512_ABSENT_PARAMS, Scheme::RsaPkcs1(Digest::Sha512));

pub(crate) static ALGORITHMS: WebPkiSupportedAlgorithms = WebPkiSupportedAlgorithms {
    all: &[
        ECDSA_P256_SHA256, ECDSA_P256_SHA384, ECDSA_P256_SHA512,
        ECDSA_P384_SHA256, ECDSA_P384_SHA384, ECDSA_P384_SHA512,
        ED25519,
        RSA_PSS_SHA256, RSA_PSS_SHA384, RSA_PSS_SHA512,
        RSA_PKCS1_SHA256, RSA_PKCS1_SHA384, RSA_PKCS1_SHA512,
        RSA_PKCS1_SHA256_ABSENT, RSA_PKCS1_SHA384_ABSENT, RSA_PKCS1_SHA512_ABSENT,
    ],
    // TLS 1.2 does not fix the curve by scheme; TLS 1.3 does (rustls checks).
    mapping: &[
        (SignatureScheme::ECDSA_NISTP384_SHA384, &[ECDSA_P384_SHA384, ECDSA_P256_SHA384]),
        (SignatureScheme::ECDSA_NISTP256_SHA256, &[ECDSA_P256_SHA256, ECDSA_P384_SHA256]),
        (SignatureScheme::ED25519, &[ED25519]),
        (SignatureScheme::RSA_PSS_SHA512, &[RSA_PSS_SHA512]),
        (SignatureScheme::RSA_PSS_SHA384, &[RSA_PSS_SHA384]),
        (SignatureScheme::RSA_PSS_SHA256, &[RSA_PSS_SHA256]),
        (SignatureScheme::RSA_PKCS1_SHA512, &[RSA_PKCS1_SHA512]),
        (SignatureScheme::RSA_PKCS1_SHA384, &[RSA_PKCS1_SHA384]),
        (SignatureScheme::RSA_PKCS1_SHA256, &[RSA_PKCS1_SHA256]),
    ],
};
