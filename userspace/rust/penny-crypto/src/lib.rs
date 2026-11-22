//! Penny's cryptography (WEB-004): a rustls `CryptoProvider` and the digests
//! Servo needs (SRI, Web Crypto, certificate fingerprints), over
//! SPARKTLSCrypto, SPARKNaCl and SPARKMLKEM through Ada exports
//! (userspace/servo/crypto). No primitive is implemented here: this crate
//! frames TLS records, parses DER keys and signatures, and calls the SPARK
//! code. Randomness comes from the OS (getrandom: the CuBit libc's).
//!
//! Not covered (refused with an ordinary error): ECDSA P-521 and ML-DSA
//! certificates (SPARKTLSCrypto has neither yet), client certificates.

use std::sync::Once;

mod aead;
mod der;
mod ffi;
mod hash;
mod hmac;
mod kx;
mod verify;

use rustls::crypto::{CryptoProvider, GetRandomFailed, KeyProvider, SecureRandom};
use rustls::pki_types::PrivateKeyDer;

static INIT: Once = Once::new();

/// The SPARK library's elaboration, once, before any call into it.
pub(crate) fn init() {
    // SAFETY: the library's init takes no arguments and runs once.
    INIT.call_once(|| {
        unsafe { ffi::penny_cryptoinit() };
        // One line per process, so every run says which code did its crypto.
        eprintln!("penny-crypto: {}", tiers_after_init());
    });
}

/// The hardware paths SPARKTLSCrypto chose from CPUID (penny_crypto.ads
/// Tier_*): each is used instead of the proved portable code when set.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Tiers {
    pub aes_ni: bool,
    pub pclmulqdq: bool,
    pub bmi2_adx: bool,
    pub avx2: bool,
    pub avx512_aes_gcm: bool,
    pub avx512_chacha20: bool,
    pub avx512_poly1305: bool,
}

// The bits of penny_crypto_tiers, as penny_crypto.ads names them.
const TIER_AES_NI: u32 = 1 << 0;
const TIER_PCLMULQDQ: u32 = 1 << 1;
const TIER_BMI2_ADX: u32 = 1 << 2;
const TIER_AVX2: u32 = 1 << 3;
const TIER_AVX512_AES_GCM: u32 = 1 << 4;
const TIER_AVX512_CHACHA20: u32 = 1 << 5;
const TIER_AVX512_POLY1305: u32 = 1 << 6;

fn tiers_after_init() -> Tiers {
    // SAFETY: reads seven elaborated booleans; no arguments.
    let bits = unsafe { ffi::penny_crypto_tiers() };
    Tiers {
        aes_ni: bits & TIER_AES_NI != 0,
        pclmulqdq: bits & TIER_PCLMULQDQ != 0,
        bmi2_adx: bits & TIER_BMI2_ADX != 0,
        avx2: bits & TIER_AVX2 != 0,
        avx512_aes_gcm: bits & TIER_AVX512_AES_GCM != 0,
        avx512_chacha20: bits & TIER_AVX512_CHACHA20 != 0,
        avx512_poly1305: bits & TIER_AVX512_POLY1305 != 0,
    }
}

/// The tiers in use, after running the library's elaboration if needed.
pub fn tiers() -> Tiers {
    init();
    tiers_after_init()
}

impl std::fmt::Display for Tiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let yn = |b: bool| if b { "yes" } else { "no" };
        write!(
            f,
            "tiers aes-ni={} pclmulqdq={} bmi2-adx={} avx2={} avx512-aes-gcm={} avx512-chacha20={} avx512-poly1305={}",
            yn(self.aes_ni),
            yn(self.pclmulqdq),
            yn(self.bmi2_adx),
            yn(self.avx2),
            yn(self.avx512_aes_gcm),
            yn(self.avx512_chacha20),
            yn(self.avx512_poly1305)
        )
    }
}

#[cfg(all(test, target_arch = "x86_64"))]
mod tier_tests {
    /// Every accelerated path the CPU supports is the one in use: a tier
    /// left off (elaboration not run, a missing compile switch, a
    /// portable-only build) fails here.
    #[test]
    fn tiers_follow_the_cpu() {
        let t = super::tiers();
        assert_eq!(t.aes_ni, std::arch::is_x86_feature_detected!("aes"), "{t}");
        assert_eq!(t.pclmulqdq, std::arch::is_x86_feature_detected!("pclmulqdq"), "{t}");
        assert_eq!(
            t.bmi2_adx,
            std::arch::is_x86_feature_detected!("bmi2") && std::arch::is_x86_feature_detected!("adx"),
            "{t}"
        );
        assert_eq!(t.avx2, std::arch::is_x86_feature_detected!("avx2"), "{t}");
    }
}

/// The largest input one call takes (penny_crypto.ads Max_Length).
pub const MAX_LENGTH: usize = 1 << 30;

pub(crate) fn len32(data: &[u8]) -> Option<u32> {
    (data.len() <= MAX_LENGTH).then_some(data.len() as u32)
}

macro_rules! digest {
    ($name:ident, $ffi:ident, $len:expr, $doc:literal) => {
        #[doc = $doc]
        /// `None` beyond [`MAX_LENGTH`].
        pub fn $name(data: &[u8]) -> Option<[u8; $len]> {
            init();
            let len = len32(data)?;
            let mut out = [0u8; $len];
            // SAFETY: data is len bytes, out the digest's size.
            (unsafe { ffi::$ffi(data.as_ptr(), len, out.as_mut_ptr()) } == 1).then_some(out)
        }
    };
}
digest!(sha1, penny_sha1, 20, "SHA-1 (Web Crypto's digest only).");
digest!(sha256, penny_sha256, 32, "SHA-256.");
digest!(sha384, penny_sha384, 48, "SHA-384.");
digest!(sha512, penny_sha512, 64, "SHA-512.");

macro_rules! hmac {
    ($name:ident, $ffi:ident, $len:expr, $doc:literal) => {
        #[doc = $doc]
        /// `None` beyond [`MAX_LENGTH`].
        pub fn $name(key: &[u8], data: &[u8]) -> Option<[u8; $len]> {
            init();
            let (key_len, len) = (len32(key)?, len32(data)?);
            let mut out = [0u8; $len];
            // SAFETY: key and data are their lengths, out the tag's size.
            (unsafe { ffi::$ffi(key.as_ptr(), key_len, data.as_ptr(), len, out.as_mut_ptr()) } == 1)
                .then_some(out)
        }
    };
}
hmac!(hmac_sha256, penny_hmac_sha256, 32, "HMAC-SHA-256.");
hmac!(hmac_sha384, penny_hmac_sha384, 48, "HMAC-SHA-384.");

/// Random bytes from the OS.
pub(crate) fn random(out: &mut [u8]) -> Result<(), GetRandomFailed> {
    getrandom::fill(out).map_err(|_| GetRandomFailed)
}

/// Penny's rustls provider: TLS 1.3 and 1.2 with AES-128/256-GCM and
/// ChaCha20-Poly1305; X25519MLKEM768, X25519, P-256 and P-384; RSA
/// (PKCS#1 v1.5 and PSS), ECDSA P-256/P-384 and Ed25519 certificates.
pub fn provider() -> CryptoProvider {
    init();
    CryptoProvider {
        cipher_suites: aead::ALL_CIPHER_SUITES.to_vec(),
        kx_groups: kx::ALL_KX_GROUPS.to_vec(),
        signature_verification_algorithms: verify::ALGORITHMS,
        secure_random: &Provider,
        key_provider: &Provider,
    }
}

#[derive(Debug)]
struct Provider;

impl SecureRandom for Provider {
    fn fill(&self, bytes: &mut [u8]) -> Result<(), GetRandomFailed> {
        random(bytes)
    }
}

impl KeyProvider for Provider {
    fn load_private_key(
        &self,
        _key: PrivateKeyDer<'static>,
    ) -> Result<std::sync::Arc<dyn rustls::sign::SigningKey>, rustls::Error> {
        // A browser without client certificates (yet).
        Err(rustls::Error::General("Penny has no client certificate signing".into()))
    }
}
