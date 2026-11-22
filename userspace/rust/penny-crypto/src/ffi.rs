//! The C ABI of userspace/servo/crypto/penny_crypto.ads (SPARK, Ada). Every
//! buffer is borrowed for the call; 1 is success or validity, 0 is not.
use std::ffi::c_int;

unsafe extern "C" {
    pub fn penny_cryptoinit();
    pub fn penny_crypto_tiers() -> u32;
    pub fn penny_sha1(m: *const u8, len: u32, out: *mut u8) -> c_int;
    pub fn penny_sha256(m: *const u8, len: u32, out: *mut u8) -> c_int;
    pub fn penny_sha384(m: *const u8, len: u32, out: *mut u8) -> c_int;
    pub fn penny_sha512(m: *const u8, len: u32, out: *mut u8) -> c_int;
    pub fn penny_hmac_sha256(k: *const u8, klen: u32, m: *const u8, mlen: u32, out: *mut u8) -> c_int;
    pub fn penny_hmac_sha384(k: *const u8, klen: u32, m: *const u8, mlen: u32, out: *mut u8) -> c_int;
    pub fn penny_aes_gcm_seal(key: *const u8, klen: u32, nonce: *const u8, aad: *const u8, alen: u32,
                              buf: *mut u8, len: u32, tag: *mut u8) -> c_int;
    pub fn penny_aes_gcm_open(key: *const u8, klen: u32, nonce: *const u8, aad: *const u8, alen: u32,
                              buf: *mut u8, len: u32, tag: *const u8) -> c_int;
    pub fn penny_aes_gcm_prepare(key: *const u8, key_len: u32) -> *mut core::ffi::c_void;
    pub fn penny_aes_gcm_free(context: *mut core::ffi::c_void);
    pub fn penny_aes_gcm_seal_prepared(context: *const core::ffi::c_void, nonce: *const u8, aad: *const u8,
                                       aad_len: u32, buf: *mut u8, len: u32, tag: *mut u8) -> c_int;
    pub fn penny_aes_gcm_open_prepared(context: *const core::ffi::c_void, nonce: *const u8, aad: *const u8,
                                       aad_len: u32, buf: *mut u8, len: u32, tag: *const u8) -> c_int;
    pub fn penny_chacha20_poly1305_seal(key: *const u8, nonce: *const u8, aad: *const u8, alen: u32,
                                        buf: *mut u8, len: u32, tag: *mut u8) -> c_int;
    pub fn penny_chacha20_poly1305_open(key: *const u8, nonce: *const u8, aad: *const u8, alen: u32,
                                        buf: *mut u8, len: u32, tag: *const u8) -> c_int;
    pub fn penny_x25519_public(sk: *const u8, pk: *mut u8) -> c_int;
    pub fn penny_x25519_shared(sk: *const u8, peer: *const u8, out: *mut u8) -> c_int;
    pub fn penny_p256_public(sk: *const u8, blind: *const u8, pk: *mut u8) -> c_int;
    pub fn penny_p256_shared(sk: *const u8, peer: *const u8, blind: *const u8, out: *mut u8) -> c_int;
    pub fn penny_p384_public(sk: *const u8, blind: *const u8, pk: *mut u8) -> c_int;
    pub fn penny_p384_shared(sk: *const u8, peer: *const u8, blind: *const u8, out: *mut u8) -> c_int;
    pub fn penny_mlkem768_keygen(d: *const u8, z: *const u8, ek: *mut u8, dk: *mut u8) -> c_int;
    pub fn penny_mlkem768_decaps(ct: *const u8, dk: *const u8, out: *mut u8) -> c_int;
    pub fn penny_rsa_pkcs1_verify(h: *const u8, hlen: u32, n: *const u8, nlen: u32, e: u32,
                                  sig: *const u8, slen: u32) -> c_int;
    pub fn penny_rsa_pss_verify(h: *const u8, hlen: u32, n: *const u8, nlen: u32, e: u32,
                                sig: *const u8, slen: u32) -> c_int;
    pub fn penny_ecdsa_p256_verify(h: *const u8, hlen: u32, qx: *const u8, qy: *const u8,
                                   r: *const u8, s: *const u8) -> c_int;
    pub fn penny_ecdsa_p384_verify(h: *const u8, hlen: u32, qx: *const u8, qy: *const u8,
                                   r: *const u8, s: *const u8) -> c_int;
    pub fn penny_ed25519_verify(pk: *const u8, m: *const u8, mlen: u32, sig: *const u8) -> c_int;
}
