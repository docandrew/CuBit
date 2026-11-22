//! rustls's HMAC (for HKDF and the TLS 1.2 PRF) over SPARK HMAC-SHA-256/384.
use rustls::crypto::hmac::{Hmac, Key, Tag};

pub(crate) struct Sha256Hmac;
pub(crate) struct Sha384Hmac;

impl Hmac for Sha256Hmac {
    fn with_key(&self, key: &[u8]) -> Box<dyn Key> {
        Box::new(HmacKey { key: key.to_vec(), len: 32 })
    }
    fn hash_output_len(&self) -> usize {
        32
    }
}

impl Hmac for Sha384Hmac {
    fn with_key(&self, key: &[u8]) -> Box<dyn Key> {
        Box::new(HmacKey { key: key.to_vec(), len: 48 })
    }
    fn hash_output_len(&self) -> usize {
        48
    }
}

struct HmacKey {
    key: Vec<u8>,
    len: usize,
}

impl Key for HmacKey {
    fn sign_concat(&self, first: &[u8], middle: &[&[u8]], last: &[u8]) -> Tag {
        let mut message = Vec::with_capacity(first.len() + last.len() + middle.iter().map(|m| m.len()).sum::<usize>());
        message.extend_from_slice(first);
        for part in middle {
            message.extend_from_slice(part);
        }
        message.extend_from_slice(last);
        match self.len {
            48 => Tag::new(&crate::hmac_sha384(&self.key, &message).expect("HMAC input within MAX_LENGTH")),
            _ => Tag::new(&crate::hmac_sha256(&self.key, &message).expect("HMAC input within MAX_LENGTH")),
        }
    }
    fn tag_len(&self) -> usize {
        self.len
    }
}
