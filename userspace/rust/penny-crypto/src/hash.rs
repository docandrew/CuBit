//! rustls's transcript hashes over the SPARK one-shot digests: a context
//! keeps the bytes (a handshake transcript is a few kilobytes) and hashes
//! them on demand.
use rustls::crypto::hash::{Context, Hash, HashAlgorithm, Output};

pub(crate) struct Sha256;
pub(crate) struct Sha384;

fn digest(algorithm: HashAlgorithm, data: &[u8]) -> Output {
    match algorithm {
        HashAlgorithm::SHA384 => Output::new(&crate::sha384(data).expect("transcript within MAX_LENGTH")),
        _ => Output::new(&crate::sha256(data).expect("transcript within MAX_LENGTH")),
    }
}

macro_rules! hash {
    ($type:ident, $algorithm:expr, $len:expr) => {
        impl Hash for $type {
            fn start(&self) -> Box<dyn Context> {
                Box::new(Transcript { algorithm: $algorithm, bytes: Vec::new() })
            }
            fn hash(&self, data: &[u8]) -> Output {
                digest($algorithm, data)
            }
            fn algorithm(&self) -> HashAlgorithm {
                $algorithm
            }
            fn output_len(&self) -> usize {
                $len
            }
        }
    };
}
hash!(Sha256, HashAlgorithm::SHA256, 32);
hash!(Sha384, HashAlgorithm::SHA384, 48);

struct Transcript {
    algorithm: HashAlgorithm,
    bytes: Vec<u8>,
}

impl Context for Transcript {
    fn fork_finish(&self) -> Output {
        digest(self.algorithm, &self.bytes)
    }
    fn fork(&self) -> Box<dyn Context> {
        Box::new(Self { algorithm: self.algorithm, bytes: self.bytes.clone() })
    }
    fn finish(self: Box<Self>) -> Output {
        digest(self.algorithm, &self.bytes)
    }
    fn update(&mut self, data: &[u8]) {
        self.bytes.extend_from_slice(data);
    }
}
