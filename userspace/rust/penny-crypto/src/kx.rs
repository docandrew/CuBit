//! Key exchange over SPARK X25519, P-256, P-384 (blinded scalar
//! multiplication, as SPARKTLS's own key shares) and ML-KEM-768: the groups
//! a browser offers, X25519MLKEM768 first (rustls's default order). Penny
//! is a client: a group starts, sends its share and completes with the
//! server's.
use rustls::crypto::{ActiveKeyExchange, SharedSecret, SupportedKxGroup};
use rustls::ffdhe_groups::FfdheGroup;
use rustls::{Error, NamedGroup, PeerMisbehaved, ProtocolVersion};

use crate::{ffi, random};

const X25519_LEN: usize = 32;
const P256_SCALAR_LEN: usize = 32;
const P256_BLIND_LEN: usize = 40;
const P256_POINT_LEN: usize = 65;
const P384_SCALAR_LEN: usize = 48;
const P384_BLIND_LEN: usize = 56;
const P384_POINT_LEN: usize = 97;
const MLKEM768_EK_LEN: usize = 1184;
const MLKEM768_DK_LEN: usize = 2400;
const MLKEM768_CT_LEN: usize = 1088;
const MLKEM_SEED_LEN: usize = 32;
const MLKEM_SECRET_LEN: usize = 32;

fn invalid_share() -> Error {
    Error::from(PeerMisbehaved::InvalidKeyShare)
}

fn fresh<const N: usize>() -> Result<[u8; N], Error> {
    let mut bytes = [0u8; N];
    random(&mut bytes).map_err(|_| Error::FailedToGetRandomBytes)?;
    Ok(bytes)
}

#[derive(Debug, Clone, Copy)]
enum Classical {
    X25519,
    P256,
    P384,
}

impl Classical {
    fn name(self) -> NamedGroup {
        match self {
            Self::X25519 => NamedGroup::X25519,
            Self::P256 => NamedGroup::secp256r1,
            Self::P384 => NamedGroup::secp384r1,
        }
    }
}

/// An ephemeral key of one classical group.
struct Ephemeral {
    group: Classical,
    secret: Vec<u8>,
    public: Vec<u8>,
}

impl Ephemeral {
    fn new(group: Classical) -> Result<Self, Error> {
        crate::init();
        let (secret, public): (Vec<u8>, Vec<u8>) = match group {
            Classical::X25519 => {
                let sk = fresh::<X25519_LEN>()?;
                let mut pk = [0u8; X25519_LEN];
                // SAFETY: 32-byte scalar and point.
                unsafe { ffi::penny_x25519_public(sk.as_ptr(), pk.as_mut_ptr()) };
                (sk.to_vec(), pk.to_vec())
            },
            Classical::P256 => {
                let (sk, blind) = (fresh::<P256_SCALAR_LEN>()?, fresh::<P256_BLIND_LEN>()?);
                let mut pk = [0u8; P256_POINT_LEN];
                // SAFETY: scalar 32, blind 40, point 65.
                unsafe { ffi::penny_p256_public(sk.as_ptr(), blind.as_ptr(), pk.as_mut_ptr()) };
                (sk.to_vec(), pk.to_vec())
            },
            Classical::P384 => {
                let (sk, blind) = (fresh::<P384_SCALAR_LEN>()?, fresh::<P384_BLIND_LEN>()?);
                let mut pk = [0u8; P384_POINT_LEN];
                // SAFETY: scalar 48, blind 56, point 97.
                unsafe { ffi::penny_p384_public(sk.as_ptr(), blind.as_ptr(), pk.as_mut_ptr()) };
                (sk.to_vec(), pk.to_vec())
            },
        };
        Ok(Self { group, secret, public })
    }

    fn agree(&self, peer: &[u8]) -> Result<Vec<u8>, Error> {
        let ok;
        let mut out;
        match self.group {
            Classical::X25519 => {
                if peer.len() != X25519_LEN {
                    return Err(invalid_share());
                }
                out = vec![0u8; X25519_LEN];
                // SAFETY: 32-byte scalar, peer point and output.
                ok = unsafe { ffi::penny_x25519_shared(self.secret.as_ptr(), peer.as_ptr(), out.as_mut_ptr()) };
            },
            Classical::P256 => {
                if peer.len() != P256_POINT_LEN {
                    return Err(invalid_share());
                }
                let blind = fresh::<P256_BLIND_LEN>()?;
                out = vec![0u8; P256_SCALAR_LEN];
                // SAFETY: scalar 32, point 65, blind 40, x 32.
                ok = unsafe {
                    ffi::penny_p256_shared(self.secret.as_ptr(), peer.as_ptr(), blind.as_ptr(), out.as_mut_ptr())
                };
            },
            Classical::P384 => {
                if peer.len() != P384_POINT_LEN {
                    return Err(invalid_share());
                }
                let blind = fresh::<P384_BLIND_LEN>()?;
                out = vec![0u8; P384_SCALAR_LEN];
                // SAFETY: scalar 48, point 97, blind 56, secret 48.
                ok = unsafe {
                    ffi::penny_p384_shared(self.secret.as_ptr(), peer.as_ptr(), blind.as_ptr(), out.as_mut_ptr())
                };
            },
        }
        if ok == 1 { Ok(out) } else { Err(invalid_share()) }
    }
}

impl Drop for Ephemeral {
    fn drop(&mut self) {
        self.secret.iter_mut().for_each(|b| *b = 0);
    }
}

// Classical groups ---------------------------------------------------------

#[derive(Debug)]
struct Group(Classical);

impl SupportedKxGroup for Group {
    fn start(&self) -> Result<Box<dyn ActiveKeyExchange>, Error> {
        Ok(Box::new(Ephemeral::new(self.0)?))
    }
    fn ffdhe_group(&self) -> Option<FfdheGroup<'static>> {
        None
    }
    fn name(&self) -> NamedGroup {
        self.0.name()
    }
}

impl ActiveKeyExchange for Ephemeral {
    fn complete(self: Box<Self>, peer: &[u8]) -> Result<SharedSecret, Error> {
        Ok(SharedSecret::from(self.agree(peer)?))
    }
    fn pub_key(&self) -> &[u8] {
        &self.public
    }
    fn ffdhe_group(&self) -> Option<FfdheGroup<'static>> {
        None
    }
    fn group(&self) -> NamedGroup {
        self.group.name()
    }
}

// X25519MLKEM768 (draft-ietf-tls-ecdhe-mlkem): ML-KEM first in both shares
// and in the secret. -------------------------------------------------------

#[derive(Debug)]
struct X25519MlKem768;

struct ActiveHybrid {
    classical: Ephemeral,
    dk: Vec<u8>,
    share: Vec<u8>,
}

impl Drop for ActiveHybrid {
    fn drop(&mut self) {
        self.dk.iter_mut().for_each(|b| *b = 0);
    }
}

impl SupportedKxGroup for X25519MlKem768 {
    fn start(&self) -> Result<Box<dyn ActiveKeyExchange>, Error> {
        let classical = Ephemeral::new(Classical::X25519)?;
        let (d, z) = (fresh::<MLKEM_SEED_LEN>()?, fresh::<MLKEM_SEED_LEN>()?);
        let mut ek = vec![0u8; MLKEM768_EK_LEN];
        let mut dk = vec![0u8; MLKEM768_DK_LEN];
        // SAFETY: seeds 32 + 32, EK 1184, DK 2400.
        unsafe { ffi::penny_mlkem768_keygen(d.as_ptr(), z.as_ptr(), ek.as_mut_ptr(), dk.as_mut_ptr()) };
        let share = [ek.as_slice(), &classical.public].concat();
        Ok(Box::new(ActiveHybrid { classical, dk, share }))
    }
    fn ffdhe_group(&self) -> Option<FfdheGroup<'static>> {
        None
    }
    fn name(&self) -> NamedGroup {
        NamedGroup::X25519MLKEM768
    }
    fn usable_for_version(&self, version: ProtocolVersion) -> bool {
        version == ProtocolVersion::TLSv1_3
    }
}

impl ActiveKeyExchange for ActiveHybrid {
    fn complete(self: Box<Self>, peer: &[u8]) -> Result<SharedSecret, Error> {
        if peer.len() != MLKEM768_CT_LEN + X25519_LEN {
            return Err(invalid_share());
        }
        let (ct, classical) = peer.split_at(MLKEM768_CT_LEN);
        let mut pq = [0u8; MLKEM_SECRET_LEN];
        // SAFETY: ciphertext 1088, DK 2400, secret 32.
        unsafe { ffi::penny_mlkem768_decaps(ct.as_ptr(), self.dk.as_ptr(), pq.as_mut_ptr()) };
        let cl = self.classical.agree(classical)?;
        let secret = [pq.as_slice(), &cl].concat();
        pq.iter_mut().for_each(|b| *b = 0);
        Ok(SharedSecret::from(secret))
    }
    fn hybrid_component(&self) -> Option<(NamedGroup, &[u8])> {
        Some((NamedGroup::X25519, &self.classical.public))
    }
    fn complete_hybrid_component(self: Box<Self>, peer: &[u8]) -> Result<SharedSecret, Error> {
        Ok(SharedSecret::from(self.classical.agree(peer)?))
    }
    fn pub_key(&self) -> &[u8] {
        &self.share
    }
    fn ffdhe_group(&self) -> Option<FfdheGroup<'static>> {
        None
    }
    fn group(&self) -> NamedGroup {
        NamedGroup::X25519MLKEM768
    }
}

pub(crate) static ALL_KX_GROUPS: &[&dyn SupportedKxGroup] = &[
    &X25519MlKem768,
    &Group(Classical::X25519),
    &Group(Classical::P256),
    &Group(Classical::P384),
];
