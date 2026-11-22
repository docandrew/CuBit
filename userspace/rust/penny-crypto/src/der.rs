//! The DER this provider reads: RSA public keys (RSAPublicKey) and ECDSA
//! signatures (ECDSA-Sig-Value). Strict: definite minimal lengths,
//! positive minimally-encoded integers, nothing trailing.

const SEQUENCE: u8 = 0x30;
const INTEGER: u8 = 0x02;

/// One TLV of `tag` at the start of `input`: (contents, rest).
fn element(input: &[u8], tag: u8) -> Option<(&[u8], &[u8])> {
    let (&found, rest) = input.split_first()?;
    if found != tag {
        return None;
    }
    let (&first, rest) = rest.split_first()?;
    let (len, rest) = match first {
        0..=0x7f => (first as usize, rest),
        0x81..=0x84 => {
            let count = (first & 0x7f) as usize;
            if rest.len() < count || rest[0] == 0 {
                return None;
            }
            let len = rest[..count].iter().fold(0usize, |acc, &b| (acc << 8) | b as usize);
            // Minimal: a long form only for 128 and above.
            if len < 0x80 {
                return None;
            }
            (len, &rest[count..])
        },
        _ => return None,
    };
    (rest.len() >= len).then(|| rest.split_at(len))
}

/// A non-negative INTEGER's magnitude, big-endian, without its sign byte.
fn unsigned(input: &[u8]) -> Option<(&[u8], &[u8])> {
    let (value, rest) = element(input, INTEGER)?;
    match value {
        [] => None,
        [first, ..] if first & 0x80 != 0 => None,
        [0, second, ..] if second & 0x80 == 0 => None,
        [0, magnitude @ ..] if !magnitude.is_empty() => Some((magnitude, rest)),
        _ => Some((value, rest)),
    }
}

/// RSAPublicKey: (modulus, exponent).
pub(crate) fn rsa_public_key(der: &[u8]) -> Option<(&[u8], u32)> {
    let (body, trailing) = element(der, SEQUENCE)?;
    if !trailing.is_empty() {
        return None;
    }
    let (n, rest) = unsigned(body)?;
    let (e, rest) = unsigned(rest)?;
    if !rest.is_empty() || e.len() > 4 {
        return None;
    }
    let exponent = e.iter().fold(0u32, |acc, &b| (acc << 8) | b as u32);
    Some((n, exponent))
}

/// ECDSA-Sig-Value: r and s, each left-padded to `size` bytes.
pub(crate) fn ecdsa_signature(der: &[u8], size: usize) -> Option<(Vec<u8>, Vec<u8>)> {
    let (body, trailing) = element(der, SEQUENCE)?;
    if !trailing.is_empty() {
        return None;
    }
    let (r, rest) = unsigned(body)?;
    let (s, rest) = unsigned(rest)?;
    if !rest.is_empty() || r.len() > size || s.len() > size {
        return None;
    }
    let pad = |v: &[u8]| {
        let mut out = vec![0u8; size - v.len()];
        out.extend_from_slice(v);
        out
    };
    Some((pad(r), pad(s)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rsa_key() {
        // SEQUENCE { INTEGER 0x00c5.., INTEGER 65537 }
        let der = [0x30, 0x09, 0x02, 0x03, 0x00, 0xc5, 0x01, 0x02, 0x02, 0x01, 0x03];
        assert_eq!(rsa_public_key(&der), Some((&[0xc5, 0x01][..], 3)));
        // A negative modulus, a non-minimal integer, trailing bytes.
        assert_eq!(rsa_public_key(&[0x30, 0x06, 0x02, 0x01, 0x85, 0x02, 0x01, 0x03]), None);
        assert_eq!(rsa_public_key(&[0x30, 0x07, 0x02, 0x02, 0x00, 0x05, 0x02, 0x01, 0x03]), None);
        assert_eq!(rsa_public_key(&[0x30, 0x06, 0x02, 0x01, 0x05, 0x02, 0x01, 0x03, 0x00]), None);
    }

    #[test]
    fn signature() {
        let der = [0x30, 0x07, 0x02, 0x01, 0x05, 0x02, 0x02, 0x00, 0x80];
        let (r, s) = ecdsa_signature(&der, 4).unwrap();
        assert_eq!((r, s), (vec![0, 0, 0, 5], vec![0, 0, 0, 0x80]));
        assert!(ecdsa_signature(&der, 0).is_none());
    }
}
