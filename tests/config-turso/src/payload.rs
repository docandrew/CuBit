//! Experimental persistable scalar-profile codec, not a second CCL type system.
//! No tags, handles, constructors, callbacks or automatic schema migrations.
use crate::{MAX_ENTRIES, MAX_VALUE, Result, Setting, name};
use minicbor::{Decoder, Encoder, data::Type};
use sha2::{Digest, Sha256};
use std::{collections::BTreeSet, sync::OnceLock};

// Exact UTF-8 bytes, without a trailing newline. This identifies this one
// experimental representation, NOT a publisher, authority or CCL interface.
pub const SCHEMA: &str = "cubit.config.scalar-profile/1;envelope=[1,sha256(schema),map];keys=ascii-dot-name:1..128;entries=0..256;values=utf8:0..4096|i64|bool;definite;shortest;keys=length-then-bytes";
pub const MAX_PAYLOAD: usize = 64 + MAX_ENTRIES * (128 + MAX_VALUE + 16);

pub fn schema_id() -> &'static [u8; 32] {
    static ID: OnceLock<[u8; 32]> = OnceLock::new();
    ID.get_or_init(|| Sha256::digest(SCHEMA.as_bytes()).into())
}

pub fn encode(entries: &[(String, Setting)]) -> Result<Vec<u8>> {
    if entries.len() > MAX_ENTRIES {
        return Err("too many settings".into());
    }
    let mut keys = BTreeSet::new();
    for (key, value) in entries {
        if !name(key)
            || !keys.insert(key)
            || matches!(value, Setting::Text(v) if v.len() > MAX_VALUE)
        {
            return Err("invalid, duplicate, or oversized setting".into());
        }
    }
    let mut sorted: Vec<_> = entries.iter().collect();
    sorted.sort_unstable_by_key(|(key, _)| (key.len(), key.as_bytes()));
    let mut e = Encoder::new(Vec::new());
    e.array(3)?
        .u8(1)?
        .bytes(schema_id())?
        .map(sorted.len() as u64)?;
    for (key, value) in sorted {
        e.str(key)?;
        match value {
            Setting::Text(v) => {
                e.str(v)?;
            }
            Setting::Integer(v) => {
                e.i64(*v)?;
            }
            Setting::Boolean(v) => {
                e.bool(*v)?;
            }
        }
    }
    Ok(e.into_writer())
}

pub fn decode(bytes: &[u8]) -> Result<Vec<(String, Setting)>> {
    if bytes.len() > MAX_PAYLOAD {
        return Err("oversized stored profile".into());
    }
    let mut d = Decoder::new(bytes);
    if d.array()? != Some(3) || d.u8()? != 1 || d.bytes()? != schema_id() {
        return Err("unsupported profile envelope/schema".into());
    }
    let count = d.map()?.ok_or("indefinite profile map")?;
    if count > MAX_ENTRIES as u64 {
        return Err("too many stored settings".into());
    }
    let mut entries = Vec::with_capacity(count as usize);
    let mut previous: Option<&str> = None;
    for _ in 0..count {
        let key = d.str()?; // Borrow first; check bounds before allocating.
        if !name(key)
            || previous.is_some_and(|p| (p.len(), p.as_bytes()) >= (key.len(), key.as_bytes()))
        {
            return Err("invalid, duplicate, or out-of-order key".into());
        }
        previous = Some(key);
        let value = match d.datatype()? {
            Type::String => {
                let value = d.str()?;
                if value.len() > MAX_VALUE {
                    return Err("oversized stored text".into());
                }
                Setting::Text(value.to_owned())
            }
            Type::Bool => Setting::Boolean(d.bool()?),
            Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Int => Setting::Integer(d.i64()?),
            _ => return Err("unsupported stored value type".into()),
        };
        entries.push((key.to_owned(), value));
    }
    if d.position() != bytes.len() {
        return Err("trailing profile data".into());
    }
    // Also reject non-shortest integer/length encodings. Resource consumption
    // is already bounded; this intentionally favors one auditable encoding over
    // a more complex fast-path validator in this hosted experiment.
    if encode(&entries)? != bytes {
        return Err("non-deterministic profile encoding".into());
    }
    entries.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn envelope(map: &[u8]) -> Vec<u8> {
        let mut result = vec![0x83, 1, 0x58, 32];
        result.extend_from_slice(schema_id());
        result.extend_from_slice(map);
        result
    }

    #[test]
    fn golden_scalars_order_and_extremes() -> Result<()> {
        let values = vec![
            ("aa".into(), Setting::Text("Cubie".into())),
            ("b".into(), Setting::Integer(-25)),
            ("z".into(), Setting::Boolean(true)),
        ];
        let expected = envelope(&[
            0xa3, 0x61, b'b', 0x38, 24, 0x61, b'z', 0xf5, 0x62, b'a', b'a', 0x65, b'C', b'u', b'b',
            b'i', b'e',
        ]);
        let hex: String = expected.iter().map(|b| format!("{b:02x}")).collect();
        assert_eq!(hex, include_str!("../fixtures/scalar-profile.hex").trim());
        assert_eq!(encode(&values)?, expected);
        assert_eq!(decode(&expected)?, values);
        let mut reversed = values.clone();
        reversed.reverse();
        assert_eq!(encode(&reversed)?, expected);
        for value in [
            i64::MIN,
            -65537,
            -256,
            -25,
            -24,
            -1,
            0,
            23,
            24,
            255,
            256,
            65535,
            65536,
            i64::MAX,
        ] {
            let values = vec![("n".into(), Setting::Integer(value))];
            assert_eq!(decode(&encode(&values)?)?, values);
        }
        assert_eq!(decode(&encode(&[])?)?, vec![]);
        Ok(())
    }

    #[test]
    fn malformed_payloads_rejected() -> Result<()> {
        let valid = encode(&[("a".into(), Setting::Text("Cubie 🌟".into()))])?;
        for end in 0..valid.len() {
            assert!(decode(&valid[..end]).is_err(), "truncation {end}");
        }
        let mut extra = valid.clone();
        extra.push(0);
        assert!(decode(&extra).is_err());
        let mut wrong_schema = valid.clone();
        wrong_schema[4] ^= 1;
        assert!(decode(&wrong_schema).is_err());
        let mut wrong_version = valid.clone();
        wrong_version[1] = 2;
        assert!(decode(&wrong_version).is_err());
        assert!(decode(&vec![0; MAX_PAYLOAD + 1]).is_err());
        for map in [
            vec![0xbf, 0xff],                         // Indefinite map.
            vec![0xb9, 1, 1],                         // 257 entries, before allocation.
            vec![0xb8, 0],                            // Non-shortest length.
            vec![0xa2, 0x61, b'a', 0, 0x61, b'a', 1], // Duplicate.
            vec![0xa2, 0x61, b'b', 0, 0x61, b'a', 1], // Wrong order.
            vec![0xa1, 0x60, 0],                      // Empty key.
            vec![0xa1, 0x61, b'.', 0],                // Invalid name.
            vec![0xa1, 0x61, 0xff, 0],                // Invalid UTF-8 key.
            vec![0xa1, 0x61, b'a', 0x18, 0],          // Non-shortest integer.
            vec![0xa1, 0x61, b'a', 0x61, 0xff],       // Invalid UTF-8 value.
            vec![
                0xa1, 0x61, b'a', 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            ],
            vec![
                0xa1, 0x61, b'a', 0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            ],
        ] {
            assert!(decode(&envelope(&map)).is_err(), "map {map:x?}");
        }
        for value in [
            vec![0xf6],
            vec![0x80],
            vec![0xa0],
            vec![0x40],
            vec![0xc0, 0],
            vec![0xf9, 0, 0],
            vec![0x7f, 0xff],
            vec![0xff],
        ] {
            let mut map = vec![0xa1, 0x61, b'a'];
            map.extend(value);
            assert!(decode(&envelope(&map)).is_err());
        }
        // Mutations are not all invalid (some change valid data). Anything
        // accepted must still have exactly one canonical re-encoding.
        for index in 0..valid.len() {
            for byte in 0..=255 {
                let mut changed = valid.clone();
                changed[index] = byte;
                if let Ok(entries) = decode(&changed) {
                    assert_eq!(encode(&entries)?, changed);
                }
            }
        }
        Ok(())
    }

    #[test]
    fn size_limits_are_checked_on_both_paths() -> Result<()> {
        let values = vec![("a".repeat(128), Setting::Text("x".repeat(MAX_VALUE)))];
        assert_eq!(decode(&encode(&values)?)?, values);
        for (key, value) in [
            ("a".repeat(129), String::new()),
            ("a".into(), "x".repeat(MAX_VALUE + 1)),
        ] {
            assert!(encode(&[(key.clone(), Setting::Text(value.clone()))]).is_err());
            let mut e = Encoder::new(Vec::new());
            e.array(3)?
                .u8(1)?
                .bytes(schema_id())?
                .map(1)?
                .str(&key)?
                .str(&value)?;
            assert!(decode(&e.into_writer()).is_err());
        }
        Ok(())
    }
}
