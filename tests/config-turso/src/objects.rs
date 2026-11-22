//! Storage adapter for the shared CCL.Objects.Persistence profile.
//! This checks bounded canonical envelopes, NOT CCL schema semantics or authority.
//! The Ada typed boundary validates before publication and again after loading.
use crate::{Commit, Result, Store, name};
use minicbor::{Decoder, Encoder};

pub const MAX_CELLS: usize = 256;
pub const MAX_TEXT: usize = 8192;
pub const MAX_ENCODED: usize = 64 + MAX_CELLS * 19 + MAX_TEXT;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EncodedObject {
    schema: [u8; 32],
    bytes: Vec<u8>,
}

impl EncodedObject {
    /// Bounded structural validation only. Success is never permission to
    /// publish as a CCL value without the expected trusted schema binding.
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        if bytes.len() > MAX_ENCODED {
            return Err("oversized CCL object".into());
        }
        let mut d = Decoder::new(bytes);
        if d.array()? != Some(4) || d.u8()? != 1 {
            return Err("unsupported CCL object envelope".into());
        }
        let schema: [u8; 32] = d.bytes()?.try_into()?;
        if schema == [0; 32] {
            return Err("missing CCL schema identity".into());
        }
        let count = d.array()?.ok_or("indefinite CCL cells")?;
        if !(1..=MAX_CELLS as u64).contains(&count) {
            return Err("invalid CCL cell count".into());
        }
        // Independently re-encode the bounded envelope to require shortest
        // CBOR. No allocation based on unchecked lengths from input.
        let mut e = Encoder::new(Vec::new());
        e.array(4)?.u8(1)?.bytes(&schema)?.array(count)?;
        for _ in 0..count {
            if d.array()? != Some(2) {
                return Err("invalid CCL cell".into());
            }
            e.array(2)?.u64(d.u64()?)?.u64(d.u64()?)?;
        }
        let text = d.bytes()?;
        if text.len() > MAX_TEXT || d.position() != bytes.len() {
            return Err("oversized or trailing CCL data".into());
        }
        e.bytes(text)?;
        let canonical = e.into_writer();
        if canonical != bytes {
            return Err("noncanonical CCL object".into());
        }
        Ok(Self {
            schema,
            bytes: canonical,
        })
    }
    pub fn schema(&self) -> &[u8; 32] {
        &self.schema
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectSnapshot {
    pub revision: i64,
    pub schema_version: i64,
    pub object: EncodedObject,
}

impl Store {
    /// Namespace/context policy and semantic validation belong to Config.
    /// SQL persists canonical typed payloads without inventing another type system.
    pub fn commit_object(
        &self,
        namespace: &str,
        profile: &str,
        expected: i64,
        schema_version: i64,
        object: &EncodedObject,
    ) -> Result<Commit> {
        self.commit_payload(
            namespace,
            profile,
            expected,
            schema_version,
            object.schema(),
            object.bytes(),
        )
    }

    pub fn read_object(
        &self,
        namespace: &str,
        profile: &str,
        expected_schema: &[u8; 32],
    ) -> Result<Option<ObjectSnapshot>> {
        self.ready()?;
        if !name(namespace) || !name(profile) {
            return Err("invalid object address".into());
        }
        let revision = self.head(namespace, profile)?;
        if revision == 0 {
            return Ok(None);
        }
        self.read_object_revision(namespace, profile, revision, expected_schema)
            .map(Some)
    }

    pub fn read_object_revision(
        &self,
        namespace: &str,
        profile: &str,
        revision: i64,
        expected_schema: &[u8; 32],
    ) -> Result<ObjectSnapshot> {
        let (schema_version, schema, bytes) =
            self.read_payload(namespace, profile, revision, MAX_ENCODED)?;
        if schema != expected_schema {
            return Err("CCL schema mismatch".into());
        }
        let object = EncodedObject::parse(&bytes)?;
        if object.schema() != expected_schema {
            return Err("CCL payload/schema mismatch".into());
        }
        Ok(ObjectSnapshot {
            revision,
            schema_version,
            object,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use turso_core::MemoryIO;

    fn object(schema: u8) -> EncodedObject {
        let mut e = Encoder::new(Vec::new());
        e.array(4)
            .unwrap()
            .u8(1)
            .unwrap()
            .bytes(&[schema; 32])
            .unwrap()
            .array(1)
            .unwrap()
            .array(2)
            .unwrap()
            .u64(42)
            .unwrap()
            .u64(0)
            .unwrap()
            .bytes(&[])
            .unwrap();
        EncodedObject::parse(&e.into_writer()).unwrap()
    }

    #[test]
    fn canonical_envelope_rejects_hostile_lengths_and_shapes() {
        let good = object(1);
        for end in 0..good.bytes().len() {
            assert!(EncodedObject::parse(&good.bytes()[..end]).is_err());
        }
        let mut trailing = good.bytes().to_vec();
        trailing.push(0);
        assert!(EncodedObject::parse(&trailing).is_err());
        let mut wide_version = good.bytes().to_vec();
        wide_version.splice(1..2, [0x18, 1]);
        assert!(EncodedObject::parse(&wide_version).is_err());
        for (index, value) in [
            (0, 0x9f),
            (1, 2),
            (2, 0x78),
            (36, 0x9f),
            (37, 0x83),
            (38, 0x20),
        ] {
            let mut bad = good.bytes().to_vec();
            bad[index] = value;
            assert!(EncodedObject::parse(&bad).is_err(), "index {index}");
        }
        for count in [0, 257, u64::MAX] {
            let mut e = Encoder::new(Vec::new());
            e.array(4)
                .unwrap()
                .u8(1)
                .unwrap()
                .bytes(&[1; 32])
                .unwrap()
                .array(count)
                .unwrap();
            assert!(EncodedObject::parse(&e.into_writer()).is_err());
        }
        let mut oversized = good.bytes().to_vec();
        oversized.resize(MAX_ENCODED + 1, 0);
        assert!(EncodedObject::parse(&oversized).is_err());
    }

    #[test]
    fn typed_revision_history_and_schema_lock() -> Result<()> {
        let io = Arc::new(MemoryIO::new());
        let store = Store::open_with_io(io.clone(), "objects.db")?;
        let value = object(1);
        assert_eq!(
            store.read_object("app.settings", "desk", value.schema())?,
            None
        );
        assert_eq!(
            store.commit_object("app.settings", "desk", 0, 1, &value)?,
            Commit::Saved(1)
        );
        assert_eq!(
            store.commit_object("app.settings", "desk", 0, 1, &value)?,
            Commit::Conflict { actual: 1 }
        );
        assert!(
            store
                .commit_object("app.settings", "desk", 1, 1, &object(2))
                .is_err()
        );
        assert!(store.commit("app.settings", "desk", 1, 1, &[]).is_err());
        assert!(store.read("app.settings", "desk").is_err());
        assert!(store.read_object("app.settings", "desk", &[2; 32]).is_err());
        assert_eq!(
            store.commit_object("app.settings", "desk", 1, 1, &value)?,
            Commit::Saved(2)
        );
        assert_eq!(
            store
                .read_object_revision("app.settings", "desk", 1, value.schema())?
                .object,
            value
        );
        assert_eq!(
            store.read_object("app.settings", "other", value.schema())?,
            None
        );
        assert!(
            store
                .commit_object("app.settings", "desk", i64::MAX, 1, &value)
                .is_err()
        );
        store.checkpoint()?;
        store.close()?;
        let reopened = Store::open_with_io(io, "objects.db")?;
        let read = reopened
            .read_object("app.settings", "desk", value.schema())?
            .unwrap();
        assert_eq!(read.revision, 2);
        assert_eq!(read.object, value);
        reopened.close()
    }

    #[test]
    fn damaged_database_payload_never_becomes_a_typed_read() -> Result<()> {
        for damage in [
            "UPDATE revisions SET payload=x'ff'",
            "UPDATE revisions SET payload=zeroblob(14000)",
            "UPDATE revisions SET payload_schema=zeroblob(32)",
            "UPDATE revisions SET schema_version=0",
        ] {
            let store = Store::open_with_io(Arc::new(MemoryIO::new()), "damaged.db")?;
            let value = object(1);
            store.commit_object("app", "desk", 0, 1, &value)?;
            store.connection.execute(damage)?;
            assert!(
                store.read_object("app", "desk", value.schema()).is_err(),
                "{damage}"
            );
            store.close()?;
        }
        Ok(())
    }
}
