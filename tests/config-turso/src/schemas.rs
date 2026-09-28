//! Portable CCL schema storage. This is not a new authority/type checker:
//! Ada validates semantic/layout/persistability rules before create and after
//! recovery. The database boundary enforces bounded canonical framing and
//! immutable, namespace/context-scoped declarations. Names/keys grant nothing.
use crate::{Commit, Result, Store, name, objects::EncodedObject, text};
use minicbor::{Decoder, Encoder};
use turso_core::Value;

pub const MAX_DECLARATIONS: u64 = 32;
pub const MAX_PARTS: u64 = 16;
pub const MAX_NAME: usize = 32;
pub const MAX_ENCODED: usize = 64 + 32 * (64 + 16 * 48);
const LAST_TYPE: u32 = 6 + MAX_DECLARATIONS as u32;

#[repr(u32)]
enum Form {
    Product = 1,
    Sum = 2,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EncodedSchema {
    key: [u8; 32],
    bytes: Vec<u8>,
}
impl EncodedSchema {
    /// Structural validation only. Do not approve a binding without the Ada
    /// importer and policy check. A claimed schema key is not a signature.
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        if bytes.len() > MAX_ENCODED {
            return Err("oversized CCL schema".into());
        }
        let mut d = Decoder::new(bytes);
        if d.array()? != Some(4) || d.u8()? != 1 {
            return Err("unsupported schema envelope".into());
        }
        let key: [u8; 32] = d.bytes()?.try_into()?;
        let root = d.u32()?;
        if key == [0; 32] || !(1..=LAST_TYPE).contains(&root) {
            return Err("missing schema identity/root".into());
        }
        let count = d.array()?.ok_or("indefinite declarations")?;
        if count > MAX_DECLARATIONS {
            return Err("too many declarations".into());
        }
        let mut e = Encoder::new(Vec::new());
        e.array(4)?.u8(1)?.bytes(&key)?.u32(root)?.array(count)?;
        for _ in 0..count {
            if d.array()? != Some(3) {
                return Err("invalid definition".into());
            }
            let identifier = d.bytes()?;
            if identifier.is_empty() || identifier.len() > MAX_NAME {
                return Err("invalid definition name size".into());
            }
            let form = d.u32()?;
            if form != Form::Product as u32 && form != Form::Sum as u32 {
                return Err("invalid definition form".into());
            }
            let parts = d.array()?.ok_or("indefinite parts")?;
            if parts > MAX_PARTS {
                return Err("too many parts".into());
            }
            e.array(3)?.bytes(identifier)?.u32(form)?.array(parts)?;
            for _ in 0..parts {
                if d.array()? != Some(2) {
                    return Err("invalid component".into());
                }
                let identifier = d.bytes()?;
                if identifier.is_empty() || identifier.len() > MAX_NAME {
                    return Err("invalid component name size".into());
                }
                let payload = d.u32()?;
                if !(1..=LAST_TYPE).contains(&payload) {
                    return Err("invalid component reference".into());
                }
                e.array(2)?.bytes(identifier)?.u32(payload)?;
            }
        }
        if d.position() != bytes.len() {
            return Err("trailing schema bytes".into());
        }
        let canonical = e.into_writer();
        if canonical != bytes {
            return Err("noncanonical schema".into());
        }
        Ok(Self {
            key,
            bytes: canonical,
        })
    }
    pub fn key(&self) -> &[u8; 32] {
        &self.key
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Creation {
    Created,
    AlreadyExists,
    DefinitionConflict,
    ManagementConflict,
}

#[repr(i64)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Management {
    ApplicationState = 1,
    DeclarationManaged = 2,
}

#[derive(Debug)]
pub struct ManagedWriteDenied;
impl std::fmt::Display for ManagedWriteDenied {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("declaration-managed collection requires activation")
    }
}
impl std::error::Error for ManagedWriteDenied {}

impl Store {
    /// Caller has approved namespace/context AND the full CCL definition.
    /// Atomic and create-only. Does not fabricate a default value/revision.
    /// Never adopt an existing unregistered value or replace its definition.
    pub fn create_object(
        &self,
        namespace: &str,
        profile: &str,
        schema: &EncodedSchema,
    ) -> Result<Creation> {
        self.register_object(namespace, profile, schema, Management::ApplicationState)
    }

    /// Trusted installation/provisioning only. Not exposed by the client Create
    /// IPC or the ordinary worker FFI. Registration is not value activation.
    pub fn register_object(
        &self,
        namespace: &str,
        profile: &str,
        schema: &EncodedSchema,
        management: Management,
    ) -> Result<Creation> {
        self.ready()?;
        if !name(namespace) || !name(profile) {
            return Err("invalid object address".into());
        }
        self.transaction_boundary("BEGIN IMMEDIATE")?;
        let result = (|| {
            if let Some(existing) = self.read_definition(namespace, profile)? {
                if self.management(namespace, profile)? != Some(management) {
                    return Ok(Creation::ManagementConflict);
                }
                return Ok(if existing == *schema {
                    Creation::AlreadyExists
                } else {
                    Creation::DefinitionConflict
                });
            }
            self.query(
                "INSERT INTO object_types VALUES(?,?,?,?,?)",
                vec![
                    text(namespace),
                    text(profile),
                    Value::from_slice(schema.key())?,
                    Value::from_slice(schema.bytes())?,
                    crate::integer(management as i64),
                ],
            )?;
            Ok(Creation::Created)
        })();
        match result {
            Ok(Creation::Created) => {
                self.transaction_boundary("COMMIT")?;
                Ok(Creation::Created)
            }
            other => {
                self.transaction_boundary("ROLLBACK")?;
                other
            }
        }
    }

    /// Classification is immutable metadata, not caller-selected authority.
    /// Missing/malformed class never defaults to application state.
    pub fn management(&self, namespace: &str, profile: &str) -> Result<Option<Management>> {
        self.ready()?;
        if !name(namespace) || !name(profile) {
            return Err("invalid object address".into());
        }
        let rows = self.query(
            "SELECT management FROM object_types WHERE namespace=? AND profile=? LIMIT 2",
            vec![text(namespace), text(profile)],
        )?;
        if rows.is_empty() {
            return Ok(None);
        }
        if rows.len() != 1 || rows[0].len() != 1 {
            return Err("ambiguous management classification".into());
        }
        match crate::as_integer(&rows[0][0])? {
            1 => Ok(Some(Management::ApplicationState)),
            2 => Ok(Some(Management::DeclarationManaged)),
            _ => Err("invalid management classification".into()),
        }
    }

    /// Returns owned metadata, not a trusted/live binding. The Ada decoder
    /// must validate it before publishing it into Config's runtime catalog.
    pub fn read_definition(&self, namespace: &str, profile: &str) -> Result<Option<EncodedSchema>> {
        self.ready()?;
        if !name(namespace) || !name(profile) {
            return Err("invalid object address".into());
        }
        let mut rows = self.query(
            "SELECT CASE WHEN typeof(schema_key)='blob' AND length(schema_key)=32
                    THEN schema_key ELSE NULL END,
             CASE WHEN typeof(declaration)='blob' AND length(declaration)<=?
                    THEN declaration ELSE NULL END
             FROM object_types WHERE namespace=? AND profile=? LIMIT 2",
            vec![
                crate::integer(MAX_ENCODED as i64),
                text(namespace),
                text(profile),
            ],
        )?;
        if rows.is_empty() {
            if self.head(namespace, profile)? != 0 {
                return Err("missing type definition for existing revisions".into());
            }
            return Ok(None);
        }
        if rows.len() != 1 {
            return Err("ambiguous type definition".into());
        }
        let [key, declaration]: [Value; 2] = rows
            .pop()
            .unwrap()
            .try_into()
            .map_err(|_| "invalid definition row")?;
        let (Value::Blob(key), Value::Blob(declaration)) = (key, declaration) else {
            return Err("malformed or oversized type definition".into());
        };
        let schema = EncodedSchema::parse(&declaration)?;
        if key.as_slice() != schema.key() {
            return Err("stored schema key mismatch".into());
        }
        if self.management(namespace, profile)?.is_none() {
            return Err("missing management classification".into());
        }
        Ok(Some(schema))
    }

    /// Declared-object path: immutable metadata must exist and match the value.
    /// Semantic value validation remains the Ada worker's responsibility.
    pub fn commit_declared_object(
        &self,
        namespace: &str,
        profile: &str,
        expected: i64,
        object: &EncodedObject,
    ) -> Result<Commit> {
        let schema = self
            .read_definition(namespace, profile)?
            .ok_or("object not created")?;
        if schema.key() != object.schema() {
            return Err("object type mismatch".into());
        }
        // One Store owner, and no API can delete/change a definition between
        // this read and the revision transaction. No cross-thread TOCTOU claim.
        self.commit_object(namespace, profile, expected, 1, object)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    };
    use turso_core::MemoryIO;

    fn schema(key: u8, root: u32) -> EncodedSchema {
        let mut e = Encoder::new(Vec::new());
        e.array(4)
            .unwrap()
            .u8(1)
            .unwrap()
            .bytes(&[key; 32])
            .unwrap()
            .u32(root)
            .unwrap()
            .array(0)
            .unwrap();
        EncodedSchema::parse(&e.into_writer()).unwrap()
    }
    fn object(key: u8) -> EncodedObject {
        let mut e = Encoder::new(Vec::new());
        e.array(4)
            .unwrap()
            .u8(1)
            .unwrap()
            .bytes(&[key; 32])
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
    fn path() -> String {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        format!(
            "schema-store-{}.sqlite",
            NEXT.fetch_add(1, Ordering::Relaxed)
        )
    }

    #[test]
    fn managed_registration_survives_disk_reopen_without_downgrade() -> Result<()> {
        let directory =
            std::env::temp_dir().join(format!("cubit-managed-{}-{}", std::process::id(), path()));
        std::fs::create_dir(&directory)?;
        let file = directory.join("config.sqlite");
        let s = schema(1, 1);
        let store = Store::open(&file)?;
        assert_eq!(
            store.register_object("org.managed", "machine", &s, Management::DeclarationManaged)?,
            Creation::Created
        );
        assert_eq!(
            store.register_object("org.state", "machine", &s, Management::ApplicationState)?,
            Creation::Created
        );
        store.close()?;
        let reopened = Store::open(&file)?;
        assert_eq!(
            reopened.management("org.managed", "machine")?,
            Some(Management::DeclarationManaged)
        );
        assert_eq!(
            reopened.read_definition("org.managed", "machine")?,
            Some(s.clone())
        );
        assert_eq!(
            reopened.create_object("org.managed", "machine", &s)?,
            Creation::ManagementConflict
        );
        assert_eq!(
            reopened.register_object("org.state", "machine", &s, Management::DeclarationManaged)?,
            Creation::ManagementConflict
        );
        for result in [
            reopened.commit_declared_object("org.managed", "machine", 0, &object(1)),
            reopened.commit_object("org.managed", "machine", 0, 1, &object(1)),
        ] {
            assert!(result.unwrap_err().is::<ManagedWriteDenied>());
        }
        assert_eq!(reopened.head("org.managed", "machine")?, 0);
        assert_eq!(
            reopened.commit_declared_object("org.state", "machine", 0, &object(1))?,
            Commit::Saved(1)
        );
        reopened.close()?;
        // Independent SQLite reader verifies actual disk format and unchanged
        // managed history, not just the Rust API's view of its own state.
        let checked = std::process::Command::new("python3").arg("-c").arg(
            "import sqlite3,sys; d=sqlite3.connect('file:'+sys.argv[1]+'?mode=ro',uri=True); assert d.execute('pragma integrity_check').fetchone()==('ok',); assert d.execute('select version from config_format').fetchall()==[(4,)]; assert d.execute('select namespace,management from object_types order by namespace').fetchall()==[('org.managed',2),('org.state',1)]; assert d.execute('select namespace,revision from collections').fetchall()==[('org.state',1)]"
        ).arg(&file).status()?;
        assert!(checked.success());
        std::fs::remove_dir_all(directory)?;
        Ok(())
    }

    #[test]
    fn malformed_management_is_not_interpreted_as_application_state() -> Result<()> {
        for bad in ["NULL", "0", "3", "'wrong'", "x'01'", "1.5"] {
            let store = Store::open_with_io(Arc::new(MemoryIO::new()), &path())?;
            let s = schema(1, 1);
            store.create_object("org.demo", "machine", &s)?;
            // Model externally corrupted metadata without SQL CHECK enforcing
            // the fixture writer. Readers must validate, not trust constraints.
            store.connection.execute(
                "CREATE TABLE damaged AS SELECT * FROM object_types;
                DROP TABLE object_types; ALTER TABLE damaged RENAME TO object_types;",
            )?;
            store
                .connection
                .execute(format!("UPDATE object_types SET management={bad}"))?;
            assert!(store.management("org.demo", "machine").is_err());
            assert!(store.read_definition("org.demo", "machine").is_err());
            assert!(store.create_object("org.demo", "machine", &s).is_err());
            assert!(
                store
                    .commit_object("org.demo", "machine", 0, 1, &object(1))
                    .is_err()
            );
            assert_eq!(store.head("org.demo", "machine")?, 0);
            store.close()?;
        }
        Ok(())
    }

    #[test]
    fn immutable_creation_value_and_reopen() -> Result<()> {
        let io = Arc::new(MemoryIO::new());
        let path = path();
        let s = schema(1, 1);
        let store = Store::open_with_io(io.clone(), &path)?;
        assert!(
            store
                .commit_declared_object("org.demo", "machine", 0, &object(1))
                .is_err()
        );
        assert_eq!(
            store.create_object("org.demo", "machine", &s)?,
            Creation::Created
        );
        assert_eq!(store.read_object("org.demo", "machine", s.key())?, None);
        assert_eq!(
            store.create_object("org.demo", "machine", &s)?,
            Creation::AlreadyExists
        );
        assert_eq!(
            store.create_object("org.demo", "machine", &schema(1, 2))?,
            Creation::DefinitionConflict
        );
        assert_eq!(
            store.create_object("org.demo", "machine", &schema(2, 1))?,
            Creation::DefinitionConflict
        );
        assert!(
            store
                .commit_declared_object("org.demo", "machine", 0, &object(2))
                .is_err()
        );
        assert_eq!(
            store.commit_declared_object("org.demo", "machine", 0, &object(1))?,
            Commit::Saved(1)
        );
        assert_eq!(
            store.commit_declared_object("org.demo", "machine", 0, &object(1))?,
            Commit::Conflict { actual: 1 }
        );
        store.close()?;
        let reopened = Store::open_with_io(io, &path)?;
        assert_eq!(
            reopened.read_definition("org.demo", "machine")?,
            Some(s.clone())
        );
        assert_eq!(reopened.read_definition("org.demo", "test")?, None);
        assert_eq!(
            reopened
                .read_object("org.demo", "machine", s.key())?
                .unwrap()
                .revision,
            1
        );
        assert_eq!(
            reopened.create_object("org.demo", "machine", &s)?,
            Creation::AlreadyExists
        );
        reopened.close()
    }

    #[test]
    fn creation_survives_without_a_value_and_is_scoped() -> Result<()> {
        let io = Arc::new(MemoryIO::new());
        let path = path();
        let store = Store::open_with_io(io.clone(), &path)?;
        let a = schema(1, 1);
        let b = schema(2, 2);
        assert_eq!(
            store.create_object("org.a", "machine", &a)?,
            Creation::Created
        );
        assert_eq!(
            store.create_object("org.b", "machine", &b)?,
            Creation::Created
        );
        assert_eq!(store.create_object("org.a", "test", &b)?, Creation::Created);
        store.close()?;
        let reopened = Store::open_with_io(io, &path)?;
        assert_eq!(reopened.read_definition("org.a", "machine")?, Some(a));
        assert_eq!(reopened.read_definition("org.a", "test")?, Some(b.clone()));
        assert_eq!(reopened.read_definition("org.b", "machine")?, Some(b));
        assert_eq!(
            reopened.query("SELECT count(*) FROM revisions", vec![])?,
            vec![vec![crate::integer(0)]]
        );
        reopened.close()
    }

    #[test]
    fn malformed_metadata_and_unregistered_values_are_not_adopted() -> Result<()> {
        for damage in [
            "schema_key=x'01'",
            "declaration=x'00'",
            "declaration='text'",
            "declaration=zeroblob(26689)",
            "schema_key=zeroblob(26689)",
            "schema_key='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'",
        ] {
            let store = Store::open_with_io(Arc::new(MemoryIO::new()), &path())?;
            let s = schema(1, 1);
            store.create_object("org.demo", "machine", &s)?;
            store
                .connection
                .execute(format!("UPDATE object_types SET {damage}"))?;
            assert!(store.read_definition("org.demo", "machine").is_err());
            assert!(store.create_object("org.demo", "machine", &s).is_err());
            assert!(
                store
                    .commit_declared_object("org.demo", "machine", 0, &object(1))
                    .is_err()
            );
            store.close()?;
        }
        let store = Store::open_with_io(Arc::new(MemoryIO::new()), &path())?;
        store.commit_object("org.demo", "machine", 0, 1, &object(1))?;
        assert!(
            store
                .create_object("org.demo", "machine", &schema(1, 1))
                .is_err()
        );
        assert!(store.read_definition("org.demo", "machine").is_err());
        store.close()
    }

    #[test]
    fn bounded_canonical_schema_profile() {
        let golden = schema(1, 1);
        for end in 0..golden.bytes().len() {
            assert!(EncodedSchema::parse(&golden.bytes()[..end]).is_err());
        }
        let mut trailing = golden.bytes().to_vec();
        trailing.push(0);
        assert!(EncodedSchema::parse(&trailing).is_err());
        let mut overlong = vec![0x98, 4];
        overlong.extend_from_slice(&golden.bytes()[1..]);
        assert!(EncodedSchema::parse(&overlong).is_err());
        let mut too_many = golden.bytes().to_vec();
        too_many.pop();
        too_many.extend_from_slice(&[0x98, 33]);
        assert!(EncodedSchema::parse(&too_many).is_err());
        let mut missing_key = golden.bytes().to_vec();
        missing_key[4..36].fill(0);
        assert!(EncodedSchema::parse(&missing_key).is_err());
        assert!(EncodedSchema::parse(&vec![0; MAX_ENCODED + 1]).is_err());
    }
}
