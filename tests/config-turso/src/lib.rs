//! Config storage backend, NOT an authority boundary or public service API.
//! The owning adapter must authenticate namespace ownership, context and operations
//! before invoking storage. Names and this library's arguments grant no authority.
use std::{cell::Cell, num::NonZeroUsize, path::Path, sync::Arc};
use turso_core::{Connection, Database, Numeric, OpenOptions, PlatformIO, SqliteDialect, Value};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub const MAX_ENTRIES: usize = 256;
pub const MAX_VALUE: usize = 4096;
pub mod io_workload;
pub mod native_io;
pub mod objects;
pub mod payload;
pub mod schemas;
#[cfg(any(test, feature = "measure"))]
pub mod transport_metrics;
pub mod worker;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Setting {
    Text(String),
    Integer(i64),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Snapshot {
    pub revision: i64,
    pub schema_version: i64,
    pub entries: Vec<(String, Setting)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Commit {
    Saved(i64),
    Conflict { actual: i64 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageState {
    Ready,
    RecoveryRequired,
}

#[derive(Debug)]
pub struct RecoveryRequired;
impl std::fmt::Display for RecoveryRequired {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Config storage requires recovery; discard this connection and reopen")
    }
}
impl std::error::Error for RecoveryRequired {}

// Single-owner prototype. No public SQL or connection escape hatch.
pub struct Store {
    connection: Arc<Connection>,
    state: Cell<StorageState>,
}

fn integer(n: i64) -> Value {
    Value::Numeric(Numeric::Integer(n))
}
fn text(s: &str) -> Value {
    Value::from_text(s.to_owned())
}
fn as_integer(value: &Value) -> Result<i64> {
    match value {
        Value::Numeric(Numeric::Integer(n)) => Ok(*n),
        _ => Err("expected integer".into()),
    }
}
fn name(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 128
        && s.split('.').all(|part| {
            !part.is_empty()
                && part
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
        })
}

impl Store {
    pub fn open(path: &Path) -> Result<Self> {
        let io = Arc::new(PlatformIO::new()?);
        Self::open_with_io(io, path.to_str().ok_or("non-UTF8 test path")?)
    }

    /// Explicit storage injection for native bring-up and fault tests. The
    /// caller supplies an already-authorized backend; a path is not authority.
    /// MemoryIO is volatile even when SQL synchronous mode is FULL.
    pub fn open_with_io(io: Arc<dyn turso_core::IO>, path: &str) -> Result<Self> {
        let db = Database::open(io, path, OpenOptions::new(Arc::new(SqliteDialect)))?;
        let store = Self {
            connection: db.connect()?,
            state: Cell::new(StorageState::Ready),
        };
        store.connection.execute(
            // In this pinned engine data_sync_retry=ON returns a sync error
            // instead of panicking. We do NOT retry: transaction_boundary
            // retires the connection until explicit recovery.
            "PRAGMA synchronous=FULL; PRAGMA data_sync_retry=ON; PRAGMA temp_store=MEMORY;",
        )?;
        store.admit_schema()?;
        Ok(store)
    }

    // The private Config database is either empty or has our complete format.
    // Never repair a partial/foreign store on open: missing metadata may mean
    // corruption, not permission to seed a replacement. Serialize empty-store
    // admission and create all format tables/marker in one SQL transaction.
    fn admit_schema(&self) -> Result<()> {
        self.transaction_boundary("BEGIN IMMEDIATE")?;
        let admitted = (|| -> Result<()> {
            let objects = self.query(
                "SELECT name, type FROM sqlite_schema WHERE substr(name,1,7)<>'sqlite_'
                 ORDER BY name LIMIT 5",
                vec![],
            )?;
            if objects.is_empty() {
                self.connection.execute(
                    "CREATE TABLE config_format (version INTEGER NOT NULL);
                    CREATE TABLE collections (
                      namespace TEXT NOT NULL, profile TEXT NOT NULL, revision INTEGER NOT NULL,
                      PRIMARY KEY(namespace, profile));
                    CREATE TABLE revisions (
                      namespace TEXT NOT NULL, profile TEXT NOT NULL, revision INTEGER NOT NULL,
                      schema_version INTEGER NOT NULL, payload_schema BLOB NOT NULL,
                      payload BLOB NOT NULL, PRIMARY KEY(namespace, profile, revision));
                    CREATE TABLE object_types (
                      namespace TEXT NOT NULL, profile TEXT NOT NULL,
                      schema_key BLOB NOT NULL, declaration BLOB NOT NULL,
                      PRIMARY KEY(namespace, profile));
                    INSERT INTO config_format VALUES(3);",
                )?;
            } else {
                let expected: Vec<_> =
                    ["collections", "config_format", "object_types", "revisions"]
                        .into_iter()
                        .map(|n| vec![text(n), text("table")])
                        .collect();
                if objects != expected {
                    return Err("incomplete or foreign Config database schema".into());
                }
                let version = self.query("SELECT version FROM config_format LIMIT 2", vec![])?;
                if version.len() != 1 || as_integer(&version[0][0])? != 3 {
                    return Err("missing or unsupported Config database format".into());
                }
            }
            Ok(())
        })();
        match admitted {
            Ok(()) => self.transaction_boundary("COMMIT"),
            Err(error) => {
                self.transaction_boundary("ROLLBACK")?;
                Err(error)
            }
        }
    }

    pub fn state(&self) -> StorageState {
        self.state.get()
    }

    fn ready(&self) -> Result<()> {
        if self.state() == StorageState::Ready {
            Ok(())
        } else {
            Err(RecoveryRequired.into())
        }
    }

    // A transaction-boundary error may have changed engine/device state even
    // when acknowledgement failed. Restore Ready only on a definite success.
    fn transaction_boundary(&self, sql: &str) -> Result<()> {
        self.state.set(StorageState::RecoveryRequired);
        self.connection.execute(sql)?;
        self.state.set(StorageState::Ready);
        Ok(())
    }

    fn query(&self, sql: &str, bindings: Vec<Value>) -> Result<Vec<Vec<Value>>> {
        let mut statement = self.connection.prepare(sql)?;
        for (index, value) in bindings.into_iter().enumerate() {
            statement.bind_at(NonZeroUsize::new(index + 1).unwrap(), value)?;
        }
        Ok(statement.run_collect_rows()?)
    }

    fn head(&self, namespace: &str, profile: &str) -> Result<i64> {
        let rows = self.query(
            "SELECT revision FROM collections WHERE namespace=? AND profile=? LIMIT 2",
            vec![text(namespace), text(profile)],
        )?;
        if rows.is_empty() {
            let orphan = self.query(
                "SELECT revision FROM revisions WHERE namespace=? AND profile=? LIMIT 1",
                vec![text(namespace), text(profile)],
            )?;
            if !orphan.is_empty() {
                return Err("missing collection head for existing revisions".into());
            }
            Ok(0)
        } else {
            if rows.len() != 1 {
                return Err("ambiguous collection head".into());
            }
            let revision = as_integer(&rows[0][0])?;
            if revision <= 0 {
                return Err("invalid collection head".into());
            }
            Ok(revision)
        }
    }

    pub fn read(&self, namespace: &str, profile: &str) -> Result<Option<Snapshot>> {
        self.ready()?;
        if !name(namespace) || !name(profile) {
            return Err("invalid collection/context name".into());
        }
        // Historical revisions are immutable. Reading the head once and then
        // that revision's payload cannot mix two commits (no pruning in this spike).
        let revision = self.head(namespace, profile)?;
        if revision == 0 {
            return Ok(None);
        }
        self.read_revision(namespace, profile, revision).map(Some)
    }

    pub fn read_revision(&self, namespace: &str, profile: &str, revision: i64) -> Result<Snapshot> {
        let (schema_version, schema, data) =
            self.read_payload(namespace, profile, revision, payload::MAX_PAYLOAD)?;
        if schema != payload::schema_id() {
            return Err("unsupported stored schema".into());
        }
        Ok(Snapshot {
            revision,
            schema_version,
            entries: payload::decode(&data)?,
        })
    }

    // Shared storage mechanics. Public typed adapters decide the expected codec
    // and schema; this private helper must not become an arbitrary blob API.
    fn read_payload(
        &self,
        namespace: &str,
        profile: &str,
        revision: i64,
        maximum: usize,
    ) -> Result<(i64, Vec<u8>, Vec<u8>)> {
        self.ready()?;
        if !name(namespace) || !name(profile) || revision <= 0 {
            return Err("invalid revision address".into());
        }
        let metadata = self.query(
            "SELECT schema_version, payload_schema,
             CASE WHEN length(payload)<=? THEN payload ELSE NULL END
             FROM revisions WHERE namespace=? AND profile=? AND revision=?",
            vec![
                integer(i64::try_from(maximum)?),
                text(namespace),
                text(profile),
                integer(revision),
            ],
        )?;
        if metadata.len() != 1 {
            return Err("missing revision".into());
        }
        let [version, schema, data]: [Value; 3] = metadata
            .into_iter()
            .next()
            .ok_or("missing revision")?
            .try_into()
            .map_err(|_| "invalid revision row")?;
        let schema_version = as_integer(&version)?;
        let Value::Blob(schema) = schema else {
            return Err("invalid stored schema identity".into());
        };
        if schema_version <= 0 || schema.len() != 32 {
            return Err("unsupported stored schema".into());
        }
        let Value::Blob(data) = data else {
            return Err("invalid or oversized stored profile".into());
        };
        // Transfer owned SQL buffers; do not clone the payload just to hand
        // it to the codec.
        Ok((schema_version, schema, data))
    }

    /// Storage-only optimistic revision check. Caller supplies already-approved
    /// data; this does not enforce publisher identity or per-setting schemas.
    pub fn commit(
        &self,
        namespace: &str,
        profile: &str,
        expected: i64,
        schema_version: i64,
        entries: &[(String, Setting)],
    ) -> Result<Commit> {
        let payload = payload::encode(entries)?;
        self.commit_payload(
            namespace,
            profile,
            expected,
            schema_version,
            payload::schema_id(),
            &payload,
        )
    }

    fn commit_payload(
        &self,
        namespace: &str,
        profile: &str,
        expected: i64,
        schema_version: i64,
        schema: &[u8; 32],
        payload: &[u8],
    ) -> Result<Commit> {
        self.ready()?;
        if !name(namespace)
            || !name(profile)
            || expected < 0
            || expected == i64::MAX
            || schema_version <= 0
        {
            return Err("invalid commit metadata".into());
        }
        self.transaction_boundary("BEGIN IMMEDIATE")?;
        let result = (|| -> Result<Commit> {
            let actual = self.head(namespace, profile)?;
            if expected != actual {
                return Ok(Commit::Conflict { actual });
            }
            if actual != 0 {
                let previous = self.query(
                    "SELECT payload_schema FROM revisions WHERE namespace=? AND profile=? AND revision=?",
                    vec![text(namespace), text(profile), integer(actual)],
                )?;
                if previous.len() != 1
                    || !matches!(&previous[0][0], Value::Blob(id) if id.as_slice() == schema)
                {
                    return Err("collection schema change requires explicit migration".into());
                }
            }
            let next = expected + 1;
            self.query(
                "INSERT INTO revisions VALUES(?,?,?,?,?,?)",
                vec![
                    text(namespace),
                    text(profile),
                    integer(next),
                    integer(schema_version),
                    Value::from_slice(schema)?,
                    Value::from_slice(payload)?,
                ],
            )?;
            self.query("INSERT INTO collections VALUES(?,?,?) ON CONFLICT(namespace,profile) DO UPDATE SET revision=excluded.revision",
                       vec![text(namespace), text(profile), integer(next)])?;
            Ok(Commit::Saved(next))
        })();
        match result {
            Ok(Commit::Saved(revision)) => {
                // An I/O failure here has an ambiguous outcome. Do NOT retry it
                // as an unconditional overwrite; reopen and inspect the revision.
                self.transaction_boundary("COMMIT")?;
                Ok(Commit::Saved(revision))
            }
            other => {
                self.transaction_boundary("ROLLBACK")?;
                other
            }
        }
    }

    pub fn checkpoint(&self) -> Result<()> {
        self.ready()?;
        self.state.set(StorageState::RecoveryRequired);
        let rows = self.query("PRAGMA wal_checkpoint(TRUNCATE)", vec![])?;
        self.state.set(StorageState::Ready);
        if rows.len() != 1 || as_integer(&rows[0][0])? != 0 {
            return Err("checkpoint busy".into());
        }
        Ok(())
    }

    pub fn close(self) -> Result<()> {
        // Do not request a fresh checkpoint after an ambiguous failure. Drop
        // releases engine resources; recovery uses a new connection/backend.
        self.ready()?;
        self.connection.close()?;
        Ok(())
    }
}

#[cfg(test)]
mod faults;

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs,
        path::PathBuf,
        process::Command,
        sync::atomic::{AtomicU64, Ordering},
    };

    static COUNTER: AtomicU64 = AtomicU64::new(0);
    struct Temp(PathBuf);
    impl Temp {
        fn new() -> Self {
            let dir = std::env::temp_dir().join(format!(
                "cubit-config-turso-{}-{}",
                std::process::id(),
                COUNTER.fetch_add(1, Ordering::Relaxed)
            ));
            fs::create_dir(&dir).unwrap(); // Exclusive; never reuse someone else's directory.
            Self(dir)
        }
        fn db(&self) -> PathBuf {
            self.0.join("config.sqlite")
        }
    }
    impl Drop for Temp {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.0).unwrap();
        }
    }
    const NS: &str = "com.cubit.desktop.v1";
    const PROFILE: &str = "desk";
    fn values(n: i64) -> Vec<(String, Setting)> {
        vec![
            ("appearance.dark".into(), Setting::Boolean(true)),
            ("counter".into(), Setting::Integer(n)),
            (
                "wallpaper".into(),
                Setting::Text("Cubie 🌟 ' ; DROP TABLE settings; --".into()),
            ),
        ]
    }

    #[test]
    fn typed_revisions_conflicts_profiles_and_reopen() -> Result<()> {
        let temp = Temp::new();
        let db = Store::open(&temp.db())?;
        assert_eq!(db.read(NS, PROFILE)?, None);
        assert_eq!(db.commit(NS, PROFILE, 0, 1, &values(1))?, Commit::Saved(1));
        assert_eq!(
            db.commit(NS, PROFILE, 0, 1, &values(999))?,
            Commit::Conflict { actual: 1 }
        );
        assert_eq!(db.commit(NS, PROFILE, 1, 2, &values(2))?, Commit::Saved(2));
        assert_eq!(db.read_revision(NS, PROFILE, 1)?.entries, values(1));
        assert_eq!(db.read(NS, PROFILE)?.unwrap().entries, values(2));
        assert_eq!(db.commit(NS, "travel", 0, 1, &values(3))?, Commit::Saved(1));
        assert_eq!(db.read(NS, "travel")?.unwrap().entries, values(3));
        assert_eq!(db.read("com.cubit.desktop.v2", PROFILE)?, None);
        // Rollback is a new revision based on old data, never a counter rewind.
        let previous = db.read_revision(NS, PROFILE, 1)?;
        assert_eq!(
            db.commit(NS, PROFILE, 2, previous.schema_version, &previous.entries)?,
            Commit::Saved(3)
        );
        db.close()?;
        let db = Store::open(&temp.db())?;
        assert_eq!(
            db.read(NS, PROFILE)?.unwrap(),
            Snapshot {
                revision: 3,
                schema_version: 1,
                entries: values(1)
            }
        );
        db.close()
    }

    #[test]
    fn two_connections_cannot_lose_an_update() -> Result<()> {
        let temp = Temp::new();
        let first = Store::open(&temp.db())?;
        let second = Store::open(&temp.db())?;
        assert_eq!(first.query("PRAGMA synchronous", vec![])?[0][0], integer(2));
        assert_eq!(
            first.query("PRAGMA data_sync_retry", vec![])?[0][0],
            integer(1)
        );
        assert_eq!(
            first.commit(NS, PROFILE, 0, 1, &values(1))?,
            Commit::Saved(1)
        );
        assert_eq!(
            second.commit(NS, PROFILE, 0, 1, &values(9))?,
            Commit::Conflict { actual: 1 }
        );
        assert_eq!(
            second.commit(NS, PROFILE, 1, 1, &values(2))?,
            Commit::Saved(2)
        );
        assert_eq!(first.read(NS, PROFILE)?.unwrap().entries, values(2));
        second.close()?;
        first.close()
    }

    #[test]
    fn invalid_stored_types_and_unknown_formats_are_rejected() -> Result<()> {
        let temp = Temp::new();
        let db = Store::open(&temp.db())?;
        db.commit(NS, PROFILE, 0, 1, &values(1))?;
        db.connection
            .execute("UPDATE revisions SET payload=X'FF'")?;
        assert!(db.read(NS, PROFILE).is_err());
        db.query(
            "UPDATE revisions SET payload=?, payload_schema=X'00'",
            vec![Value::from_slice(&payload::encode(&values(1))?)?],
        )?;
        assert!(db.read(NS, PROFILE).is_err());
        db.connection
            .execute("UPDATE config_format SET version=999")?;
        db.close()?;
        assert!(Store::open(&temp.db()).is_err());
        Ok(())
    }

    // Test-only raw inspection bypasses admission, never exposed to callers.
    fn inspect_unadmitted(path: &Path) -> Result<Store> {
        let db = Database::open(
            Arc::new(PlatformIO::new()?),
            path.to_str().unwrap(),
            OpenOptions::new(Arc::new(SqliteDialect)),
        )?;
        Ok(Store {
            connection: db.connect()?,
            state: Cell::new(StorageState::Ready),
        })
    }

    #[test]
    fn damaged_or_foreign_schema_is_not_repaired_on_open() -> Result<()> {
        for damage in [
            "DELETE FROM config_format",
            "DROP TABLE config_format",
            "DROP TABLE collections",
            "DROP TABLE revisions",
            "DROP TABLE object_types",
            "INSERT INTO config_format VALUES(3)",
            "UPDATE config_format SET version=2",
            "UPDATE config_format SET version='wrong'",
            "CREATE TABLE foreign_data (value TEXT)",
        ] {
            let temp = Temp::new();
            let db = Store::open(&temp.db())?;
            db.commit(NS, PROFILE, 0, 1, &values(1))?;
            db.connection.execute(damage)?;
            let before = db.query(
                "SELECT name,type,sql FROM sqlite_schema ORDER BY name",
                vec![],
            )?;
            let marker = if damage == "DROP TABLE config_format" {
                None
            } else {
                Some(db.query("SELECT version FROM config_format", vec![])?)
            };
            db.close()?;
            assert!(Store::open(&temp.db()).is_err(), "accepted {damage}");
            let raw = inspect_unadmitted(&temp.db())?;
            assert_eq!(
                raw.query(
                    "SELECT name,type,sql FROM sqlite_schema ORDER BY name",
                    vec![]
                )?,
                before,
                "schema changed after rejecting {damage}"
            );
            if let Some(marker) = marker {
                assert_eq!(
                    raw.query("SELECT version FROM config_format", vec![])?,
                    marker,
                    "format marker reseeded after {damage}"
                );
            }
            raw.close()?;
        }
        Ok(())
    }

    #[test]
    fn unrelated_database_is_not_claimed_as_config() -> Result<()> {
        let temp = Temp::new();
        let raw = inspect_unadmitted(&temp.db())?;
        raw.connection
            .execute("CREATE TABLE notes (body TEXT); INSERT INTO notes VALUES('keep me')")?;
        raw.close()?;
        assert!(Store::open(&temp.db()).is_err());
        let raw = inspect_unadmitted(&temp.db())?;
        assert_eq!(
            raw.query("SELECT name FROM sqlite_schema WHERE type='table'", vec![])?,
            vec![vec![text("notes")]]
        );
        assert_eq!(
            raw.query("SELECT body FROM notes", vec![])?,
            vec![vec![text("keep me")]]
        );
        raw.close()
    }

    #[test]
    fn invalid_or_missing_heads_are_errors_not_absence() -> Result<()> {
        for damage in [
            "UPDATE collections SET revision=0",
            "UPDATE collections SET revision=-1",
            "UPDATE collections SET revision=1.5",
            "UPDATE collections SET revision='invalid'",
            "DELETE FROM collections",
        ] {
            let temp = Temp::new();
            let db = Store::open(&temp.db())?;
            db.commit(NS, PROFILE, 0, 1, &values(1))?;
            db.connection.execute(damage)?;
            db.close()?;
            let db = Store::open(&temp.db())?;
            assert!(db.read(NS, PROFILE).is_err(), "read accepted {damage}");
            assert!(
                db.read_object(NS, PROFILE, &[1; 32]).is_err(),
                "object read accepted {damage}"
            );
            assert!(
                db.commit(NS, PROFILE, 0, 1, &values(2)).is_err(),
                "commit accepted {damage}"
            );
            assert_eq!(
                db.query("SELECT COUNT(*) FROM revisions", vec![])?,
                vec![vec![integer(1)]]
            );
            assert_eq!(db.read_revision(NS, PROFILE, 1)?.entries, values(1));
            db.close()?;
        }
        Ok(())
    }

    #[test]
    fn bounds_and_failure_preserve_revision() -> Result<()> {
        let temp = Temp::new();
        let db = Store::open(&temp.db())?;
        db.commit(NS, PROFILE, 0, 1, &values(1))?;
        assert!(
            db.commit(
                NS,
                PROFILE,
                1,
                1,
                &[("key".into(), Setting::Text("x".repeat(MAX_VALUE + 1)))]
            )
            .is_err()
        );
        assert!(
            db.commit(
                NS,
                PROFILE,
                1,
                1,
                &[
                    ("key".into(), Setting::Integer(1)),
                    ("key".into(), Setting::Integer(2))
                ]
            )
            .is_err()
        );
        assert!(db.commit("com..invalid", PROFILE, 1, 1, &[]).is_err());
        assert!(db.commit(NS, PROFILE, i64::MAX, 1, &[]).is_err());
        let full: Vec<_> = (0..MAX_ENTRIES)
            .map(|n| (format!("item{n:03}"), Setting::Text("v".repeat(MAX_VALUE))))
            .collect();
        let mut too_many = full.clone();
        too_many.push(("overflow".into(), Setting::Boolean(false)));
        assert!(db.commit(NS, PROFILE, 1, 1, &too_many).is_err());
        assert_eq!(db.read(NS, PROFILE)?.unwrap().revision, 1);
        assert_eq!(db.commit(NS, PROFILE, 1, 1, &full)?, Commit::Saved(2));
        assert_eq!(db.read(NS, PROFILE)?.unwrap().entries, full);
        db.close()
    }

    #[test]
    fn backend_error_rolls_back_all_rows() -> Result<()> {
        let temp = Temp::new();
        let db = Store::open(&temp.db())?;
        db.commit(NS, PROFILE, 0, 1, &values(1))?;
        db.commit(NS, "other", 0, 1, &values(1))?;
        db.commit(NS, "other", 1, 1, &values(2))?;
        db.connection
            .execute("CREATE UNIQUE INDEX reject_same_head ON collections(revision)")?;
        // Revision payload insertion succeeds; publishing its head fails.
        assert!(db.commit(NS, PROFILE, 1, 1, &values(2)).is_err());
        assert_eq!(db.read(NS, PROFILE)?.unwrap().entries, values(1));
        assert!(db.read_revision(NS, PROFILE, 2).is_err());
        db.connection.execute("DROP INDEX reject_same_head")?;
        assert_eq!(db.commit(NS, PROFILE, 1, 1, &values(2))?, Commit::Saved(2));
        db.close()
    }

    #[test]
    fn portable_closed_snapshot_and_sqlite_reader() -> Result<()> {
        let temp = Temp::new();
        let db = Store::open(&temp.db())?;
        db.commit(NS, PROFILE, 0, 1, &values(7))?;
        db.checkpoint()?;
        db.close()?;
        // Copy only the checkpointed, closed file: no WAL, lock or journal.
        let other = Temp::new();
        fs::copy(temp.db(), other.db())?;
        let result = Command::new("python3").args(["-c",
            "import sqlite3,sys; c=sqlite3.connect('file:'+sys.argv[1]+'?mode=ro',uri=True); assert c.execute('pragma integrity_check').fetchone()==('ok',); assert c.execute('select count(*) from revisions').fetchone()==(1,); assert c.execute('select typeof(payload),length(payload_schema) from revisions').fetchone()==('blob',32)"])
            .arg(other.db()).status()?;
        assert!(
            result.success(),
            "independent SQLite reader rejected snapshot"
        );
        let db = Store::open(&other.db())?;
        assert_eq!(db.read(NS, PROFILE)?.unwrap().entries, values(7));
        db.close()
    }

    #[test]
    fn crash_child() -> Result<()> {
        let Ok(path) = std::env::var("CUBIT_CONFIG_CRASH_DB") else {
            return Ok(());
        };
        let stage = std::env::var("CUBIT_CONFIG_CRASH_STAGE")?;
        let db = Store::open(Path::new(&path))?;
        if stage == "committed" {
            assert_eq!(db.commit(NS, PROFILE, 1, 1, &values(2))?, Commit::Saved(2));
        } else {
            db.connection.execute("BEGIN IMMEDIATE")?;
            db.query(
                "INSERT INTO revisions VALUES(?,?,2,1,?,X'')",
                vec![
                    text(NS),
                    text(PROFILE),
                    Value::from_slice(payload::schema_id())?,
                ],
            )?;
            if stage != "revision" {
                db.query(
                    "UPDATE revisions SET payload=? WHERE namespace=? AND profile=? AND revision=2",
                    vec![
                        Value::from_slice(&payload::encode(&values(2))?)?,
                        text(NS),
                        text(PROFILE),
                    ],
                )?;
            }
            if stage == "head" {
                db.query(
                    "UPDATE collections SET revision=2 WHERE namespace=? AND profile=?",
                    vec![text(NS), text(PROFILE)],
                )?;
            }
        }
        // Skip ALL Rust destructors/connection cleanup. This models process
        // death, not loss of the host page cache or torn device writes.
        std::process::exit(73);
    }

    #[test]
    fn process_death_never_publishes_partial_revision() -> Result<()> {
        for stage in ["revision", "payload", "head", "committed"] {
            let temp = Temp::new();
            let db = Store::open(&temp.db())?;
            db.commit(NS, PROFILE, 0, 1, &values(1))?;
            db.close()?;
            let status = Command::new(std::env::current_exe()?)
                .args(["--exact", "tests::crash_child", "--nocapture"])
                .env("CUBIT_CONFIG_CRASH_DB", temp.db())
                .env("CUBIT_CONFIG_CRASH_STAGE", stage)
                .status()?;
            assert_eq!(status.code(), Some(73));
            let db = Store::open(&temp.db())?;
            let expected = if stage == "committed" { 2 } else { 1 };
            assert_eq!(
                db.read(NS, PROFILE)?.unwrap().entries,
                values(expected),
                "stage {stage}"
            );
            assert_eq!(db.read(NS, PROFILE)?.unwrap().revision, expected);
            if expected == 1 {
                assert!(db.read_revision(NS, PROFILE, 2).is_err());
            }
            db.close()?;
        }
        Ok(())
    }
}
