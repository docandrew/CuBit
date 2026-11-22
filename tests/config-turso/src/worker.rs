//! Private in-process worker bridge. No SQL, path, pointer or authority from
//! a client is accepted here. Ada snapshots/validates the native object, then
//! passes bounded CBOR across this ABI only at the database boundary.
pub mod schemas;
use crate::{
    Commit, Result, StorageState, Store, name,
    objects::{EncodedObject, MAX_ENCODED},
};

#[repr(u32)]
enum Status {
    Loaded = 1,
    Absent = 2,
    LoadFailed = 3,
    Committed = 4,
    Conflict = 5,
    Rejected = 6,
    Uncertain = 7,
}

#[repr(C)]
pub struct Request {
    pub action: u32,
    pub name_length: u32,
    pub context_length: u32,
    pub reserved: u32,
    pub expected_revision: u64,
    pub schema: [u64; 4],
    pub name: [u8; 128],
    pub context: [u8; 128],
    pub input_length: u64,
    pub input: *const u8,
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct Reply {
    pub code: u32,
    pub length: u32,
    pub revision: u64,
    pub data: [u8; MAX_ENCODED],
}
impl Reply {
    fn new(code: Status, revision: u64) -> Self {
        Self {
            code: code as u32,
            length: 0,
            revision,
            data: [0; MAX_ENCODED],
        }
    }
    fn invalid() -> Self {
        Self {
            code: 0,
            length: 0,
            revision: 0,
            data: [0; MAX_ENCODED],
        }
    }
}

/// Single-owner database instance. Recover by reopening with a new instance,
/// never by clearing retirement after an uncertain operation.
pub struct Database {
    store: Store,
    retired: bool,
}
impl Database {
    /// `Store` was opened by trusted startup against already-authorized IO.
    pub fn new(store: Store) -> Self {
        Self {
            store,
            retired: false,
        }
    }
    pub fn close(self) -> Result<()> {
        // Retirement applies to final checkpointing too, not only requests.
        // Consume/drop the owner without asking a failed session to write.
        if self.retired {
            return Err(Box::new(crate::RecoveryRequired));
        }
        self.store.close()
    }
    pub fn retired(&self) -> bool {
        self.retired
    }

    fn fail(&mut self, action: u32) -> Reply {
        self.retired = true;
        Reply::new(
            if action == 1 {
                Status::LoadFailed
            } else {
                Status::Uncertain
            },
            0,
        )
    }

    fn execute(&mut self, request: &Request, input: &[u8]) -> Reply {
        if self.retired || self.store.state() != StorageState::Ready {
            return self.fail(request.action);
        }
        let expected = request.expected_revision;
        // Metadata validation is before any database call. These checks also
        // defend the internal FFI against integration mistakes; not pointers
        // from an untrusted process (which must never reach this function).
        let field = |bytes: &[u8; 128], length: u32| -> Option<String> {
            let n = usize::try_from(length).ok()?;
            if !(1..=128).contains(&n) || bytes[n..].iter().any(|b| *b != 0) {
                return None;
            }
            let s = std::str::from_utf8(&bytes[..n]).ok()?;
            name(s).then(|| s.to_owned())
        };
        if !matches!(request.action, 1 | 2)
            || request.reserved != 0
            || expected > i64::MAX as u64
            || request.schema == [0; 4]
            || (request.action == 1 && (expected != 0 || !input.is_empty()))
            || (request.action == 2 && expected == i64::MAX as u64)
        {
            return Reply::invalid();
        }
        let (Some(object_name), Some(context)) = (
            field(&request.name, request.name_length),
            field(&request.context, request.context_length),
        ) else {
            return Reply::invalid();
        };
        let mut schema = [0; 32];
        for (part, word) in schema.chunks_exact_mut(8).zip(request.schema) {
            part.copy_from_slice(&word.to_be_bytes());
        }
        if request.action == 1 {
            return match self.store.read_object(&object_name, &context, &schema) {
                Ok(Some(snapshot)) if snapshot.schema_version == 1 && snapshot.revision > 0 => {
                    let bytes = snapshot.object.bytes();
                    let mut reply = Reply::new(Status::Loaded, snapshot.revision as u64);
                    reply.length = bytes.len() as u32; // EncodedObject bounds this
                    reply.data[..bytes.len()].copy_from_slice(bytes);
                    reply
                }
                Ok(None) => Reply::new(Status::Absent, 0),
                _ => self.fail(request.action),
            };
        }
        // Parsing/schema mismatch is a definite pre-I/O rejection. Every
        // database error below is uncertain, even if a rollback seems likely.
        let object = match EncodedObject::parse(input) {
            Ok(object) if object.schema() == &schema => object,
            _ => return Reply::new(Status::Rejected, expected),
        };
        match self
            .store
            .commit_object(&object_name, &context, expected as i64, 1, &object)
        {
            Ok(Commit::Saved(revision)) if revision as u64 == expected + 1 => {
                Reply::new(Status::Committed, revision as u64)
            }
            Ok(Commit::Conflict { actual }) if actual >= 0 && actual as u64 != expected => {
                Reply::new(Status::Conflict, actual as u64)
            }
            Err(error) if error.is::<crate::schemas::ManagedWriteDenied>() => {
                Reply::new(Status::Rejected, expected)
            }
            _ => self.fail(request.action),
        }
    }
}

/// Direct Ada/Rust ABI. The caller owns all buffers; nothing is retained.
///
/// # Safety
/// `database` is a live exclusively borrowed `Database`. `request` and its
/// declared input slice are readable, initialized, and stable until return;
/// `output` is aligned writable storage for one `Reply`. They must not overlap
/// each other or the database. No concurrent use or destruction is permitted.
/// Only owned worker buffers, never client/grant pointers, may be passed here.
/// Native builds abort on panic; unwinding must not cross the ABI.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_config_database_execute(
    database: *mut Database,
    request: *const Request,
    output: *mut Reply,
) {
    if output.is_null() {
        return;
    }
    // SAFETY: exclusive, initialized-or-uninitialized output storage is caller-owned.
    unsafe {
        output.write(Reply::invalid());
    }
    if database.is_null() || request.is_null() {
        return;
    }
    let request = unsafe { &*request };
    if request.input_length > MAX_ENCODED as u64
        || (request.input_length != 0 && request.input.is_null())
    {
        return;
    }
    let input = if request.input_length == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(request.input, request.input_length as usize) }
    };
    let reply = unsafe { &mut *database }.execute(request, input);
    unsafe {
        output.write(reply);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        mem::{align_of, offset_of, size_of},
        ptr,
        sync::{
            Arc,
            atomic::{AtomicU64, Ordering},
        },
    };
    use turso_core::MemoryIO;

    pub(super) fn database() -> Database {
        // Turso shares database identity by path even across IO instances.
        // Parallel fixtures must not compete for one logical database.
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let path = format!("worker-{}.db", NEXT.fetch_add(1, Ordering::Relaxed));
        Database::new(Store::open_with_io(Arc::new(MemoryIO::new()), &path).unwrap())
    }
    pub(super) fn request(action: u32) -> Request {
        let mut r = Request {
            action,
            name_length: 9,
            context_length: 4,
            reserved: 0,
            expected_revision: 0,
            schema: [0x0101010101010101; 4],
            name: [0; 128],
            context: [0; 128],
            input_length: 0,
            input: ptr::null(),
        };
        r.name[..9].copy_from_slice(b"org.cubit");
        r.context[..4].copy_from_slice(b"test");
        r
    }
    fn object() -> Vec<u8> {
        let mut e = minicbor::Encoder::new(Vec::new());
        e.array(4)
            .unwrap()
            .u8(1)
            .unwrap()
            .bytes(&[1; 32])
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
        e.into_writer()
    }

    #[test]
    fn x86_64_ada_layout_matches() {
        assert_eq!(size_of::<Request>(), 328);
        assert_eq!(align_of::<Request>(), 8);
        assert_eq!(offset_of!(Request, action), 0);
        assert_eq!(offset_of!(Request, name_length), 4);
        assert_eq!(offset_of!(Request, context_length), 8);
        assert_eq!(offset_of!(Request, reserved), 12);
        assert_eq!(offset_of!(Request, expected_revision), 16);
        assert_eq!(offset_of!(Request, schema), 24);
        assert_eq!(offset_of!(Request, name), 56);
        assert_eq!(offset_of!(Request, context), 184);
        assert_eq!(offset_of!(Request, input_length), 312);
        assert_eq!(offset_of!(Request, input), 320);
        assert_eq!(size_of::<Reply>(), 16 + MAX_ENCODED);
        assert_eq!(align_of::<Reply>(), 8);
        assert_eq!(offset_of!(Reply, code), 0);
        assert_eq!(offset_of!(Reply, length), 4);
        assert_eq!(offset_of!(Reply, revision), 8);
        assert_eq!(offset_of!(Reply, data), 16);
    }

    #[test]
    fn commit_load_conflict_and_context_isolation() {
        let mut db = database();
        let mut r = request(1);
        assert_eq!(db.execute(&r, &[]), Reply::new(Status::Absent, 0));
        r.action = 2;
        let bytes = object();
        assert_eq!(db.execute(&r, &bytes), Reply::new(Status::Committed, 1));
        assert_eq!(db.execute(&r, &bytes), Reply::new(Status::Conflict, 1));
        r.action = 1;
        let loaded = db.execute(&r, &[]);
        assert_eq!(
            (loaded.code, loaded.revision, loaded.length),
            (1, 1, bytes.len() as u32)
        );
        assert_eq!(&loaded.data[..bytes.len()], bytes);
        assert!(loaded.data[bytes.len()..].iter().all(|b| *b == 0));
        r.context[..4].copy_from_slice(b"prod");
        assert_eq!(db.execute(&r, &[]), Reply::new(Status::Absent, 0));
        assert!(!db.retired());
        db.close().unwrap();
    }

    #[test]
    fn lost_commit_receipt_recovers_saved_value_without_replaying() {
        let io = Arc::new(MemoryIO::new());
        let path = "worker-lost-commit-receipt.db";
        let mut db = Database::new(Store::open_with_io(io.clone(), path).unwrap());
        let bytes = object();
        // The database commits, but its receipt never reaches Config's client.
        let receipt = db.execute(&request(2), &bytes);
        assert_eq!(receipt, Reply::new(Status::Committed, 1));
        // Loss of the worker/receipt is not a definite rejection. Retire it
        // without a retry or an explicit final checkpoint.
        assert_eq!(db.fail(2), Reply::new(Status::Uncertain, 0));
        assert!(db.close().unwrap_err().is::<crate::RecoveryRequired>());
        let mut recovered = Database::new(Store::open_with_io(io, path).unwrap());
        let loaded = recovered.execute(&request(1), &[]);
        assert_eq!(loaded.code, Status::Loaded as u32);
        assert_eq!(loaded.revision, 1);
        assert_eq!(&loaded.data[..loaded.length as usize], bytes);
        // Recovery reads first. A stale expected-revision write cannot add a
        // second revision, and no service layer has silently retried it.
        assert_eq!(
            recovered.execute(&request(2), &bytes),
            Reply::new(Status::Conflict, 1)
        );
        assert_eq!(recovered.execute(&request(1), &[]).revision, 1);
        recovered.close().unwrap();
    }

    #[test]
    fn invalid_metadata_and_payload_do_not_write() {
        let mut db = database();
        let bytes = object();
        for case in 0..9 {
            let mut r = request(2);
            match case {
                0 => r.action = 3,
                1 => r.reserved = 1,
                2 => r.name_length = 129,
                3 => r.context_length = 0,
                4 => r.name[127] = b'x',
                5 => r.name[0] = b'.',
                6 => r.schema = [0; 4],
                7 => r.expected_revision = i64::MAX as u64,
                _ => r.expected_revision = u64::MAX,
            }
            assert_eq!(db.execute(&r, &bytes), Reply::invalid(), "case {case}");
        }
        let r = request(2);
        for end in 0..bytes.len() {
            assert_eq!(
                db.execute(&r, &bytes[..end]),
                Reply::new(Status::Rejected, 0)
            );
        }
        let mut mismatch = request(2);
        mismatch.schema[0] = 2;
        assert_eq!(
            db.execute(&mismatch, &bytes),
            Reply::new(Status::Rejected, 0)
        );
        assert_eq!(db.execute(&request(1), &[]), Reply::new(Status::Absent, 0));
        assert!(!db.retired());
        db.close().unwrap();
    }

    #[test]
    fn schema_mismatch_retires_without_further_writes() {
        let mut db = database();
        assert_eq!(db.execute(&request(2), &object()).code, 4);
        let mut r = request(1);
        r.schema[0] = 2;
        assert_eq!(db.execute(&r, &[]), Reply::new(Status::LoadFailed, 0));
        assert!(db.retired());
        let mut commit = request(2);
        commit.expected_revision = 1;
        assert_eq!(
            db.execute(&commit, &object()),
            Reply::new(Status::Uncertain, 0)
        );
        assert_eq!(
            db.execute(&request(1), &[]),
            Reply::new(Status::LoadFailed, 0)
        );
        assert!(db.close().unwrap_err().is::<crate::RecoveryRequired>());
    }

    #[test]
    fn ffi_checks_lengths_and_nulls_before_reading_payload() {
        let mut db = database();
        let mut r = request(1);
        let mut reply = Reply::new(Status::Committed, 99);
        // All nonnull pointers below refer to live, disjoint stack objects.
        unsafe {
            cubit_config_database_execute(&mut db, &r, &mut reply);
            assert_eq!(reply, Reply::new(Status::Absent, 0));
            r.input_length = 1;
            cubit_config_database_execute(&mut db, &r, &mut reply);
            assert_eq!(reply, Reply::invalid());
            r.input_length = MAX_ENCODED as u64 + 1;
            cubit_config_database_execute(&mut db, &r, &mut reply);
            assert_eq!(reply, Reply::invalid());
            cubit_config_database_execute(ptr::null_mut(), &r, &mut reply);
            assert_eq!(reply, Reply::invalid());
            cubit_config_database_execute(&mut db, ptr::null(), &mut reply);
            assert_eq!(reply, Reply::invalid());
            cubit_config_database_execute(&mut db, &r, ptr::null_mut());
        }
        assert!(!db.retired());
        db.close().unwrap();
    }
}
