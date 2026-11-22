//! Deterministic I/O-error model, not a power-loss/storage-device simulation.
use super::*;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use turso_core::io::{
    FileId, FileSyncType,
    clock::{MonotonicInstant, WallClockInstant},
};
use turso_core::{Buffer, Clock, Completion, CompletionError, File, IO, MemoryIO, OpenFlags};

#[repr(u8)]
#[derive(Clone, Copy)]
enum Fault {
    None,
    Write,
    Flush,
}

#[test]
fn failed_type_creation_retires_without_retry_or_partial_adoption() -> Result<()> {
    use crate::schemas::EncodedSchema;
    let mut encoder = minicbor::Encoder::new(Vec::new());
    encoder.array(4)?.u8(1)?.bytes(&[1; 32])?.u8(1)?.array(0)?;
    let schema = EncodedSchema::parse(&encoder.into_writer())?;
    for (index, fault) in [Fault::Write, Fault::Flush].into_iter().enumerate() {
        let io = Arc::new(FaultIO::new());
        let path = format!("type-create-fault-{index}.sqlite");
        let store = Store::open_with_io(io.clone(), &path)?;
        io.arm(fault);
        assert!(store.create_object("org.demo", "machine", &schema).is_err());
        assert_eq!(io.fault.load(Ordering::SeqCst), Fault::None as u8);
        assert_eq!(store.state(), StorageState::RecoveryRequired);
        let calls = io.calls.load(Ordering::SeqCst);
        assert!(
            store
                .read_definition("org.demo", "machine")
                .unwrap_err()
                .is::<RecoveryRequired>()
        );
        assert!(
            store
                .create_object("org.demo", "machine", &schema)
                .unwrap_err()
                .is::<RecoveryRequired>()
        );
        assert!(store.close().unwrap_err().is::<RecoveryRequired>());
        assert_eq!(io.calls.load(Ordering::SeqCst), calls);
        let recovered = Store::open_with_io(io, &path)?;
        if let Some(actual) = recovered.read_definition("org.demo", "machine")? {
            assert_eq!(actual, schema);
        }
        assert_eq!(
            recovered.query("SELECT count(*) FROM revisions", vec![])?,
            vec![vec![integer(0)]]
        );
        recovered.close()?;
    }
    Ok(())
}
struct FaultIO {
    memory: MemoryIO,
    fault: Arc<AtomicU8>,
    calls: Arc<AtomicUsize>,
}
impl FaultIO {
    fn new() -> Self {
        Self {
            memory: MemoryIO::new(),
            fault: Arc::new(AtomicU8::new(0)),
            calls: Arc::new(AtomicUsize::new(0)),
        }
    }
    fn arm(&self, fault: Fault) {
        self.fault.store(fault as u8, Ordering::SeqCst);
    }
}
impl Clock for FaultIO {
    fn current_time_monotonic(&self) -> MonotonicInstant {
        self.memory.current_time_monotonic()
    }
    fn current_time_wall_clock(&self) -> WallClockInstant {
        self.memory.current_time_wall_clock()
    }
}
impl IO for FaultIO {
    fn open_file(
        &self,
        path: &str,
        flags: OpenFlags,
        direct: bool,
    ) -> turso_core::Result<Arc<dyn File>> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        Ok(Arc::new(FaultFile {
            inner: self.memory.open_file(path, flags, direct)?,
            fault: self.fault.clone(),
            calls: self.calls.clone(),
        }))
    }
    fn remove_file(&self, path: &str) -> turso_core::Result<()> {
        self.memory.remove_file(path)
    }
    fn file_id(&self, path: &str) -> turso_core::Result<FileId> {
        self.memory.file_id(path)
    }
}
struct FaultFile {
    inner: Arc<dyn File>,
    fault: Arc<AtomicU8>,
    calls: Arc<AtomicUsize>,
}
impl FaultFile {
    fn fail(&self, point: Fault, c: &Completion) -> bool {
        self.calls.fetch_add(1, Ordering::SeqCst);
        if self
            .fault
            .compare_exchange(
                point as u8,
                Fault::None as u8,
                Ordering::SeqCst,
                Ordering::SeqCst,
            )
            .is_ok()
        {
            c.error(CompletionError::IOError(
                std::io::ErrorKind::Other,
                "injected Config storage error",
            ));
            true
        } else {
            false
        }
    }
}
impl File for FaultFile {
    fn lock_file(&self, exclusive: bool) -> turso_core::Result<()> {
        self.inner.lock_file(exclusive)
    }
    fn unlock_file(&self) -> turso_core::Result<()> {
        self.inner.unlock_file()
    }
    fn size(&self) -> turso_core::Result<u64> {
        self.inner.size()
    }
    fn pread(&self, pos: u64, c: Completion) -> turso_core::Result<Completion> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        self.inner.pread(pos, c)
    }
    fn pwrite(
        &self,
        pos: u64,
        buffer: Arc<Buffer>,
        c: Completion,
    ) -> turso_core::Result<Completion> {
        if self.fail(Fault::Write, &c) {
            Ok(c)
        } else {
            self.inner.pwrite(pos, buffer, c)
        }
    }
    fn pwritev(
        &self,
        pos: u64,
        buffers: Vec<Arc<Buffer>>,
        c: Completion,
    ) -> turso_core::Result<Completion> {
        // Preserve MemoryIO's whole-operation completion. Upstream's default
        // fan-out implementation does not complete its parent on a child
        // completion error; a native backend must supply its own pwritev.
        if self.fail(Fault::Write, &c) {
            Ok(c)
        } else {
            self.inner.pwritev(pos, buffers, c)
        }
    }
    fn sync(&self, c: Completion, kind: FileSyncType) -> turso_core::Result<Completion> {
        if self.fail(Fault::Flush, &c) {
            Ok(c)
        } else {
            self.inner.sync(c, kind)
        }
    }
    fn truncate(&self, len: u64, c: Completion) -> turso_core::Result<Completion> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        self.inner.truncate(len, c)
    }
}

#[test]
fn failed_commit_requires_recovery_before_any_more_operations() -> Result<()> {
    for fault in [Fault::Write, Fault::Flush] {
        let io = Arc::new(FaultIO::new());
        let store = Store::open_with_io(io.clone(), "config.sqlite")?;
        let old = vec![("theme".into(), Setting::Text("Alloy".into()))];
        let new = vec![("theme".into(), Setting::Text("Dark".into()))];
        store.commit("desktop", "desk", 0, 1, &old)?;
        io.arm(fault);
        assert!(store.commit("desktop", "desk", 1, 1, &new).is_err());
        assert_eq!(
            io.fault.load(Ordering::SeqCst),
            Fault::None as u8,
            "fault must actually fire"
        );
        assert_eq!(store.state(), StorageState::RecoveryRequired);
        let calls = io.calls.load(Ordering::SeqCst);
        assert!(
            store
                .read("desktop", "desk")
                .unwrap_err()
                .is::<RecoveryRequired>()
        );
        assert!(
            store
                .read_revision("desktop", "desk", 1)
                .unwrap_err()
                .is::<RecoveryRequired>()
        );
        assert!(
            store
                .commit("desktop", "desk", 1, 1, &new)
                .unwrap_err()
                .is::<RecoveryRequired>()
        );
        assert!(store.checkpoint().unwrap_err().is::<RecoveryRequired>());
        assert_eq!(io.calls.load(Ordering::SeqCst), calls);
        assert!(store.close().unwrap_err().is::<RecoveryRequired>());
        assert_eq!(
            io.calls.load(Ordering::SeqCst),
            calls,
            "retirement must not retry/checkpoint"
        );
        let recovered = Store::open_with_io(io, "config.sqlite")?;
        let snapshot = recovered.read("desktop", "desk")?.unwrap();
        // Data may have reached the WAL before a failed sync acknowledgement.
        assert!(snapshot.revision == 1 || snapshot.revision == 2);
        assert_eq!(
            snapshot.entries,
            if snapshot.revision == 1 { old } else { new }
        );
        recovered.close()?;
    }
    Ok(())
}
