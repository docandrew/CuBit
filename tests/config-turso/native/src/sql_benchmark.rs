//! Diagnostic Store-level transaction profile over actual CuBit filesystem IPC.
//! Not the public Config endpoint benchmark; no inline serial while measuring.
use config_storage::{
    Commit, Store,
    native_io::Operation,
    objects::EncodedObject,
    schemas::EncodedSchema,
    transport_metrics::{Monitor, OPERATIONS},
};
use std::sync::Arc;
use turso_core::IO;

unsafe extern "C" {
    fn cubit_probe_clock_calibrate() -> u64;
    fn cubit_probe_clock_counter() -> u64;
}
pub fn counter() -> u64 {
    // SAFETY: scalar local TSC read after generated Ada library initialization.
    unsafe { cubit_probe_clock_counter() }
}

pub fn run(io: Arc<dyn IO>, monitor: Monitor) {
    // SAFETY: same single-owner startup calibration as the File benchmark.
    let rate = unsafe { cubit_probe_clock_calibrate() };
    assert_ne!(rate, 0);
    let mut envelope = vec![0x84, 1, 0x58, 32];
    let mut key = [0u8; 32];
    for n in 1u64..=4 {
        key[(n as usize - 1) * 8..n as usize * 8].copy_from_slice(&n.to_be_bytes());
    }
    envelope.extend_from_slice(&key);
    let mut schema_bytes = envelope.clone();
    schema_bytes.extend_from_slice(&[1, 0x80]);
    let schema = EncodedSchema::parse(&schema_bytes).unwrap();
    let name = "org.cubit.publication";
    let store = Store::open_with_io(io, "@nvme:0/turso-native/transactions.sqlite").unwrap();
    store.create_object(name, "machine", &schema).unwrap();
    let mut samples = Vec::with_capacity(129);
    for revision in 1u8..=129 {
        let mut bytes = envelope.clone();
        bytes.extend_from_slice(&[0x81, 0x82]);
        if revision >= 24 {
            bytes.push(24);
        }
        bytes.extend_from_slice(&[revision, 0, 0x40]);
        let value = EncodedObject::parse(&bytes).unwrap();
        let before = monitor.snapshot();
        let start = counter();
        let result = store.commit_declared_object(name, "machine", i64::from(revision) - 1, &value);
        let stop = counter();
        let delta = monitor.snapshot().difference(before);
        assert_eq!(result.unwrap(), Commit::Saved(i64::from(revision)));
        let ticks = stop.checked_sub(start).unwrap();
        assert!(ticks > 0 && delta.ticks() <= ticks);
        assert!(delta.entries.iter().all(|op| op.errors == 0));
        let restored = store.read_object(name, "machine", &key).unwrap().unwrap();
        assert_eq!(restored.revision, i64::from(revision));
        assert_eq!(restored.object, value);
        samples.push((ticks, delta));
    }
    // Quiescent checkpointed extra DB is inspected independently by SQLite.
    store.close().unwrap();
    cubit::debug_write(&format!(
        "TURSO-SQL: start samples=129 ticks_per_ms={rate}\n"
    ));
    for (index, (ticks, delta)) in samples.iter().enumerate() {
        let read = delta.entry(Operation::Read);
        let write = delta.entry(Operation::Write);
        let flush = delta.entry(Operation::Flush);
        cubit::debug_write(&format!(
            "TURSO-SQL: sample revision={} ticks={ticks} io_ticks={} reads={} read_bytes={} writes={} write_bytes={} vectors={} flushes={}\n",
            index + 1,
            delta.ticks(),
            read.calls,
            read.bytes,
            write.calls,
            write.bytes,
            delta.vectors,
            flush.calls
        ));
    }
    for op in OPERATIONS {
        let (calls, ticks, bytes) =
            samples
                .iter()
                .fold((0, 0, 0), |(calls, ticks, bytes), (_, delta)| {
                    let entry = delta.entry(op);
                    (
                        calls + entry.calls,
                        ticks + entry.ticks,
                        bytes + entry.bytes,
                    )
                });
        cubit::debug_write(&format!(
            "TURSO-SQL: operation={op:?} calls={calls} ticks={ticks} bytes={bytes}\n"
        ));
    }
    cubit::debug_write("TURSO-SQL: PASS\n");
}
