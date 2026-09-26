//! Isolated native bring-up, not the Config service or a general std port.
#![deny(unsafe_op_in_unsafe_fn)]

use core::arch::global_asm;

#[cfg(feature = "turso")]
#[path = "../../../../userspace/lib/storage/native/bridge.rs"]
mod storage;

#[cfg(feature = "turso")]
mod threaded_io;

#[cfg(feature = "bench")]
mod benchmark;
#[cfg(feature = "sql-bench")]
mod sql_benchmark;

#[global_allocator]
static ALLOCATOR: cubit_allocator::BoundedAllocator = cubit_allocator::BoundedAllocator;

include!(concat!(env!("CUBIT_BINDINGS_DIR"), "/bindings.rs"));

global_asm!(
    r#"
    .section .text._start,"ax",@progbits
    .global _start
    _start:
        xor rbp, rbp
        and rsp, -16
        cld
        call rust_main
        ud2
    .section .note.GNU-stack,"",@progbits
"#
);

#[path = "../../../../userspace/services/config-storage/std_hooks.rs"]
mod std_hooks;

#[unsafe(no_mangle)]
extern "C" fn rust_main() -> ! {
    #[cfg(feature = "turso")]
    {
        unsafe extern "C" {
            fn config_worker_probeinit();
        }
        // SAFETY: called once before any Ada entrypoint, including its native
        // storage dependencies. Zero-filled BSS is not Ada package elaboration.
        unsafe { config_worker_probeinit() };
    }
    use std::{
        collections::HashMap,
        sync::{Arc, Mutex, OnceLock},
        time::Instant,
    };
    cubit::debug_write("TURSO-NATIVE: std probe starting\n");
    // CuBit currently saves FXSAVE state, not AVX/AVX-512 extended state.
    // Runtime SIMD dispatch must not select an unsupported register file.
    assert!(!std::is_x86_feature_detected!("avx"));
    assert!(!std::is_x86_feature_detected!("avx512f"));
    let start = Instant::now();
    let mut values = HashMap::new();
    for i in 0..1000 {
        values.insert(format!("setting{i}"), i);
    }
    assert_eq!(values.get("setting42"), Some(&42));
    let shared = Arc::new(Mutex::new(values));
    let clone = shared.clone();
    assert_eq!(clone.lock().unwrap().len(), 1000);
    let once = OnceLock::new();
    assert_eq!(*once.get_or_init(|| 42), 42);
    std::thread_local! { static LOCAL: std::cell::Cell<u32> = const { std::cell::Cell::new(7) }; }
    LOCAL.with(|n| {
        assert_eq!(n.get(), 7);
        n.set(8);
    });
    LOCAL.with(|n| assert_eq!(n.get(), 8));
    // Unsupported facilities must fail, not grant ambient filesystem/network
    // access. Threads now use the shared CuBit std runtime.
    assert!(std::fs::File::open("/ungranted").is_err());
    let worker = std::thread::Builder::new()
        .spawn(|| {
            LOCAL.with(|n| {
                assert_eq!(n.get(), 7);
                n.set(99);
            });
            42
        })
        .unwrap();
    assert_eq!(worker.join().unwrap(), 42);
    LOCAL.with(|n| assert_eq!(n.get(), 8));
    assert!(Instant::now() >= start);
    assert!(std::time::SystemTime::now() >= std::time::UNIX_EPOCH);
    cubit::debug_write("TURSO-NATIVE: std probe PASS\n");
    #[cfg(feature = "turso")]
    run_database();
    #[cfg(feature = "turso")]
    run_persistent();
    cubit::exit(0)
}

#[cfg(feature = "turso")]
fn run_database() {
    use std::sync::Arc;
    use turso_core::{Database, MemoryIO, Numeric, OpenOptions, SqliteDialect, Value};
    let db = Database::open(
        Arc::new(MemoryIO::new()),
        "config.sqlite",
        OpenOptions::new(Arc::new(SqliteDialect)),
    )
    .unwrap();
    let connection = db.connect().unwrap();
    connection.execute("PRAGMA temp_store=MEMORY; CREATE TABLE config(k TEXT PRIMARY KEY, v INTEGER); BEGIN IMMEDIATE; INSERT INTO config VALUES('answer',42); COMMIT").unwrap();
    let mut statement = connection
        .prepare("SELECT v FROM config WHERE k='answer'")
        .unwrap();
    let rows = statement.run_collect_rows().unwrap();
    assert_eq!(rows[0][0], Value::Numeric(Numeric::Integer(42)));
    drop(statement);
    connection.close().unwrap();
    drop(connection);
    drop(db);
    cubit::debug_write("TURSO-NATIVE: volatile SQL transaction PASS\n");

    use config_storage::{Commit, Setting, Store};
    let io = Arc::new(MemoryIO::new());
    let store = Store::open_with_io(io.clone(), "profile.sqlite").unwrap();
    let entries = vec![
        ("theme".to_string(), Setting::Text("Alloy".to_string())),
        ("scale".to_string(), Setting::Integer(125)),
        ("enabled".to_string(), Setting::Boolean(true)),
    ];
    assert_eq!(
        store
            .commit("com.cubit.desktop", "laptop", 0, 1, &entries)
            .unwrap(),
        Commit::Saved(1)
    );
    assert_eq!(
        store
            .commit("com.cubit.desktop", "laptop", 0, 1, &entries)
            .unwrap(),
        Commit::Conflict { actual: 1 }
    );
    let snapshot = store.read("com.cubit.desktop", "laptop").unwrap().unwrap();
    assert_eq!(snapshot.revision, 1);
    assert_eq!(snapshot.entries.len(), 3);
    for entry in &entries {
        assert!(snapshot.entries.contains(entry));
    }
    store.close().unwrap();
    // Reopen against the same in-memory device, not a durable filesystem.
    let reopened = Store::open_with_io(io, "profile.sqlite").unwrap();
    assert_eq!(
        reopened.read("com.cubit.desktop", "laptop").unwrap(),
        Some(snapshot)
    );
    reopened.close().unwrap();
    cubit::debug_write("TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (volatile)\n");

    // Exercise the same File-trait workload used by the Linux benchmark.
    // These are contract checks on MemoryIO, not native disk timings (our
    // Clock bridge has millisecond resolution and incurs IPC per timestamp).
    use config_storage::io_workload::{self, Operation};
    use turso_core::{IO, OpenFlags};
    let io = MemoryIO::new();
    let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
    io_workload::initialize(&io, file.as_ref(), 4096, 32).unwrap();
    for operation in [
        Operation::SequentialRead,
        Operation::RandomRead,
        Operation::Overwrite,
        Operation::VectoredWrite,
        Operation::WriteThenFlush,
    ] {
        for depth in [1, 8, 32] {
            if matches!(operation, Operation::WriteThenFlush) && depth != 1 {
                continue;
            }
            let m =
                io_workload::measure(&io, file.as_ref(), operation, 4096, 32, depth, 1).unwrap();
            assert_eq!(m.latencies_ns.len(), depth);
            assert_eq!(m.peak_deferred, 0);
        }
    }
    cubit::debug_write("TURSO-NATIVE: shared File workload PASS (volatile)\n");
}

#[cfg(feature = "turso")]
fn run_persistent() {
    use config_storage::{
        Commit, Setting, Store,
        io_workload::{self, Operation},
        native_io::NativeIO,
    };
    use std::sync::Arc;
    use turso_core::{IO, OpenFlags};
    const DATABASE: &str = "@nvme:0/turso-native/profile.sqlite";
    const WORKLOAD: &str = "@nvme:0/turso-native/workload.bin";
    const THREADED_WORKLOAD: &str = "@nvme:0/turso-native/threaded.bin";
    let bridge = storage::Bridge::new(SLOT_FILESYSTEM);
    #[cfg(feature = "sql-bench")]
    let (bridge, metrics) =
        config_storage::transport_metrics::Meter::new(bridge, sql_benchmark::counter);
    let transfer_bytes = config_storage::native_io::Transport::transfer_capacity(&bridge).get();
    let io = Arc::new(NativeIO::new(
        bridge,
        &[
            DATABASE,
            "@nvme:0/turso-native/profile.sqlite-wal",
            WORKLOAD,
            THREADED_WORKLOAD,
            #[cfg(feature = "sql-bench")]
            "@nvme:0/turso-native/transactions.sqlite",
            #[cfg(feature = "sql-bench")]
            "@nvme:0/turso-native/transactions.sqlite-wal",
        ],
    ));
    cubit::debug_write("TURSO-NATIVE: filesystem adapter starting\n");
    assert!(
        io.open_file("@nvme:0/ungranted", OpenFlags::Create, false)
            .is_err()
    );
    let file = io.open_file(WORKLOAD, OpenFlags::Create, false).unwrap();
    io_workload::initialize(io.as_ref(), file.as_ref(), 4096, 32).unwrap();
    for operation in [
        Operation::SequentialRead,
        Operation::RandomRead,
        Operation::Overwrite,
        Operation::VectoredWrite,
        Operation::WriteThenFlush,
    ] {
        let m =
            io_workload::measure(io.as_ref(), file.as_ref(), operation, 4096, 32, 1, 1).unwrap();
        assert_eq!(m.latencies_ns.len(), 1);
        assert_eq!(m.peak_deferred, 0);
    }
    drop(file);
    cubit::debug_write("TURSO-NATIVE: shared File workload PASS (filesystem)\n");
    threaded_io::run(
        io.open_file(THREADED_WORKLOAD, OpenFlags::Create, false)
            .unwrap(),
        transfer_bytes,
    );
    #[cfg(feature = "bench")]
    {
        let scratch = io.open_file(WORKLOAD, OpenFlags::None, false).unwrap();
        benchmark::run(io.as_ref(), scratch.as_ref(), transfer_bytes);
    }
    if cfg!(feature = "reopen") {
        let existing = io
            .open_file(DATABASE, OpenFlags::None, false)
            .expect("fresh boot requires an existing database");
        assert!(existing.size().unwrap() > 0);
        drop(existing);
    }
    let store = Store::open_with_io(io.clone(), DATABASE).unwrap();
    assert!(io.open_file(DATABASE, OpenFlags::None, false).is_err());
    assert!(
        io.open_file(
            "@nvme:0/turso-native/profile.sqlite-wal",
            OpenFlags::None,
            false
        )
        .is_err()
    );
    cubit::debug_write("TURSO-NATIVE: database and WAL exclusion PASS\n");
    let mut entries = vec![
        ("theme".to_string(), Setting::Text("Alloy".to_string())),
        ("scale".to_string(), Setting::Integer(125)),
        ("enabled".to_string(), Setting::Boolean(true)),
    ];
    let previous = store.read("com.cubit.desktop", "laptop").unwrap();
    let expected = if cfg!(feature = "reopen") {
        let restored = previous.expect("fresh boot must restore the existing profile");
        assert_eq!(restored.revision, 1);
        assert_eq!(restored.entries.len(), entries.len());
        for entry in &entries {
            assert!(restored.entries.contains(entry));
        }
        cubit::debug_write("TURSO-NATIVE: fresh boot restored revision 1 PASS\n");
        entries[1].1 = Setting::Integer(150);
        1
    } else {
        assert!(previous.is_none(), "seed run requires a fresh profile");
        0
    };
    assert_eq!(
        store
            .commit("com.cubit.desktop", "laptop", expected, 1, &entries)
            .unwrap(),
        Commit::Saved(expected + 1)
    );
    assert_eq!(
        store
            .commit("com.cubit.desktop", "laptop", expected, 1, &entries)
            .unwrap(),
        Commit::Conflict {
            actual: expected + 1
        }
    );
    let snapshot = store.read("com.cubit.desktop", "laptop").unwrap().unwrap();
    assert_eq!(snapshot.revision, expected + 1);
    assert_eq!(snapshot.entries.len(), entries.len());
    for entry in entries {
        assert!(snapshot.entries.contains(&entry));
    }
    store.checkpoint().unwrap();
    store.close().unwrap();
    let reopened = Store::open_with_io(io.clone(), DATABASE).unwrap();
    assert_eq!(
        reopened.read("com.cubit.desktop", "laptop").unwrap(),
        Some(snapshot)
    );
    reopened.close().unwrap();
    cubit::debug_write("TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (filesystem)\n");
    if cfg!(feature = "reopen") {
        cubit::debug_write("TURSO-NATIVE: fresh boot committed revision 2 PASS\n");
    }

    // Exercise the actual Ada publication/worker/codec and Rust database ABI,
    // not just the earlier Rust-only scalar profile. One database owner at a
    // time, and an explicit native IO allow-list; no Linux filesystem fallback.
    unsafe extern "C" {
        fn cubit_config_worker_probe(database: *mut core::ffi::c_void, phase: u32) -> u32;
    }
    let mut worker =
        config_storage::worker::Database::new(Store::open_with_io(io.clone(), DATABASE).unwrap());
    // SAFETY: the local worker is live and exclusively borrowed until return.
    // Ada does not retain the pointer. The phase is Seed=0 or Advance=1.
    assert_eq!(
        unsafe {
            cubit_config_worker_probe(
                (&mut worker as *mut config_storage::worker::Database).cast(),
                u32::from(cfg!(feature = "reopen")),
            )
        },
        0
    );
    worker.close().unwrap();
    if cfg!(feature = "reopen") {
        let mut restored = config_storage::worker::Database::new(
            Store::open_with_io(io.clone(), DATABASE).unwrap(),
        );
        // SAFETY: same exclusive-lifetime contract; phase 2 only reads.
        assert_eq!(
            unsafe {
                cubit_config_worker_probe(
                    (&mut restored as *mut config_storage::worker::Database).cast(),
                    2,
                )
            },
            0
        );
        restored.close().unwrap();
    }
    cubit::debug_write("TURSO-NATIVE: Ada typed worker publication PASS (filesystem)\n");
    #[cfg(feature = "sql-bench")]
    sql_benchmark::run(io.clone(), metrics);
    drop(io);
}
