//! Isolated native bring-up, not the Config service or a general std port.
#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    alloc::{GlobalAlloc, Layout},
    arch::global_asm,
};
use cubit::{EndpointSlot, Message};

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

// All bridge pointers originate in the matching patched std and are borrowed
// only for the duration of the call. No Rust unwind crosses this ABI.
#[unsafe(no_mangle)]
unsafe extern "C" fn cubit_std_allocate(size: usize, alignment: usize) -> *mut u8 {
    let Ok(layout) = Layout::from_size_align(size, alignment) else {
        return core::ptr::null_mut();
    };
    unsafe { ALLOCATOR.alloc(layout) }
}
#[unsafe(no_mangle)]
unsafe extern "C" fn cubit_std_release(ptr: *mut u8, size: usize, alignment: usize) {
    let layout = Layout::from_size_align(size, alignment).expect("invalid std allocation layout");
    unsafe { ALLOCATOR.dealloc(ptr, layout) };
}
#[unsafe(no_mangle)]
unsafe extern "C" fn cubit_std_random(ptr: *mut u8, len: usize) -> bool {
    let bytes = unsafe { core::slice::from_raw_parts_mut(ptr, len) };
    getrandom::fill(bytes).is_ok()
}
#[unsafe(no_mangle)]
unsafe extern "C" fn cubit_std_diagnostic(ptr: *const u8, len: usize) {
    let bytes = unsafe { core::slice::from_raw_parts(ptr, len) };
    if let Ok(text) = core::str::from_utf8(bytes) {
        cubit::debug_write(text);
    }
}
#[unsafe(no_mangle)]
unsafe extern "C" fn cubit_std_time(wall: bool, secs: *mut u64, nanos: *mut u32) -> bool {
    let Some(clock) = EndpointSlot::new(SLOT_CLOCK) else {
        return false;
    };
    let operation = if wall { 0x0b01 } else { 0x0b00 };
    let Ok(reply) = clock.call(Message::new(operation, if wall { 0 } else { 1 }, [0; 4])) else {
        return false;
    };
    if reply.tag.label != 0xf000
        || reply.tag.length != if wall { 4 } else { 1 }
        || reply.tag.flags != 0
        || reply.tag.reserved != 0
        || (wall && !(1..=3).contains(&reply.words[3]))
    {
        return false;
    }
    let (seconds, fraction) = if wall {
        (reply.words[0], 0)
    } else {
        (
            reply.words[0] / 1000,
            ((reply.words[0] % 1000) * 1_000_000) as u32,
        )
    };
    unsafe {
        secs.write(seconds);
        nanos.write(fraction);
    }
    true
}

#[unsafe(no_mangle)]
extern "C" fn rust_main() -> ! {
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
    // access or silently claim to have created a native thread.
    assert!(std::fs::File::open("/ungranted").is_err());
    assert!(std::thread::Builder::new().spawn(|| ()).is_err());
    assert!(Instant::now() >= start);
    assert!(std::time::SystemTime::now() >= std::time::UNIX_EPOCH);
    cubit::debug_write("TURSO-NATIVE: std probe PASS\n");
    #[cfg(feature = "turso")]
    run_database();
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
