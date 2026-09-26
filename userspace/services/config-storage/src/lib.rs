//! Dedicated, single-owner native Config database worker. No ambient FS API.
#![deny(unsafe_op_in_unsafe_fn)]
use config_storage::{Store, native_io::NativeIO, worker::Database};
use core::arch::global_asm;
use std::sync::Arc;

#[global_allocator]
static ALLOCATOR: cubit_allocator::BoundedAllocator = cubit_allocator::BoundedAllocator;
include!(concat!(env!("CUBIT_BINDINGS_DIR"), "/bindings.rs"));
#[path = "../std_hooks.rs"]
mod std_hooks;
#[path = "../../../lib/storage/native/bridge.rs"]
mod storage;

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

unsafe extern "C" {
    fn config_storage_hostinit();
    fn cubit_config_storage_path(buffer: *mut u8, capacity: u64, length: *mut u64) -> u32;
    fn cubit_config_storage_run(database: *mut core::ffi::c_void, config_endpoint: u64) -> u32;
}

#[unsafe(no_mangle)]
extern "C" fn rust_main() -> ! {
    // SAFETY: single-threaded startup, exactly once, before any Ada entrypoint.
    // The GNAT-generated binder initializes the full Ada library closure;
    // zeroed BSS alone does not elaborate package constants or bodies.
    unsafe { config_storage_hostinit() };
    let mut bytes = [0u8; 1024];
    let mut length = 0;
    // SAFETY: owned writable buffer and disjoint length, borrowed until return.
    let status =
        unsafe { cubit_config_storage_path(bytes.as_mut_ptr(), bytes.len() as u64, &mut length) };
    let path = bytes
        .get(..usize::try_from(length).unwrap_or(usize::MAX))
        .and_then(|value| std::str::from_utf8(value).ok());
    let Some(path) = path.filter(|path| {
        status == 0xf000
            && !path.is_empty()
            && path.starts_with('@')
            && !path.contains('\0')
            && path.len() <= 1020
    }) else {
        cubit::debug_write("CONFIG-STORAGE: missing/invalid bootstrap database path\n");
        cubit::exit(1);
    };
    let wal = format!("{path}-wal");
    // Exact two-path narrowing on top of manifest-installed filesystem scopes.
    let io = Arc::new(NativeIO::new(
        storage::Bridge::new(SLOT_FILESYSTEM),
        &[path, &wal],
    ));
    let store = match Store::open_with_io(io, path) {
        Ok(store) => store,
        Err(_) => {
            cubit::debug_write("CONFIG-STORAGE: database open failed\n");
            cubit::exit(1);
        }
    };
    let mut database = Database::new(store);
    // SAFETY: one stable, exclusively borrowed Database, no other thread uses
    // it. The Ada loop retains no pointer after return and supplies owned FFI
    // buffers only. All external messages pass its authenticated grant shell.
    let _status =
        unsafe { cubit_config_storage_run((&mut database as *mut Database).cast(), SLOT_CONFIG) };
    // Run returns only on terminal failure. Do not retry a write or checkpoint
    // an uncertain session while exiting. Process teardown reclaims resources.
    cubit::debug_write("CONFIG-STORAGE: retired; restart/recovery required\n");
    cubit::exit(1)
}
