//! Linux-only ownership fixture. Production startup supplies authorized native IO.
use cubit_config_turso_spike::{Store, worker::Database};
use std::{path::Path, ptr, slice, str};

/// Linux-only trusted installation fixture, deliberately not a production ABI.
/// # Safety
/// Path points to length readable stable bytes; no other database owner is live.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_config_test_seed_managed(path: *const u8, length: usize) -> u32 {
    use cubit_config_turso_spike::schemas::{Creation, EncodedSchema, Management};
    if path.is_null() || length == 0 || length > 4096 {
        return 0;
    }
    let Ok(path) = str::from_utf8(unsafe { slice::from_raw_parts(path, length) }) else {
        return 0;
    };
    let Ok(store) = Store::open(Path::new(path)) else {
        return 0;
    };
    let mut bytes = vec![0x84, 1, 0x58, 32];
    for word in [1_u64, 2, 3, 4] {
        bytes.extend_from_slice(&word.to_be_bytes());
    }
    bytes.extend_from_slice(&[1, 0x80]);
    let Ok(schema) = EncodedSchema::parse(&bytes) else {
        return 0;
    };
    let registered = store.register_object(
        "org.cubit.managed",
        "machine",
        &schema,
        Management::DeclarationManaged,
    );
    u32::from(matches!(registered, Ok(Creation::Created)) && store.close().is_ok())
}

/// # Safety
/// `path` points to `length` stable readable bytes. Returns an owned instance;
/// callers must serialize access and close it exactly once after all calls finish.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_config_test_open(path: *const u8, length: usize) -> *mut Database {
    if path.is_null() || length == 0 || length > 4096 {
        return ptr::null_mut();
    }
    let Ok(path) = str::from_utf8(unsafe { slice::from_raw_parts(path, length) }) else {
        return ptr::null_mut();
    };
    match Store::open(Path::new(path)) {
        Ok(store) => Box::into_raw(Box::new(Database::new(store))),
        Err(_) => ptr::null_mut(),
    }
}

/// # Safety
/// `database` is an exclusively owned pointer returned by open, not yet closed.
/// The pointer is consumed even on failure; it must never be used again.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_config_test_close(database: *mut Database) -> u32 {
    if database.is_null() {
        return 0;
    }
    let database = unsafe { Box::from_raw(database) };
    u32::from(database.close().is_ok())
}
