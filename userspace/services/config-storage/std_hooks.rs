// Shared by the native probe and storage worker until the general Rust std
// runtime port owns these hooks. Endpoint/allocator constants come from host.
use super::{ALLOCATOR, SLOT_CLOCK};
use core::alloc::{GlobalAlloc, Layout};
use cubit::{EndpointSlot, Message};
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
