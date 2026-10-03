//! Last-resort diagnostics must not allocate or use the normal stderr stream.
use std::ffi::{c_char, c_void};
unsafe extern "C" {
    fn cubit_debug_write(data: *const u8, len: usize);
    fn _Unwind_Backtrace(callback: extern "C" fn(*mut c_void, *mut c_void) -> i32, data: *mut c_void) -> i32;
    fn _Unwind_GetIP(context: *mut c_void) -> usize;
    fn __real_abort() -> !;
    fn __real_mozalloc_abort(message: *const c_char) -> !;
}
fn write(bytes: &[u8]) { unsafe { cubit_debug_write(bytes.as_ptr(), bytes.len()); } }
extern "C" fn frame(context: *mut c_void, data: *mut c_void) -> i32 {
    let count = unsafe { &mut *data.cast::<u32>() };
    let ip = unsafe { _Unwind_GetIP(context) };
    let mut line = *b"0x0000000000000000 ";
    for i in 0..16 { line[17-i] = b"0123456789abcdef"[(ip >> (i*4)) & 15]; }
    write(&line);
    *count += 1;
    if *count == 40 { 5 } else { 0 }
}
fn trace() {
    write(b"PENNY-ABORT: return addresses ");
    let mut count = 0u32;
    unsafe { _Unwind_Backtrace(frame, (&mut count as *mut u32).cast()); }
    write(b"\n");
}
#[unsafe(no_mangle)]
pub extern "C" fn __wrap_abort() -> ! {
    trace();
    unsafe { __real_abort() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __wrap_mozalloc_abort(message: *const c_char) -> ! {
    write(b"PENNY-ABORT: ");
    if !message.is_null() {
        let mut length = 0;
        while length < 512 && unsafe { *message.add(length) } != 0 { length += 1; }
        unsafe { cubit_debug_write(message.cast(), length); }
    }
    write(b"\n");
    trace();
    unsafe { __real_mozalloc_abort(message) }
}
