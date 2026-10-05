//! Penny has no stderr-stream subscriber. Send its diagnostics directly to the
//! host-captured kernel console, without allocating or entering stream IPC.
use std::ffi::c_void;
#[repr(C)]
pub struct IoVec { base: *const c_void, len: usize }
unsafe extern "C" {
    fn cubit_debug_write(data: *const u8, len: usize);
    fn __real___cubit_fd_writev(fd: i32, iov: *const IoVec, count: i32) -> isize;
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __wrap___cubit_fd_writev(fd: i32, iov: *const IoVec, count: i32) -> isize {
    if fd != 2 { return unsafe { __real___cubit_fd_writev(fd, iov, count) }; }
    if count < 0 || count > 1024 { return -22; }
    if count > 0 && iov.is_null() { return -14; }
    let mut total = 0usize;
    for index in 0..count as usize {
        let entry = unsafe { &*iov.add(index) };
        let Some(next) = total.checked_add(entry.len) else { return -22; };
        if next > isize::MAX as usize { return -22; }
        total = next;
    }
    for index in 0..count as usize {
        let entry = unsafe { &*iov.add(index) };
        if entry.len != 0 { unsafe { cubit_debug_write(entry.base.cast(), entry.len); } }
    }
    total as isize
}
