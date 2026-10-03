/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Opt-in native fixture for the libc contract used by the CuBit GC adapter.
//! This is allocator/protection/reuse evidence, not a proof of SpiderMonkey.
use std::ffi::c_void;

unsafe extern "C" {
    fn posix_memalign(result: *mut *mut c_void, alignment: usize, size: usize) -> i32;
    fn mprotect(address: *mut c_void, bytes: usize, protection: i32) -> i32;
    fn free(address: *mut c_void);
}

pub fn aligned_gc_allocator() -> bool {
    const CHUNK: usize = 1024 * 1024;
    let mut previous = [0usize; 32];
    let mut reused = 0;
    for cycle in 0..previous.len() {
        let mut region = std::ptr::null_mut::<c_void>();
        if unsafe { posix_memalign(&mut region, CHUNK, CHUNK) } != 0 || region.is_null() {
            return false;
        }
        if region as usize % CHUNK != 0 { unsafe { free(region) }; return false; }
        if previous[..cycle].contains(&(region as usize)) { reused += 1; }
        previous[cycle] = region as usize;
        let bytes = region.cast::<u8>();
        unsafe {
            bytes.write_volatile(0x3a);
            bytes.add(CHUNK - 1).write_volatile(0xa3);
            if mprotect(region, 4096, 1) != 0 { free(region); return false; }
            if bytes.read_volatile() != 0x3a { return false; }
            if mprotect(region, 4096, 3) != 0 { return false; }
            bytes.write_volatile(0x5c);
            if bytes.read_volatile() != 0x5c || bytes.add(CHUNK - 1).read_volatile() != 0xa3 {
                free(region); return false;
            }
            free(region);
        }
    }
    reused > 0
}

/// Verify the actual scoped path/read-dir interface used by font enumeration.
pub fn fonts() -> Result<usize, String> {
    use std::io::Read;
    let entries = std::fs::read_dir("/fonts").map_err(|e| format!("read_dir: {e}"))?;
    let mut count = 0;
    for entry in entries.take(32) {
        let entry = entry.map_err(|e| format!("directory entry: {e}"))?;
        let path = entry.path();
        if path.extension().is_none_or(|ext| ext != "ttf") { continue; }
        if !entry.file_type().is_ok_and(|kind| kind.is_file()) {
            return Err(format!("not regular: {}", path.display()));
        }
        let mut file = std::fs::File::open(&path)
            .map_err(|e| format!("open {}: {e}", path.display()))?;
        let mut magic = [0u8; 4];
        file.read_exact(&mut magic).map_err(|e| format!("read {}: {e}", path.display()))?;
        if magic != [0, 1, 0, 0] { return Err(format!("header {}: {magic:?}", path.display())); }
        count += 1;
    }
    if count < 4 { return Err(format!("only {count} TrueType entries")); }
    Ok(count)
}

/// Native oracle for the self-only owned-frame query. Run before Servo starts
/// worker threads: a private mapping must be charged, then released on unmap.
pub fn memory_accounting() -> bool {
    unsafe extern "C" {
        fn mmap(addr: *mut c_void, len: usize, prot: i32, flags: i32, fd: i32, offset: i64) -> *mut c_void;
        fn munmap(addr: *mut c_void, len: usize) -> i32;
    }
    const BYTES: usize = 2 * 1024 * 1024;
    let before = crate::cubit_desktop::memory_owned();
    if before == u64::MAX || before == 0 { return false; }
    let region = unsafe { mmap(std::ptr::null_mut(), BYTES, 3, 0x22, -1, 0) };
    if region as usize == usize::MAX { return false; }
    for offset in (0..BYTES).step_by(4096) {
        unsafe { region.cast::<u8>().add(offset).write_volatile(0x5a); }
    }
    let during = crate::cubit_desktop::memory_owned();
    if unsafe { munmap(region, BYTES) } != 0 { return false; }
    let after = crate::cubit_desktop::memory_owned();
    during >= before + BYTES as u64 && after <= during - BYTES as u64
}

/// Opt-in disposable-disk oracle for Penny's filesystem capability scopes.
/// The harness provides both readable and forbidden canaries. An absent file
/// or unsupported operation is not accepted as evidence of access denial.
pub fn filesystem_scopes() -> Result<(), String> {
    use std::fs::{self, OpenOptions};
    use std::io::{ErrorKind, Write};
    const CANARY: &[u8] = b"penny-filesystem-scope-canary\n";
    let readable = "/servo/sandbox-read-control";
    if fs::read(readable).map_err(|e| format!("read control: {e}"))? != CANARY {
        return Err("read control contents differ".into());
    }
    for path in ["/Bookmarks/penny-sandbox-write-probe", "/Downloads/penny-sandbox-write-probe"] {
        // Never truncate pre-existing user data, even if this test is enabled
        // accidentally outside its intended disposable filesystem.
        let mut file = OpenOptions::new().write(true).create_new(true).open(path)
            .map_err(|e| format!("create control {path}: {e}"))?;
        file.write_all(CANARY).map_err(|e| format!("write control {path}: {e}"))?;
        drop(file);
        if fs::read(path).map_err(|e| format!("readback {path}: {e}"))? != CANARY {
            return Err(format!("write control contents differ: {path}"));
        }
    }
    match fs::read("/sandbox-private/canary") {
        Err(e) if e.kind() == ErrorKind::PermissionDenied => {},
        Err(e) => return Err(format!("private read not explicitly denied: {e}")),
        Ok(_) => return Err("private canary was readable".into()),
    }
    match OpenOptions::new().write(true).open(readable) {
        Err(e) if e.kind() == ErrorKind::PermissionDenied => {},
        Err(e) => return Err(format!("read-only write-open not explicitly denied: {e}")),
        Ok(_) => return Err("read-only scope permitted write-open".into()),
    }
    for path in ["/servo/penny-sandbox-denied-create", "/sandbox-private/penny-sandbox-denied-create"] {
        match OpenOptions::new().write(true).create_new(true).open(path) {
            Err(e) if e.kind() == ErrorKind::PermissionDenied => {},
            Err(e) => return Err(format!("create {path} not explicitly denied: {e}")),
            Ok(_) => return Err(format!("forbidden create succeeded: {path}")),
        }
    }
    Ok(())
}
