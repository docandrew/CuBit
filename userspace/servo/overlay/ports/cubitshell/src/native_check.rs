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
