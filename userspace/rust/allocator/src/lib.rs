//! Opt-in bounded allocator: SPARK metadata, Rust payload/GlobalAlloc boundary.
//! Requires the matching Ada archive at final link. Native backing uses sbrk;
//! Linux-hosted tests acquire backing directly from System, not this allocator.
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    alloc::{GlobalAlloc, Layout},
    ptr,
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

#[cfg(not(any(target_os = "none", target_os = "cubit")))]
extern crate std;

pub const ARENA_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_ALIGNMENT: usize = 1024 * 1024;

unsafe extern "C" {
    fn cubit_heap_init();
    fn cubit_heap_allocate(bytes: u64, alignment: u64) -> u64;
    fn cubit_heap_release(offset: u64) -> u64;
    fn cubit_heap_usable_size(offset: u64) -> u64;
}

// Independent backing permits using only the slab arena or only the large
// arena. Metadata remains bounded; payload no longer occupies static BSS.
static LARGE_BACKING: AtomicPtr<u8> = AtomicPtr::new(ptr::null_mut());
// A slab never spans a 64 KiB boundary, and small requests have alignment at
// most 4 KiB. Native small-object payload can therefore be backed in separate
// 1 MiB chunks without changing the proved metadata's logical offsets.
const SMALL_CHUNK_BYTES: usize = 1024 * 1024;
static SMALL_BACKING: [AtomicPtr<u8>; ARENA_BYTES / SMALL_CHUNK_BYTES] =
    [const { AtomicPtr::new(ptr::null_mut()) }; ARENA_BYTES / SMALL_CHUNK_BYTES];
static LOCKED: AtomicBool = AtomicBool::new(false);
static INITIALIZED: AtomicBool = AtomicBool::new(false);

struct Guard;
impl Guard {
    fn acquire() -> Self {
        while LOCKED
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            core::hint::spin_loop();
        }
        if !INITIALIZED.load(Ordering::Relaxed) {
            // SAFETY: this lock is also the exclusive initialization gate.
            unsafe {
                cubit_heap_init();
            }
            INITIALIZED.store(true, Ordering::Relaxed);
        }
        Self
    }
}
impl Drop for Guard {
    fn drop(&mut self) {
        LOCKED.store(false, Ordering::Release);
    }
}

/// All instances use one serialized process-local heap. The guard makes this
/// boundary safe to call concurrently. This is not a scalable
/// concurrent-allocator design.
pub struct BoundedAllocator;

// Called with Guard held. Native heap users outside this allocator must obey
// the same serialization rule. Growth itself is one kernel operation.
unsafe fn large_backing() -> *mut u8 {
    let existing = LARGE_BACKING.load(Ordering::Relaxed);
    if !existing.is_null() {
        return existing;
    }
    #[cfg(any(target_os = "none", target_os = "cubit"))]
    let result = {
        // One atomic growth request includes alignment slack; never query then
        // assume a separate growth call returns the same break.
        let Some(raw) = (unsafe { cubit::grow_heap(ARENA_BYTES + MAX_ALIGNMENT - 1) }) else {
            return ptr::null_mut();
        };
        let address = (raw.as_ptr().addr() + MAX_ALIGNMENT - 1) & !(MAX_ALIGNMENT - 1);
        raw.as_ptr().with_addr(address)
    };
    #[cfg(not(any(target_os = "none", target_os = "cubit")))]
    let result = unsafe {
        std::alloc::System.alloc(Layout::from_size_align(ARENA_BYTES, MAX_ALIGNMENT).unwrap())
    };
    LARGE_BACKING.store(result, Ordering::Relaxed);
    result
}

// GlobalAlloc requires a live pointer from this heap. Determine which separate
// arena owns it before pointer subtraction; never subtract unrelated pointers.
unsafe fn offset_of(pointer: *mut u8) -> u64 {
    for (index, chunk) in SMALL_BACKING.iter().enumerate() {
        let base = chunk.load(Ordering::Relaxed);
        if !base.is_null() && pointer.addr().wrapping_sub(base.addr()) < SMALL_CHUNK_BYTES {
            return (index * SMALL_CHUNK_BYTES + unsafe { pointer.offset_from(base) } as usize)
                as u64;
        }
    }
    let large = LARGE_BACKING.load(Ordering::Relaxed);
    ARENA_BYTES as u64 + unsafe { pointer.offset_from(large) } as u64
}

// Called under Guard. The returned pointer belongs to the backing allocation
// selected by the logical metadata offset (not a fabricated contiguous arena).
unsafe fn payload_at(offset: usize) -> *mut u8 {
    if offset < ARENA_BYTES {
        let chunk = &SMALL_BACKING[offset / SMALL_CHUNK_BYTES];
        let mut base = chunk.load(Ordering::Relaxed);
        if base.is_null() {
            const ALIGNMENT: usize = 4096;
            #[cfg(any(target_os = "none", target_os = "cubit"))]
            {
                let Some(raw) = (unsafe { cubit::grow_heap(SMALL_CHUNK_BYTES + ALIGNMENT - 1) })
                else {
                    return ptr::null_mut();
                };
                let address = (raw.as_ptr().addr() + ALIGNMENT - 1) & !(ALIGNMENT - 1);
                base = raw.as_ptr().with_addr(address);
            }
            #[cfg(not(any(target_os = "none", target_os = "cubit")))]
            {
                base = unsafe {
                    std::alloc::System
                        .alloc(Layout::from_size_align(SMALL_CHUNK_BYTES, ALIGNMENT).unwrap())
                };
                if base.is_null() {
                    return base;
                }
            }
            chunk.store(base, Ordering::Relaxed);
        }
        // SAFETY: metadata small blocks cannot cross a slab/chunk boundary.
        return unsafe { base.add(offset % SMALL_CHUNK_BYTES) };
    }
    let base = unsafe { large_backing() };
    if base.is_null() {
        base
    } else {
        unsafe { base.add(offset % ARENA_BYTES) }
    }
}

// Called only while holding Guard; returns null without changing live payload.
unsafe fn allocate(layout: Layout) -> *mut u8 {
    if layout.size() == 0 || layout.size() > ARENA_BYTES || layout.align() > MAX_ALIGNMENT {
        return ptr::null_mut();
    }
    // SAFETY: metadata initialized under Guard; validated sizes fit the ABI.
    let offset = unsafe { cubit_heap_allocate(layout.size() as u64, layout.align() as u64) };
    if offset == u64::MAX {
        return ptr::null_mut();
    }
    let payload = unsafe { payload_at(offset as usize) };
    if payload.is_null() {
        // No payload was published. Return metadata ownership on provider
        // failure so a later retry has the full capacity available.
        unsafe {
            cubit_heap_release(offset);
        }
        return ptr::null_mut();
    }
    // SAFETY: the core returns disjoint offsets within the selected arena.
    payload
}

// SAFETY: Guard serializes the singleton metadata and initialization. Every
// successful return is suitably aligned, live and disjoint; invalid/unsupported
// layouts fail. Realloc allocates first and frees only after preserving bytes.
unsafe impl GlobalAlloc for BoundedAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let _guard = Guard::acquire();
        unsafe { allocate(layout) }
    }
    unsafe fn dealloc(&self, pointer: *mut u8, _: Layout) {
        let _guard = Guard::acquire();
        // SAFETY: GlobalAlloc caller supplies a live allocation from this heap.
        let offset = unsafe { offset_of(pointer) };
        unsafe {
            cubit_heap_release(offset);
        }
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { self.alloc(layout) };
        if !pointer.is_null() {
            // SAFETY: this allocation uniquely owns at least layout.size bytes.
            unsafe {
                pointer.write_bytes(0, layout.size());
            }
        }
        pointer
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let Ok(new_layout) = Layout::from_size_align(new_size, layout.align()) else {
            return ptr::null_mut();
        };
        if new_size == 0 {
            return ptr::null_mut();
        }
        let _guard = Guard::acquire();
        // SAFETY: caller supplies a live allocation and its original layout.
        let offset = unsafe { offset_of(pointer) };
        let usable = unsafe { cubit_heap_usable_size(offset) } as usize;
        if new_size <= usable {
            return pointer;
        }
        let replacement = unsafe { allocate(new_layout) };
        if !replacement.is_null() {
            // SAFETY: distinct live blocks; copy only original initialized-or-
            // uninitialized bytes, without interpreting them as Rust values.
            unsafe {
                ptr::copy_nonoverlapping(pointer, replacement, layout.size().min(new_size));
                cubit_heap_release(offset);
            }
        }
        replacement
    }
}
