//! Rust's global allocator on CuBit: CuAlloc, the one process heap
//! (userspace/allocator/process, docs/userspace-allocator.md). Every
//! language in a process allocates from the same heap: this crate, Ada's
//! System.Memory and the libc malloc family all call CuAlloc's C entry
//! points, which also serialize the heap. Link `libcubit_allocator.a` (or a
//! libc.a / libgnat-user.a that carries the same objects); hosted tests link
//! the hosted CuAlloc over Linux memory.
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

use core::alloc::{GlobalAlloc, Layout};

/// The largest alignment CuAlloc serves.
pub const MAX_ALIGNMENT: usize = 1 << 30;

unsafe extern "C" {
    fn cualloc_allocate(bytes: u64, alignment: u64) -> u64;
    fn cualloc_allocate_zeroed(bytes: u64, alignment: u64) -> u64;
    fn cualloc_free(item: u64);
    fn cualloc_reallocate(item: u64, bytes: u64) -> u64;
}

/// The process heap. Any number of instances use the same heap.
pub struct BoundedAllocator;

// SAFETY: CuAlloc returns live, disjoint blocks of at least the requested
// size and alignment (or null), serializes its own state, frees only blocks
// it returned, and moves contents on realloc before freeing the old block.
unsafe impl GlobalAlloc for BoundedAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if layout.align() > MAX_ALIGNMENT {
            return core::ptr::null_mut();
        }
        // SAFETY: plain C call; arguments are values.
        unsafe { cualloc_allocate(layout.size() as u64, layout.align() as u64) as *mut u8 }
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        if layout.align() > MAX_ALIGNMENT {
            return core::ptr::null_mut();
        }
        // SAFETY: as alloc; CuAlloc zeroes the block.
        unsafe { cualloc_allocate_zeroed(layout.size() as u64, layout.align() as u64) as *mut u8 }
    }
    unsafe fn dealloc(&self, pointer: *mut u8, _: Layout) {
        // SAFETY: the caller hands back a live block from this heap.
        unsafe { cualloc_free(pointer as u64) }
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        if layout.align() <= 16 {
            // SAFETY: a live block; CuAlloc keeps 16-byte alignment and the
            // contents, and frees the old block only after the move.
            return unsafe { cualloc_reallocate(pointer as u64, new_size as u64) as *mut u8 };
        }
        // Over-aligned: keep the alignment explicitly.
        let Ok(new_layout) = Layout::from_size_align(new_size, layout.align()) else {
            return core::ptr::null_mut();
        };
        // SAFETY: distinct live blocks; copy only the bytes both hold.
        unsafe {
            let replacement = self.alloc(new_layout);
            if !replacement.is_null() {
                core::ptr::copy_nonoverlapping(pointer, replacement, layout.size().min(new_size));
                self.dealloc(pointer, layout);
            }
            replacement
        }
    }
}
