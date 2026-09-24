// CuBit single-thread userspace std bring-up. No libc/system allocator.
use crate::alloc::{GlobalAlloc, Layout, System};

unsafe extern "C" {
    fn cubit_std_allocate(size: usize, alignment: usize) -> *mut u8;
    fn cubit_std_release(ptr: *mut u8, size: usize, alignment: usize);
}

// SAFETY: the linked bridge implements GlobalAlloc with CuBit's allocator.
#[stable(feature = "alloc_system_type", since = "1.28.0")]
unsafe impl GlobalAlloc for System {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { cubit_std_allocate(layout.size(), layout.align()) }
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { cubit_std_release(ptr, layout.size(), layout.align()) }
    }
    unsafe fn realloc(&self, ptr: *mut u8, old: Layout, new_size: usize) -> *mut u8 {
        unsafe { super::realloc_fallback(self, ptr, old, new_size) }
    }
}
