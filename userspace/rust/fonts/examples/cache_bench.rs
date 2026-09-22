//! Linux-hosted cache lookup measurement, not end-to-end native UI latency.
use cubit_fonts::cubit_font_glyph;
use std::{
    alloc::{GlobalAlloc, Layout, System},
    hint::black_box,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
    time::Instant,
};

struct Measured;
static MEASURE: AtomicBool = AtomicBool::new(false);
static MAX_REQUEST: AtomicUsize = AtomicUsize::new(0);
static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);
#[global_allocator]
static ALLOCATOR: Measured = Measured;
// Hosted benchmark instrumentation only. Do not allocate in these callbacks.
unsafe impl GlobalAlloc for Measured {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() && MEASURE.load(Ordering::Relaxed) {
            MAX_REQUEST.fetch_max(layout.size(), Ordering::Relaxed);
            let live = LIVE.fetch_add(layout.size(), Ordering::Relaxed) + layout.size();
            PEAK.fetch_max(live, Ordering::Relaxed);
        }
        pointer
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        if MEASURE.load(Ordering::Relaxed) {
            LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
        }
        unsafe {
            System.dealloc(pointer, layout);
        }
    }
}

fn main() {
    for size in 0..2 {
        MAX_REQUEST.store(0, Ordering::Relaxed);
        PEAK.store(0, Ordering::Relaxed);
        let cold = Instant::now();
        MEASURE.store(true, Ordering::Relaxed);
        for face in 0..2 {
            for code in 32..=126 {
                assert!(!cubit_font_glyph(face, size, code).is_null());
            }
        }
        MEASURE.store(false, Ordering::Relaxed);
        println!("Cold 190 glyphs at size {size}: {:?}", cold.elapsed());
        println!(
            "Cold scratch: largest request={} bytes, peak={} bytes, retained={} bytes",
            MAX_REQUEST.load(Ordering::Relaxed),
            PEAK.load(Ordering::Relaxed),
            LIVE.load(Ordering::Relaxed)
        );
        // A font update must not silently wake the native large-object arena.
        assert!(MAX_REQUEST.load(Ordering::Relaxed) <= 4096);
        assert_eq!(LIVE.load(Ordering::Relaxed), 0);
    }
    let start = Instant::now();
    let iterations = 2_000_000;
    for index in 0..iterations {
        black_box(cubit_font_glyph(
            black_box(index % 2),
            0,
            black_box(32 + index % 95),
        ));
    }
    println!(
        "Warm {iterations} lookups: {:?} ({:.1} ns/lookup)",
        start.elapsed(),
        start.elapsed().as_nanos() as f64 / iterations as f64
    );
}
