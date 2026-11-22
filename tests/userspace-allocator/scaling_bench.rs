//! CuAlloc's known scaling costs, against the references (Linux-hosted):
//!   routing N: small-block churn with N huge blocks live. CuAlloc routes
//!     every free through a binary search over its arena directory (one
//!     record per huge block), so its cost grows with log N.
//!   miss K: K arenas' worth of live 4096-byte blocks, then churn that frees
//!     a random one and allocates again. CuAlloc's class hint names the last
//!     arena that served, which is full again, so a miss scans the arenas
//!     from the first (each a failing slab search) until the freed slot.
//! The unit is free + allocate (no payload touches); every 64th pair is also
//! timed alone. Synthetic worst cases: not general performance claims.
use std::{
    ffi::{CStr, c_char, c_void},
    hint::black_box,
    time::Instant,
};
unsafe extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(pointer: *mut u8);
    fn dlsym(handle: *mut c_void, name: *const c_char) -> *mut c_void;
    fn dladdr(address: *const c_void, info: *mut DlInfo) -> i32;
    fn cualloc_allocate(bytes: u64, alignment: u64) -> u64;
    fn cualloc_free(item: u64);
}
#[repr(C)]
struct DlInfo {
    filename: *const c_char,
    base: *mut c_void,
    symbol: *const c_char,
    address: *mut c_void,
}
type Allocate = unsafe extern "C" fn(usize) -> *mut u8;
type Release = unsafe extern "C" fn(*mut u8);
unsafe extern "C" fn cualloc_malloc(size: usize) -> *mut u8 {
    unsafe { cualloc_allocate(size as u64, 16) as *mut u8 }
}
unsafe extern "C" fn cualloc_release(pointer: *mut u8) {
    unsafe { cualloc_free(pointer as u64) }
}
fn next(seed: &mut u64) -> u64 {
    *seed ^= *seed << 13;
    *seed ^= *seed >> 7;
    *seed ^= *seed << 17;
    *seed
}
// Just over CuAlloc's medium limit: a reservation (and arena) of its own.
const HUGE: usize = 1_048_577;
// CuAlloc's largest small class: 16 per slab, 4096 per 16 MiB arena.
const PAGE_BLOCK: usize = 4096;
const BLOCKS_PER_ARENA: usize = 4096;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let engine = args[1].as_str();
    let mode = args[2].as_str();
    let scale: usize = args[3].parse().unwrap();
    let iterations: usize = args[4].parse().unwrap();
    let (allocate, release): (Allocate, Release) = if engine == "cualloc" {
        (cualloc_malloc, cualloc_release)
    } else {
        (malloc, free)
    };
    let backend = if engine == "cualloc" {
        "CuAlloc".to_string()
    } else {
        let mut info = DlInfo {
            filename: std::ptr::null(),
            base: std::ptr::null_mut(),
            symbol: std::ptr::null(),
            address: std::ptr::null_mut(),
        };
        unsafe {
            let symbol = dlsym(std::ptr::null_mut(), c"malloc".as_ptr());
            assert!(!symbol.is_null() && dladdr(symbol, &mut info) != 0);
            CStr::from_ptr(info.filename).to_str().unwrap().to_owned()
        }
    };
    let expected = match engine {
        "glibc" => "libc.so",
        "cualloc" => "CuAlloc",
        other => other,
    };
    assert!(backend.contains(expected), "wrong backend: {backend}");

    let mut seed = 0x9e3779b97f4a7c15u64;
    let mut held: Vec<*mut u8> = Vec::new();
    let mut live: Vec<*mut u8>;
    let size;
    match mode {
        "routing" => {
            for _ in 0..scale {
                let p = unsafe { allocate(HUGE) };
                assert!(!p.is_null());
                held.push(p);
            }
            size = 64;
            live = (0..1024).map(|_| unsafe { allocate(size) }).collect();
        }
        "miss" => {
            size = PAGE_BLOCK;
            live = (0..scale * BLOCKS_PER_ARENA).map(|_| unsafe { allocate(size) }).collect();
        }
        _ => panic!("mode"),
    }
    assert!(live.iter().all(|p| !p.is_null()));
    let picks: Vec<usize> = (0..iterations).map(|_| next(&mut seed) as usize % live.len()).collect();
    // Warm up with the same pattern.
    for &k in picks.iter().take(iterations / 10) {
        unsafe {
            release(live[k]);
            live[k] = allocate(black_box(size));
        }
    }
    let mut samples = Vec::with_capacity(iterations / 64 + 1);
    let start = Instant::now();
    for (i, &k) in picks.iter().enumerate() {
        let t = if i % 64 == 0 { Some(Instant::now()) } else { None };
        unsafe {
            release(black_box(live[k]));
            let p = allocate(black_box(size));
            assert!(!p.is_null());
            live[k] = p;
        }
        if let Some(t) = t {
            samples.push(t.elapsed().as_nanos());
        }
    }
    let elapsed = start.elapsed().as_nanos();
    for p in live.into_iter().chain(held) {
        unsafe { release(p) };
    }
    samples.sort_unstable();
    let p50 = samples[samples.len() / 2];
    let p99 = samples[samples.len() * 99 / 100];
    let p999 = samples[samples.len() * 999 / 1000];
    println!(
        "{{\"engine\":\"{engine}\",\"mode\":\"{mode}\",\"scale\":{scale},\"iterations\":{iterations},\"ns_per_pair\":{},\"sampled_p50_ns\":{p50},\"sampled_p99_ns\":{p99},\"sampled_p999_ns\":{p999},\"backend\":\"{backend}\"}}",
        elapsed as f64 / iterations as f64
    );
}
