//! Concurrency experiments: the reference allocators, and CuAlloc (the
//! process heap: one spin lock today) through its C entry points.
use std::{
    ffi::{CStr, c_char, c_void},
    sync::{Arc, Barrier, mpsc},
    time::Instant,
};
unsafe extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(p: *mut u8);
    fn dlsym(handle: *mut c_void, name: *const c_char) -> *mut c_void;
    fn dladdr(address: *const c_void, info: *mut DlInfo) -> i32;
    fn cualloc_allocate(bytes: u64, alignment: u64) -> u64;
    fn cualloc_free(item: u64);
}
type Allocate = unsafe extern "C" fn(usize) -> *mut u8;
type Release = unsafe extern "C" fn(*mut u8);
unsafe extern "C" fn cualloc_malloc(size: usize) -> *mut u8 {
    unsafe { cualloc_allocate(size as u64, 16) as *mut u8 }
}
unsafe extern "C" fn cualloc_release(pointer: *mut u8) {
    unsafe { cualloc_free(pointer as u64) }
}
fn rss_kib(field: &str) -> usize {
    let status = std::fs::read_to_string("/proc/self/status").unwrap();
    status
        .lines()
        .find(|l| l.starts_with(field))
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse()
        .unwrap()
}
#[repr(C)]
struct DlInfo {
    filename: *const c_char,
    base: *mut c_void,
    symbol: *const c_char,
    address: *mut c_void,
}
fn next(seed: &mut u64) -> usize {
    *seed ^= *seed << 13;
    *seed ^= *seed >> 7;
    *seed ^= *seed << 17;
    *seed as usize
}
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let engine = &args[1];
    let work = &args[2];
    let threads: usize = args[3].parse().unwrap();
    let rounds: usize = args[4].parse().unwrap();
    assert!(matches!(
        engine.as_str(),
        "glibc" | "mimalloc" | "jemalloc" | "tcmalloc-gperftools" | "cualloc"
    ));
    let (allocate, release): (Allocate, Release) = if engine == "cualloc" {
        (cualloc_malloc, cualloc_release)
    } else {
        (malloc, free)
    };
    assert!(matches!(work.as_str(), "local" | "remote") && threads > 0 && rounds > 0);
    let backend = unsafe {
        let mut info = DlInfo {
            filename: std::ptr::null(),
            base: std::ptr::null_mut(),
            symbol: std::ptr::null(),
            address: std::ptr::null_mut(),
        };
        let sym = dlsym(std::ptr::null_mut(), c"malloc".as_ptr());
        assert!(!sym.is_null() && dladdr(sym, &mut info) != 0);
        CStr::from_ptr(info.filename).to_str().unwrap().to_owned()
    };
    // cualloc runs without LD_PRELOAD: malloc stays glibc's (Rust's own
    // channels and vectors use it), the measured calls go to CuAlloc.
    let expected = match engine.as_str() {
        "glibc" | "cualloc" => "libc.so",
        "tcmalloc-gperftools" => "tcmalloc",
        other => other,
    };
    assert!(backend.contains(expected), "wrong allocator: {backend}");
    const BATCH: usize = 512;
    let ready = Arc::new(Barrier::new(threads + 1));
    let start = Arc::new(Barrier::new(threads + 1));
    let done = Arc::new(Barrier::new(threads + 1));
    let mut senders = Vec::new();
    let mut receivers = Vec::new();
    for _ in 0..threads {
        let (s, r) = mpsc::sync_channel::<Vec<(usize, usize)>>(1);
        senders.push(s);
        receivers.push(Some(r));
    }
    let mut workers = Vec::new();
    let samples_out = Arc::new(std::sync::Mutex::new(Vec::<u32>::new()));
    for id in 0..threads {
        let sender = senders[(id + 1) % threads].clone();
        let receiver = receivers[id].take().unwrap();
        let ready = ready.clone();
        let start = start.clone();
        let done = done.clone();
        let remote = work == "remote";
        let samples_out = samples_out.clone();
        workers.push(std::thread::spawn(move || {
            // Every 64th allocation also times one extra allocate+free pair
            // alone (the latency tail, lock waits included).
            let mut samples: Vec<u32> = Vec::with_capacity(rounds * BATCH / 64 + 1);
            let mut counter = 0usize;
            let mut seed = 0x123456789abcdefu64 + id as u64;
            let sizes: Vec<usize> = (0..rounds * BATCH)
                .map(|_| 1 + next(&mut seed) % 4096)
                .collect();
            let mut batch: Vec<(usize, usize)> = Vec::with_capacity(BATCH);
            // Warm this thread's allocator before synchronization/timing.
            for size in sizes.iter().take(BATCH) {
                unsafe {
                    let p = allocate(*size);
                    assert!(!p.is_null());
                    batch.push((p as usize, *size));
                }
            }
            for (p, _) in batch.drain(..) {
                unsafe {
                    release(p as *mut u8);
                }
            }
            ready.wait();
            start.wait();
            for sizes in sizes.chunks_exact(BATCH) {
                for &size in sizes {
                    unsafe {
                        counter += 1;
                        if counter % 64 == 0 {
                            let t = Instant::now();
                            let q = allocate(size);
                            release(q);
                            samples.push(t.elapsed().as_nanos().min(u32::MAX as u128) as u32);
                        }
                        let p = allocate(size);
                        assert!(!p.is_null());
                        p.write_volatile(0x5a);
                        p.add(size - 1).write_volatile(0x5a);
                        batch.push((p as usize, size));
                    }
                }
                if remote {
                    // Transfer ownership through Rust channels. The producer
                    // stays alive while a different worker frees its blocks.
                    sender.send(batch).unwrap();
                    batch = receiver.recv().unwrap();
                }
                for (p, size) in batch.drain(..) {
                    unsafe {
                        let p = p as *mut u8;
                        assert_eq!(p.read_volatile(), 0x5a);
                        assert_eq!(p.add(size - 1).read_volatile(), 0x5a);
                        release(p);
                    }
                }
            }
            done.wait();
            samples_out.lock().unwrap().extend_from_slice(&samples);
        }));
    }
    ready.wait();
    let begin = Instant::now();
    start.wait();
    done.wait();
    let elapsed = begin.elapsed().as_nanos();
    for worker in workers {
        worker.join().unwrap();
    }
    let pairs = threads * rounds * BATCH;
    let mut samples = samples_out.lock().unwrap().clone();
    samples.sort_unstable();
    let quantile = |q: usize| samples[(samples.len() * q / 1000).min(samples.len() - 1)];
    let (p50, p99, p999) = (quantile(500), quantile(990), quantile(999));
    let max = *samples.last().unwrap();
    let peak = rss_kib("VmHWM:");
    let after = rss_kib("VmRSS:");
    println!(
        "{{\"engine\":\"{engine}\",\"workload\":\"{work}\",\"threads\":{threads},\"pairs\":{pairs},\"elapsed_ns\":{elapsed},\"ns_per_pair\":{},\"pair_p50_ns\":{p50},\"pair_p99_ns\":{p99},\"pair_p999_ns\":{p999},\"pair_max_ns\":{max},\"peak_rss_kib\":{peak},\"rss_after_kib\":{after},\"backend\":\"{backend}\"}}",
        elapsed as f64 / pairs as f64
    );
}
