//! Reference-only concurrency experiments. The SPARK pilot is single-owner.
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
        "glibc" | "mimalloc" | "jemalloc" | "tcmalloc-gperftools"
    ));
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
    let expected = match engine.as_str() {
        "glibc" => "libc.so",
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
    for id in 0..threads {
        let sender = senders[(id + 1) % threads].clone();
        let receiver = receivers[id].take().unwrap();
        let ready = ready.clone();
        let start = start.clone();
        let done = done.clone();
        let remote = work == "remote";
        workers.push(std::thread::spawn(move || {
            let mut seed = 0x123456789abcdefu64 + id as u64;
            let sizes: Vec<usize> = (0..rounds * BATCH)
                .map(|_| 1 + next(&mut seed) % 4096)
                .collect();
            let mut batch: Vec<(usize, usize)> = Vec::with_capacity(BATCH);
            // Warm this thread's allocator before synchronization/timing.
            for size in sizes.iter().take(BATCH) {
                unsafe {
                    let p = malloc(*size);
                    assert!(!p.is_null());
                    batch.push((p as usize, *size));
                }
            }
            for (p, _) in batch.drain(..) {
                unsafe {
                    free(p as *mut u8);
                }
            }
            ready.wait();
            start.wait();
            for sizes in sizes.chunks_exact(BATCH) {
                for &size in sizes {
                    unsafe {
                        let p = malloc(size);
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
                        free(p);
                    }
                }
            }
            done.wait();
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
    println!(
        "{{\"engine\":\"{engine}\",\"workload\":\"{work}\",\"threads\":{threads},\"pairs\":{pairs},\"elapsed_ns\":{elapsed},\"ns_per_pair\":{},\"backend\":\"{backend}\"}}",
        elapsed as f64 / pairs as f64
    );
}
