//! Linux-hosted comparison only. C allocators are references, not CuBit dependencies.
use std::{
    ffi::{CStr, c_char, c_void},
    hint::black_box,
    io::{Read, Write},
    time::Instant,
};
unsafe extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(pointer: *mut u8);
    fn ca_init();
    fn ca_malloc(size: usize) -> *mut u8;
    fn ca_free(pointer: *mut u8);
    fn ca_usable_size(pointer: *mut u8) -> usize;
    fn ca_reserved_bytes() -> usize;
    fn ca_metadata_bytes() -> usize;
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
type Allocate = unsafe extern "C" fn(usize) -> *mut u8;
type Release = unsafe extern "C" fn(*mut u8);
type Usable = unsafe extern "C" fn(*mut u8) -> usize;

fn pattern(slot: usize, byte: usize) -> u8 {
    slot.to_le_bytes()[byte % std::mem::size_of::<usize>()] ^ (byte / 8) as u8
}

fn next(seed: &mut u64) -> u64 {
    *seed ^= *seed << 13;
    *seed ^= *seed >> 7;
    *seed ^= *seed << 17;
    *seed
}

// Optional Linux perf FIFO handshake. Only the churn loop is profiled, excluding
// trace generation, payload validation, warmup and final sample sorting. These
// two handshakes are outside the timed loop; no per-allocation instrumentation.
fn perf_command(pipes: &mut Option<(std::fs::File, std::fs::File)>, command: &[u8]) {
    if let Some((control, ack)) = pipes {
        control.write_all(command).unwrap();
        let mut response = [0u8; 4];
        // Some perf versions include a trailing NUL after ack\n. Consume that
        // before the next response, while accepting versions without it.
        loop {
            ack.read_exact(&mut response[..1]).unwrap();
            if response[0] != 0 {
                break;
            }
        }
        ack.read_exact(&mut response[1..]).unwrap();
        assert_eq!(&response, b"ack\n");
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let engine = &args[1];
    let workload = &args[2];
    let iterations: usize = args[3].parse().unwrap();
    assert!(iterations >= 1000);
    let (allocate, release): (Allocate, Release) = if engine == "cubit-slabs" {
        unsafe {
            ca_init();
        }
        (ca_malloc, ca_free)
    } else {
        (malloc, free)
    };

    let backend = if engine == "cubit-slabs" {
        "SPARK-slab-prototype".to_string()
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
    let expected = match engine.as_str() {
        "glibc" => "libc.so",
        "mimalloc" => "mimalloc",
        "jemalloc" => "jemalloc",
        "tcmalloc-gperftools" => "tcmalloc",
        "cubit-slabs" => "SPARK",
        _ => panic!("engine"),
    };
    assert!(backend.contains(expected), "wrong backend: {backend}");
    let usable: Usable = if engine == "cubit-slabs" {
        ca_usable_size
    } else {
        unsafe {
            let symbol = dlsym(std::ptr::null_mut(), c"malloc_usable_size".as_ptr());
            let mut info = DlInfo {
                filename: std::ptr::null(),
                base: std::ptr::null_mut(),
                symbol: std::ptr::null(),
                address: std::ptr::null_mut(),
            };
            assert!(!symbol.is_null() && dladdr(symbol, &mut info) != 0);
            assert_eq!(
                CStr::from_ptr(info.filename).to_str().unwrap(),
                backend,
                "usable-size function must come from the same allocator"
            );
            std::mem::transmute::<*mut c_void, Usable>(symbol)
        }
    };

    const LIVE: usize = 1024;
    let mut seed = 0x123456789abcdefu64;
    let trace: Vec<(usize, usize)> = (0..iterations)
        .map(|_| {
            let slot = next(&mut seed) as usize % LIVE;
            let size = match workload.as_str() {
                "fixed64" => 64,
                "fixed256" => 256,
                "mixed" => 1 + next(&mut seed) as usize % 4096,
                "small" => 1 + next(&mut seed) as usize % 128,
                "boundary" => {
                    const SIZES: [usize; 25] = [
                        1, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257, 511,
                        512, 513, 1023, 1024, 1025, 2047, 2049, 4096,
                    ];
                    SIZES[next(&mut seed) as usize % SIZES.len()]
                }
                "bimodal" => {
                    let n = next(&mut seed) as usize;
                    if n % 8 == 0 {
                        2048 + (n >> 3) % 2049
                    } else {
                        1 + (n >> 3) % 64
                    }
                }
                _ => panic!("workload"),
            };
            (slot, size)
        })
        .collect();
    let mut pointers: Vec<*mut u8> = vec![std::ptr::null_mut(); LIVE];
    let mut sizes = vec![0usize; LIVE];
    let mut samples = Vec::with_capacity(iterations / 128 + 1);
    let mut timer_samples: Vec<u128> = (0..4096)
        .map(|_| {
            let t = Instant::now();
            black_box(t.elapsed().as_nanos())
        })
        .collect();
    timer_samples.sort_unstable();
    let timer_p50 = timer_samples[timer_samples.len() / 2];

    // Untimed integrity pass: verify every byte before freeing; randomized
    // live intervals detect overlap and preservation errors in the adapter.
    for &(slot, size) in trace.iter().take(10_000) {
        unsafe {
            if !pointers[slot].is_null() {
                for i in 0..sizes[slot] {
                    assert_eq!(pointers[slot].add(i).read(), pattern(slot, i));
                }
                release(pointers[slot]);
            }
            let pointer = allocate(size);
            assert!(!pointer.is_null(), "unexpected OOM");
            // malloc references may use weak alignment for tiny allocations;
            // this is not yet a Rust Layout/aligned-allocation comparison.
            let alignment = if engine == "cubit-slabs" {
                16
            } else {
                1usize << size.ilog2().min(4)
            };
            assert_eq!(pointer as usize % alignment, 0);
            for i in 0..size {
                pointer.add(i).write(pattern(slot, i));
            }
            pointers[slot] = pointer;
            sizes[slot] = size;
        }
    }
    for (slot, p) in pointers.iter_mut().enumerate() {
        unsafe {
            if !p.is_null() {
                for i in 0..sizes[slot] {
                    assert_eq!(p.add(i).read(), pattern(slot, i));
                }
            }
            release(*p);
        }
        *p = std::ptr::null_mut();
    }

    // Warm all live positions equally before measuring churn. Touch first/last
    // bytes, not whole payloads, so this measures allocation rather than memset.
    for slot in 0..LIVE {
        unsafe {
            pointers[slot] = allocate(64);
            assert!(!pointers[slot].is_null());
            pointers[slot].write_volatile(1);
            pointers[slot].add(63).write_volatile(2);
            sizes[slot] = 64;
        }
    }
    let mut perf_pipes = match (
        std::env::var_os("CUBIT_PERF_CONTROL"),
        std::env::var_os("CUBIT_PERF_ACK"),
    ) {
        (None, None) => None,
        (Some(control), Some(ack)) => Some((
            std::fs::OpenOptions::new().write(true).open(control).unwrap(),
            std::fs::File::open(ack).unwrap(),
        )),
        _ => panic!("Both perf FIFO paths must be supplied"),
    };
    perf_command(&mut perf_pipes, b"enable\n");
    let start = Instant::now();
    for (index, &(slot, size)) in trace.iter().enumerate() {
        let sample = if index % 128 == 0 {
            Some(Instant::now())
        } else {
            None
        };
        unsafe {
            release(black_box(pointers[slot]));
            let pointer = allocate(black_box(size));
            assert!(!pointer.is_null(), "unexpected OOM in timed pass");
            pointer.write_volatile(1);
            pointer.add(size - 1).write_volatile(2);
            pointers[slot] = pointer;
            sizes[slot] = size;
        }
        if let Some(t) = sample {
            samples.push(t.elapsed().as_nanos());
        }
    }
    let elapsed = start.elapsed().as_nanos();
    perf_command(&mut perf_pipes, b"disable\n");
    // Introspection is outside timing, never used to write past the request.
    let live_requested: usize = sizes.iter().sum();
    let live_usable: usize = pointers
        .iter()
        .zip(&sizes)
        .map(|(&p, &size)| {
            let bytes = unsafe { usable(p) };
            assert!(bytes >= size);
            bytes
        })
        .sum();
    let (reserved, metadata) = if engine == "cubit-slabs" {
        unsafe { (ca_reserved_bytes(), ca_metadata_bytes()) }
    } else {
        (0, 0)
    }; // Zero means not measured, not absence of overhead.
    for pointer in pointers {
        unsafe {
            release(pointer);
        }
    }
    samples.sort_unstable();
    let p50 = samples[samples.len() / 2];
    let p99 = samples[samples.len() * 99 / 100];
    let status = std::fs::read_to_string("/proc/self/status").unwrap();
    let peak_kib = status
        .lines()
        .find(|l| l.starts_with("VmHWM:"))
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap();
    println!(
        "{{\"engine\":\"{engine}\",\"workload\":\"{workload}\",\"iterations\":{iterations},\"elapsed_ns\":{elapsed},\"ns_per_pair\":{},\"sampled_p50_ns\":{p50},\"sampled_p99_ns\":{p99},\"timer_only_p50_ns\":{timer_p50},\"process_peak_rss_kib\":{peak_kib},\"live_requested_bytes\":{live_requested},\"live_usable_bytes\":{live_usable},\"prototype_reserved_bytes\":{reserved},\"prototype_metadata_bytes\":{metadata},\"backend\":\"{backend}\"}}",
        elapsed as f64 / iterations as f64
    );
}
