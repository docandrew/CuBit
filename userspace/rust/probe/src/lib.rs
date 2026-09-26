#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;

use alloc::{boxed::Box, string::String, vec::Vec};
use core::{arch::global_asm, panic::PanicInfo};
use cubit::{CallError, EndpointSlot, Message};

#[global_allocator]
static ALLOCATOR: cubit_allocator::BoundedAllocator = cubit_allocator::BoundedAllocator;

include!(concat!(env!("CUBIT_BINDINGS_DIR"), "/bindings.rs"));

// The ELF loader supplies initialized data/BSS and the requested stack.
// No argv, environment, TLS, GNAT runtime, C startup, or Linux entry ABI.
global_asm!(
    r#"
    .section .text._start,"ax",@progbits
    .global _start
    _start:
        xor rbp, rbp
        and rsp, -16
        cld
        call rust_main
        ud2
    .section .note.GNU-stack,"",@progbits
"#
);

const REPLY_OK: u32 = 0xf000;
const ECHO: u32 = 0x0c10;
const REPORT: u32 = 0x0c11;
const ALLOCATION_REPORT: u32 = 0x0c12;
const HEAP_REPORT: u32 = 0x0c13;
const CLOCK_MONOTONIC_MS: u32 = 0x0b00;
const HELLO: [u64; 4] = [
    u64::from_le_bytes(*b"Hello fr"),
    u64::from_le_bytes(*b"om Rust!"),
    0x8000_0000_ffff_ffff,
    0x1234_5678_9abc_def0,
];

// Force a real BSS segment; the volatile read tests loader initialization.
static mut ZEROED: [u64; 8] = [0; 8];

fn report(outcome: u64, sample: u64) {
    if let Some(host) = EndpointSlot::new(SLOT_TEST_HOST) {
        let _ = host.call(Message::new(REPORT, 2, [outcome, sample, 0, 0]));
    }
}

fn allocation_probe() {
    let mut values = Vec::<u64>::new();
    // Exercise multiple size classes, then cross into page-run allocations.
    for i in 0..16384 {
        values.push(i ^ 0xc0b17);
    }
    for (i, value) in values.iter().enumerate() {
        assert_eq!(*value, i as u64 ^ 0xc0b17);
    }
    // A failed fallible request must leave the original Vec usable.
    assert!(values.try_reserve(cubit_allocator::ARENA_BYTES).is_err());
    assert_eq!(values.len(), 16384);
    assert_eq!(values[16383], 16383 ^ 0xc0b17);
    let mut text = String::from("CuBit ");
    text.push_str("owns this Rust heap");
    assert_eq!(text, "CuBit owns this Rust heap");
    #[repr(align(8192))]
    struct Aligned([u8; 8192]);
    let aligned = Box::new(Aligned([0x5a; 8192]));
    assert_eq!((&*aligned as *const Aligned as usize) % 8192, 0);
    assert!(aligned.0.iter().all(|b| *b == 0x5a));
    // Explicit exhaustion/zeroing through GlobalAlloc, followed by recovery.
    drop(aligned);
    drop(text);
    drop(values);
    use core::alloc::{GlobalAlloc, Layout};
    let full = Layout::from_size_align(cubit_allocator::ARENA_BYTES, 1048576).unwrap();
    unsafe {
        let p = ALLOCATOR.alloc(full);
        assert!(!p.is_null());
        p.write(0xa5);
        p.add(full.size() - 1).write(0x5a);
        assert!(ALLOCATOR.alloc(full).is_null());
        assert_eq!(p.read(), 0xa5);
        assert_eq!(p.add(full.size() - 1).read(), 0x5a);
        ALLOCATOR.dealloc(p, full);
        let zero_layout = Layout::from_size_align(8192, 8192).unwrap();
        let zero = ALLOCATOR.alloc_zeroed(zero_layout);
        assert!(!zero.is_null());
        assert!(
            core::slice::from_raw_parts(zero, 8192)
                .iter()
                .all(|b| *b == 0)
        );
        ALLOCATOR.dealloc(zero, zero_layout);
    }
}

/// Threads and futex locks (docs/threads.md): contended mutex, condvar
/// handoff and join, on however many CPUs the kernel spreads them over.
fn thread_probe() {
    use alloc::sync::Arc;
    use cubit::sync::{Condvar, Mutex};

    let counter = Arc::new(Mutex::new(0u64));
    let handles: Vec<_> = (0..4)
        .map(|i| {
            let counter = counter.clone();
            cubit::thread::spawn_with_stack(64 * 1024, move || {
                for _ in 0..10_000 {
                    *counter.lock() += 1;
                }
                i
            })
            .expect("thread started")
        })
        .collect();
    let ids: u64 = handles.into_iter().map(|h| h.join()).sum();
    assert_eq!(ids, 0 + 1 + 2 + 3);
    assert_eq!(*counter.lock(), 40_000);

    let pair = Arc::new((Mutex::new(0u32), Condvar::new()));
    let other = pair.clone();
    let waiter = cubit::thread::spawn_with_stack(64 * 1024, move || {
        let (m, c) = &*other;
        let mut v = m.lock();
        while *v == 0 {
            v = c.wait(v);
        }
        *v
    })
    .expect("thread started");
    {
        let (m, c) = &*pair;
        *m.lock() = 7;
        c.notify_all();
    }
    assert_eq!(waiter.join(), 7);
    cubit::debug_write("RUST-THREADS: PASS\n");
}

fn heap_failure_probe(host: &EndpointSlot) {
    // The dedicated headless VM has 128 MiB. This runs only after both
    // executables have loaded and completed their allocator/authority checks.
    // No production fault-injection syscall or global host pressure is used.
    let mut saved = Vec::with_capacity(8192);
    saved.resize(8192, 0xabu8);
    unsafe {
        let before = cubit::grow_heap(0).unwrap();
        for _ in 0..3 {
            assert!(cubit::grow_heap(256 * 1024 * 1024).is_none());
            assert_eq!(cubit::grow_heap(0), Some(before));
            assert!(saved.iter().all(|b| *b == 0xab));
        }
        let page = cubit::grow_heap(4096).unwrap();
        assert_eq!(page, before);
        for i in 0..4096 {
            assert_eq!(page.as_ptr().add(i).read_volatile(), 0);
            page.as_ptr().add(i).write_volatile((i % 251) as u8);
        }
    }
    let response = host
        .call(Message::new(HEAP_REPORT, 1, [1, 0, 0, 0]))
        .unwrap();
    assert_eq!(response.tag.label, REPLY_OK);
}

#[unsafe(no_mangle)]
extern "C" fn rust_main() -> ! {
    // SAFETY: read before any thread starts; ZEROED is never mutated.
    let zeroed = unsafe { core::ptr::read_volatile(&raw const ZEROED) };
    if zeroed != [0; 8] {
        report(0, 1);
        cubit::exit(1);
    }
    let host = EndpointSlot::new(SLOT_TEST_HOST).unwrap();
    let response = host.call(Message::new(ECHO, 4, HELLO));
    match response {
        Ok(reply)
            if reply.tag.label == REPLY_OK && reply.tag.length == 4 && reply.words == HELLO => {}
        _ => {
            report(0, 2);
            cubit::exit(2);
        }
    }
    allocation_probe();
    thread_probe();
    match host.call(Message::new(ALLOCATION_REPORT, 1, [1, 0, 0, 0])) {
        Ok(reply) if reply.tag.label == REPLY_OK && reply.tag.length == 0 => {}
        _ => {
            report(0, 6);
            cubit::exit(6);
        }
    }
    let clock = EndpointSlot::new(SLOT_CLOCK).unwrap();
    match clock.call(Message::new(CLOCK_MONOTONIC_MS, 1, [0; 4])) {
        Ok(first) if first.tag.label == REPLY_OK && first.tag.length == 1 => {
            match clock.call(Message::new(CLOCK_MONOTONIC_MS, 1, [0; 4])) {
                Ok(next)
                    if next.tag.label == REPLY_OK
                        && next.tag.length == 1
                        && next.words[0] >= first.words[0] =>
                {
                    report(1, next.words[0]);
                    heap_failure_probe(&host);
                }
                _ => report(0, 3),
            }
        }
        Err(CallError::Unavailable) => report(2, 0),
        _ => report(0, 4),
    }
    cubit::exit(0)
}

#[panic_handler]
fn panic(info: &PanicInfo<'_>) -> ! {
    report(
        0,
        info.location()
            .map_or(5, |location| 1000 + u64::from(location.line())),
    );
    cubit::exit(5)
}
