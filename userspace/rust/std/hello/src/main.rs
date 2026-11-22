//! First std program on CuBit (docs/rust-std.md): threads, locks, condition
//! variables, thread-locals, collections, time and sleep through std.

use std::cell::Cell;
use std::io::Write;
use std::collections::HashMap;
use std::sync::{Arc, Condvar, Mutex, RwLock, mpsc};
use std::thread;
use std::time::{Duration, Instant};

/// Test verdicts go to the debug console, which the headless runner reads.
/// On the Unix-family target that is the CuBit libc's explicit channel
/// (stdout is the program's CuBit stream); the Motor-based std still maps
/// stdout to the console.
#[cfg(target_family = "unix")]
fn say(text: &str) {
    unsafe extern "C" {
        fn cubit_debug_write(text: *const u8, length: usize);
    }
    unsafe { cubit_debug_write(text.as_ptr(), text.len()) }
}

#[cfg(not(target_family = "unix"))]
fn say(text: &str) {
    print!("{text}");
}

fn check(ok: bool, name: &str) -> bool {
    say(&format!("rust-std: {name} {}\n", if ok { "PASS" } else { "FAIL" }));
    ok
}

thread_local! {
    static PER_THREAD: Cell<u64> = const { Cell::new(0) };
}

fn main() {
    say("rust-std: hello from std on CuBit\n");
    let mut ok = true;
    ok &= check(writeln!(std::io::stdout(), "rust-std: to the stdout stream").is_ok(),
                "stdout writes");

    let counter = Arc::new(Mutex::new(0u64));
    let handles: Vec<_> = (0..8u64)
        .map(|i| {
            let counter = counter.clone();
            thread::spawn(move || {
                for _ in 0..10_000 {
                    *counter.lock().unwrap() += 1;
                }
                i
            })
        })
        .collect();
    let ids: u64 = handles.into_iter().map(|h| h.join().unwrap()).sum();
    ok &= check(*counter.lock().unwrap() == 80_000 && ids == 28, "mutex and join across threads");

    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let other = pair.clone();
    let waiter = thread::spawn(move || {
        let (m, c) = &*other;
        let mut ready = m.lock().unwrap();
        while !*ready {
            ready = c.wait(ready).unwrap();
        }
    });
    thread::sleep(Duration::from_millis(5));
    {
        let (m, c) = &*pair;
        *m.lock().unwrap() = true;
        c.notify_all();
    }
    waiter.join().unwrap();
    ok &= check(true, "condvar handoff");

    let values: Vec<u64> = (1..=4u64)
        .map(|i| {
            thread::spawn(move || {
                PER_THREAD.with(|v| v.set(i * 100));
                thread::sleep(Duration::from_millis(2));
                PER_THREAD.with(|v| v.get())
            })
        })
        .map(|h| h.join().unwrap())
        .collect();
    ok &= check(values == [100, 200, 300, 400] && PER_THREAD.with(|v| v.get()) == 0,
                "thread-locals are per thread");

    let (tx, rx) = mpsc::channel();
    for i in 0..4 {
        let tx = tx.clone();
        thread::spawn(move || tx.send(i).unwrap());
    }
    drop(tx);
    let mut got: Vec<i32> = rx.iter().collect();
    got.sort();
    ok &= check(got == [0, 1, 2, 3], "channels");

    let lock = RwLock::new(5);
    ok &= check(*lock.read().unwrap() == 5, "rwlock");

    let mut map = HashMap::new();
    for i in 0..1000 {
        map.insert(format!("key{i}"), i);
    }
    ok &= check(map.len() == 1000 && map["key999"] == 999, "hashmap with random keys");

    let start = Instant::now();
    thread::sleep(Duration::from_millis(30));
    let slept = start.elapsed();
    ok &= check(slept >= Duration::from_millis(30) && slept < Duration::from_secs(2), "sleep and Instant");

    let joined = thread::Builder::new().stack_size(64 * 1024)
        .spawn(|| (0..100u64).sum::<u64>()).unwrap().join().unwrap();
    ok &= check(joined == 4950, "thread with a custom stack");

    say(if ok { "RUST-STD: PASS\n" } else { "RUST-STD: FAIL\n" });
}
