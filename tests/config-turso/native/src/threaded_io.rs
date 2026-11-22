//! Exercise the real grant-backed File adapter from native Rust threads.
//! No SQL connection is shared, and this does not claim concurrent device I/O:
//! NativeIO serializes the shared transport across each complete operation.
use std::sync::{
    Arc, Barrier,
    atomic::{AtomicUsize, Ordering},
    mpsc,
};
use std::time::Duration;
use turso_core::{Buffer, Completion, File, io::FileSyncType};

pub fn run(file: Arc<dyn File>, transfer_bytes: usize) {
    const THREADS: usize = 4;
    const ROUNDS: usize = 2;
    let record = transfer_bytes.checked_add(37).unwrap();
    let gate = Arc::new(Barrier::new(THREADS));
    let callbacks = Arc::new(AtomicUsize::new(0));
    let (done, completed) = mpsc::channel();
    let mut workers = Vec::new();
    for thread in 0..THREADS {
        let file = file.clone();
        let gate = gate.clone();
        let callbacks = callbacks.clone();
        let done = done.clone();
        workers.push(
            std::thread::Builder::new()
                .stack_size(1024 * 1024)
                .spawn(move || {
                    gate.wait();
                    for round in 0..ROUNDS {
                        let pos = ((round * THREADS + thread) * record) as u64;
                        let expected: Vec<u8> = (0..record)
                            .map(|i| (1 + (i + thread * 17 + round * 31) % 251) as u8)
                            .collect();
                        let parts = [0..17, 17..transfer_bytes, transfer_bytes..record]
                            .into_iter()
                            .map(|range| Arc::new(Buffer::new(expected[range].to_vec())))
                            .collect();
                        let count = callbacks.clone();
                        let reentrant = file.clone();
                        let write = file
                            .pwritev(
                                pos,
                                parts,
                                Completion::new_write(move |r| {
                                    assert_eq!(r.unwrap(), record as i32);
                                    // This must execute after the shared transport is unlocked.
                                    assert!(reentrant.size().unwrap() >= pos + record as u64);
                                    count.fetch_add(1, Ordering::SeqCst);
                                }),
                            )
                            .unwrap();
                        assert!(write.succeeded());
                        let output = Arc::new(Buffer::new(vec![0; record]));
                        let observed = output.clone();
                        let count = callbacks.clone();
                        let read = file
                            .pread(
                                pos,
                                Completion::new_read(output, move |r| {
                                    assert_eq!(r.unwrap().1, record as i32);
                                    assert_eq!(observed.as_slice(), expected.as_slice());
                                    count.fetch_add(1, Ordering::SeqCst);
                                    None
                                }),
                            )
                            .unwrap();
                        assert!(read.succeeded());
                    }
                    done.send(()).unwrap();
                })
                .unwrap(),
        );
    }
    drop(done);
    for _ in 0..THREADS {
        completed.recv_timeout(Duration::from_secs(20)).unwrap();
    }
    for worker in workers {
        worker.join().unwrap();
    }
    assert_eq!(callbacks.load(Ordering::SeqCst), THREADS * ROUNDS * 2);
    assert!(
        file.sync(
            Completion::new_sync(|r| {
                assert_eq!(r.unwrap(), 0);
            }),
            FileSyncType::Fsync
        )
        .unwrap()
        .succeeded()
    );
    cubit::debug_write("TURSO-NATIVE: threaded grant-backed File callbacks PASS\n");
}
