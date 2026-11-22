use super::*;
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicUsize, Ordering};

const PATHS: &[&str] = &["profile.sqlite", "profile.sqlite-wal", "workload"];
const TRANSFER_BYTES: usize = 65536;
#[derive(Default)]
struct Disk {
    files: BTreeMap<String, Vec<u8>>,
    handles: BTreeMap<u64, String>,
    next: u64,
    calls: usize,
    writes: usize,
    reads: usize,
    vectors: usize,
    fail_write: usize,
    short_write: usize,
    fail_flush: bool,
    fail_open: bool,
    oversized_read: bool,
    bad_resize: bool,
    bad_close: bool,
    trace: Vec<(Operation, u64, u64, usize)>,
}
struct Model(Arc<Mutex<Disk>>, NonZeroUsize);
impl Transport for Model {
    fn transfer_capacity(&self) -> NonZeroUsize {
        self.1
    }
    fn write_vectored(
        &mut self,
        handle: u64,
        position: u64,
        parts: &[&[u8]],
    ) -> Result<u64, TransportError> {
        assert!(!parts.is_empty() && parts.len() <= MAX_WRITE_SEGMENTS);
        assert!(parts.iter().all(|p| !p.is_empty()));
        assert!(parts.iter().map(|p| p.len()).sum::<usize>() <= self.1.get());
        self.0.lock().unwrap().vectors += 1;
        // The model joins bytes to represent one device write. The production
        // FFI instead fills its existing grant directly, with no payload Vec.
        self.call(Operation::Write, handle, position, &parts.concat(), &mut [])
    }
    fn call(
        &mut self,
        op: Operation,
        handle: u64,
        pos: u64,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<u64, TransportError> {
        let mut disk = self.0.lock().unwrap();
        disk.trace
            .push((op, handle, pos, input.len().max(output.len())));
        if op == Operation::Read {
            assert!(output.len() <= self.1.get());
            disk.reads += 1;
        } else if op == Operation::Write {
            assert!(input.len() <= self.1.get());
        }
        disk.calls += 1;
        if matches!(op, Operation::OpenExisting | Operation::OpenCreate) {
            if disk.fail_open {
                return Err(TransportError::Failed(0xf006));
            }
            let path = std::str::from_utf8(input).unwrap().to_string();
            if disk.handles.values().any(|v| *v == path) {
                return Err(TransportError::Rejected(0xf00f));
            }
            if op == Operation::OpenExisting && !disk.files.contains_key(&path) {
                return Err(TransportError::Rejected(0xf00b));
            }
            disk.files.entry(path.clone()).or_default();
            disk.next += 1;
            let id = disk.next;
            disk.handles.insert(id, path);
            return Ok(id);
        }
        let path = disk
            .handles
            .get(&handle)
            .ok_or(TransportError::Failed(1))?
            .clone();
        if op == Operation::Close {
            disk.handles.remove(&handle);
            return Ok(u64::from(disk.bad_close));
        }
        if op == Operation::Write {
            disk.writes += 1;
            if disk.writes == disk.fail_write {
                return Err(TransportError::Failed(0xf006));
            }
            if disk.writes == disk.short_write {
                return Ok(1);
            }
        }
        if op == Operation::Flush && disk.fail_flush {
            return Err(TransportError::Failed(0xf006));
        }
        if op == Operation::Read && disk.oversized_read {
            return Ok(output.len() as u64 + 1);
        }
        if op == Operation::Resize && disk.bad_resize {
            return Ok(1);
        }
        let data = disk.files.get_mut(&path).unwrap();
        match op {
            Operation::Size => Ok(data.len() as u64),
            Operation::Resize => {
                data.resize(pos as usize, 0);
                Ok(0)
            }
            Operation::Flush => Ok(0), // mock completion, NOT durable storage
            Operation::Read => {
                let count = output.len().min(data.len().saturating_sub(pos as usize));
                if count > 0 {
                    output[..count].copy_from_slice(&data[pos as usize..pos as usize + count]);
                }
                Ok(count as u64)
            }
            Operation::Write => {
                let end = pos as usize + input.len();
                if data.len() < end {
                    data.resize(end, 0);
                }
                data[pos as usize..end].copy_from_slice(input);
                Ok(input.len() as u64)
            }
            _ => unreachable!(),
        }
    }
}

fn setup() -> (Arc<NativeIO<Model>>, Arc<Mutex<Disk>>) {
    setup_capacity(TRANSFER_BYTES)
}

fn setup_capacity(capacity: usize) -> (Arc<NativeIO<Model>>, Arc<Mutex<Disk>>) {
    let disk = Arc::new(Mutex::new(Disk::default()));
    (
        Arc::new(NativeIO::new(
            Model(disk.clone(), NonZeroUsize::new(capacity).unwrap()),
            PATHS,
        )),
        disk,
    )
}

#[test]
fn boot_selected_allow_list_is_owned_and_exact() {
    let disk = Arc::new(Mutex::new(Disk::default()));
    let io = {
        let database = String::from("@nvme:0/system-config.sqlite");
        let wal = format!("{database}-wal");
        NativeIO::new(
            Model(disk.clone(), NonZeroUsize::new(TRANSFER_BYTES).unwrap()),
            &[&database, &wal],
        )
    }; // The input strings/array are gone, with no leak or retained borrow.
    for path in [
        "@nvme:0/system-config.sqlite",
        "@nvme:0/system-config.sqlite-wal",
    ] {
        assert!(io.file_id(path).is_ok());
        drop(io.open_file(path, OpenFlags::Create, false).unwrap());
    }
    let before = disk.lock().unwrap().calls;
    for path in [
        "@nvme:0/system-config.sqlite-other",
        "@nvme:0/elsewhere",
        "",
    ] {
        assert!(io.file_id(path).is_err());
        assert!(io.open_file(path, OpenFlags::Create, false).is_err());
    }
    assert_eq!(disk.lock().unwrap().calls, before);
}
fn buffer(size: usize) -> Arc<Buffer> {
    Arc::new(Buffer::new_temporary(size))
}

#[test]
fn transfer_capacity_controls_exact_chunks_and_preserves_bytes() {
    for capacity in [4096, 65536] {
        for length in [1_usize, 4095, 4096, 4097, 65535, 65536, 65537, 131073] {
            let (io, disk) = setup_capacity(capacity);
            let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
            let expected: Vec<_> = (0..length).map(|i| (i % 251) as u8).collect();
            let data = Arc::new(Buffer::new(expected.clone()));
            let done = file
                .pwrite(
                    17,
                    data,
                    Completion::new_write(move |r| {
                        assert_eq!(r.unwrap(), length as i32);
                    }),
                )
                .unwrap();
            assert!(done.succeeded());
            assert_eq!(disk.lock().unwrap().writes, length.div_ceil(capacity));
            let output = buffer(length);
            output.as_mut_slice().fill(255);
            let done = file
                .pread(
                    17,
                    Completion::new_read(output.clone(), move |r| {
                        assert_eq!(r.unwrap().1, length as i32);
                        None
                    }),
                )
                .unwrap();
            assert!(done.succeeded());
            assert_eq!(disk.lock().unwrap().reads, length.div_ceil(capacity));
            assert_eq!(output.as_slice(), expected);
        }
    }
}

#[test]
fn adapter_sql_reopen_and_exclusion() {
    use crate::{Commit, Setting, Store};
    let (io, disk) = setup();
    assert!(io.open_file("ungranted", OpenFlags::Create, false).is_err());
    assert_eq!(disk.lock().unwrap().calls, 0);
    let store = Store::open_with_io(io.clone(), "profile.sqlite").unwrap();
    assert!(
        io.open_file("profile.sqlite", OpenFlags::None, false)
            .is_err()
    );
    assert_eq!(
        store
            .commit(
                "com.cubit.test",
                "test",
                0,
                1,
                &[("answer".into(), Setting::Integer(42))]
            )
            .unwrap(),
        Commit::Saved(1)
    );
    let expected = store.read("com.cubit.test", "test").unwrap();
    store.close().unwrap();
    assert!(disk.lock().unwrap().handles.is_empty());
    let reopened = Store::open_with_io(io, "profile.sqlite").unwrap();
    assert_eq!(reopened.read("com.cubit.test", "test").unwrap(), expected);
    reopened.close().unwrap();
}

#[test]
fn vectors_pack_headers_pages_and_descriptor_boundaries() {
    for (sizes, calls, vectors) in [
        (vec![2048, 2048], 1, 1),
        (vec![24, 4096, 24, 4096], 1, 1),
        (vec![40000, 40000], 2, 1),
        (vec![0, 0], 0, 0),
        (vec![1; 65], 3, 2),
        (vec![65536, 0, 65537], 3, 0),
    ] {
        let (io, disk) = setup();
        let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
        let buffers: Vec<_> = sizes
            .iter()
            .enumerate()
            .map(|(i, size)| Arc::new(Buffer::new(vec![i as u8; *size])))
            .collect();
        let expected: Vec<_> = buffers
            .iter()
            .flat_map(|b| b.as_slice().iter().copied())
            .collect();
        let length = expected.len();
        let done = file
            .pwritev(
                0,
                buffers,
                Completion::new_write(move |r| {
                    assert_eq!(r.unwrap(), length as i32);
                }),
            )
            .unwrap();
        assert!(done.succeeded());
        let disk = disk.lock().unwrap();
        assert_eq!(disk.writes, calls, "sizes={sizes:?}");
        assert_eq!(disk.vectors, vectors, "sizes={sizes:?}");
        assert_eq!(disk.files["workload"], expected);
    }
}

#[test]
fn packed_vector_failures_stop_at_first_failed_batch() {
    for short in [false, true] {
        for boundary in 1..=4 {
            let (io, disk) = setup();
            let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
            if short {
                disk.lock().unwrap().short_write = boundary;
            } else {
                disk.lock().unwrap().fail_write = boundary;
            }
            let completed = Arc::new(AtomicUsize::new(0));
            let seen = completed.clone();
            let done = file
                .pwritev(
                    0,
                    (0..8).map(|_| buffer(TRANSFER_BYTES / 2)).collect(),
                    Completion::new_write(move |r| {
                        assert!(r.is_err());
                        seen.fetch_add(1, Ordering::SeqCst);
                    }),
                )
                .unwrap();
            assert!(done.failed());
            assert_eq!(completed.load(Ordering::SeqCst), 1);
            assert_eq!(disk.lock().unwrap().writes, boundary);
            assert_eq!(disk.lock().unwrap().vectors, boundary);
            assert!(file.size().is_err());
            drop(file);
            assert!(disk.lock().unwrap().handles.is_empty());
        }
    }
}

#[test]
fn vectored_failures_complete_once_and_stop_data_io() {
    for short in [false, true] {
        for boundary in 1..=4 {
            let (io, disk) = setup();
            let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
            {
                let mut disk = disk.lock().unwrap();
                if short {
                    disk.short_write = boundary;
                } else {
                    disk.fail_write = boundary;
                }
            }
            let callbacks = Arc::new(AtomicUsize::new(0));
            let count = callbacks.clone();
            let c = Completion::new_write(move |r| {
                assert!(r.is_err());
                count.fetch_add(1, Ordering::SeqCst);
            });
            let c = file
                .pwritev(
                    0,
                    vec![buffer(2 * TRANSFER_BYTES), buffer(2 * TRANSFER_BYTES)],
                    c,
                )
                .unwrap();
            assert!(c.finished() && c.failed());
            assert_eq!(callbacks.load(Ordering::SeqCst), 1);
            let calls = disk.lock().unwrap().calls;
            assert_eq!(disk.lock().unwrap().writes, boundary);
            assert!(file.size().is_err());
            let later = file
                .sync(
                    Completion::new_sync(|r| assert!(r.is_err())),
                    FileSyncType::Fsync,
                )
                .unwrap();
            assert!(later.failed());
            assert_eq!(disk.lock().unwrap().calls, calls);
            drop(file); // close is allowed solely to release the owned handle
            assert!(disk.lock().unwrap().handles.is_empty());
        }
    }
}

// Yield before every transfer so competing files/threads get an opportunity
// to contend for the shared transport. The disk model's own mutex only protects
// individual modeled calls; it cannot ensure whole multi-chunk operations stay
// together. That serialization must come from NativeIO.
struct YieldingModel(Model);
impl Transport for YieldingModel {
    fn transfer_capacity(&self) -> NonZeroUsize {
        self.0.transfer_capacity()
    }
    fn write_vectored(
        &mut self,
        handle: u64,
        position: u64,
        parts: &[&[u8]],
    ) -> Result<u64, TransportError> {
        std::thread::yield_now();
        self.0.write_vectored(handle, position, parts)
    }
    fn call(
        &mut self,
        op: Operation,
        handle: u64,
        pos: u64,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<u64, TransportError> {
        std::thread::yield_now();
        self.0.call(op, handle, pos, input, output)
    }
}

#[test]
fn concurrent_files_keep_chunk_ownership_and_release_before_callbacks() {
    use std::sync::{Barrier, mpsc};
    use std::time::Duration;

    const THREADS: usize = 8;
    const ROUNDS: usize = 8;
    const CAPACITY: usize = 64;
    const RECORD: usize = 293;
    let disk = Arc::new(Mutex::new(Disk::default()));
    let io = NativeIO::new(
        YieldingModel(Model(disk.clone(), NonZeroUsize::new(CAPACITY).unwrap())),
        PATHS,
    );
    let files = [
        io.open_file("workload", OpenFlags::Create, false).unwrap(),
        io.open_file("profile.sqlite-wal", OpenFlags::Create, false)
            .unwrap(),
    ];
    let gate = Arc::new(Barrier::new(THREADS));
    let callbacks = Arc::new(AtomicUsize::new(0));
    let (done, completed) = mpsc::channel();
    let mut workers = Vec::new();
    for thread in 0..THREADS {
        let file = files[thread % files.len()].clone();
        let gate = gate.clone();
        let callbacks = callbacks.clone();
        let done = done.clone();
        workers.push(std::thread::spawn(move || {
            gate.wait();
            for round in 0..ROUNDS {
                let pos = ((round * THREADS + thread) * RECORD) as u64;
                let expected: Vec<u8> = (0..RECORD)
                    .map(|i| (1 + (i + thread * 17 + round * 31) % 251) as u8)
                    .collect();
                let parts = [0..37, 37..208, 208..RECORD]
                    .into_iter()
                    .map(|range| Arc::new(Buffer::new(expected[range].to_vec())))
                    .collect();
                let reentrant = file.clone();
                let count = callbacks.clone();
                let write = file
                    .pwritev(
                        pos,
                        parts,
                        Completion::new_write(move |r| {
                            assert_eq!(r.unwrap(), RECORD as i32);
                            // Reentry would deadlock if the transport mutex
                            // were retained while delivering this callback.
                            assert!(reentrant.size().unwrap() >= pos + RECORD as u64);
                            count.fetch_add(1, Ordering::SeqCst);
                        }),
                    )
                    .unwrap();
                assert!(write.succeeded());
                let output = buffer(RECORD);
                let observed = output.clone();
                let count = callbacks.clone();
                let read = file
                    .pread(
                        pos,
                        Completion::new_read(output, move |r| {
                            assert_eq!(r.unwrap().1, RECORD as i32);
                            assert_eq!(observed.as_slice(), expected.as_slice());
                            count.fetch_add(1, Ordering::SeqCst);
                            None
                        }),
                    )
                    .unwrap();
                assert!(read.succeeded());
                let count = callbacks.clone();
                let flush = file
                    .sync(
                        Completion::new_sync(move |r| {
                            r.unwrap();
                            count.fetch_add(1, Ordering::SeqCst);
                        }),
                        FileSyncType::Fsync,
                    )
                    .unwrap();
                assert!(flush.succeeded());
            }
            done.send(()).unwrap();
        }));
    }
    drop(done);
    // A regression must fail the test instead of hanging indefinitely inside
    // a reentrant callback. Join only workers that have signaled completion.
    for _ in 0..THREADS {
        completed.recv_timeout(Duration::from_secs(10)).unwrap();
    }
    for worker in workers {
        worker.join().unwrap();
    }
    assert_eq!(callbacks.load(Ordering::SeqCst), THREADS * ROUNDS * 3);
    {
        let disk = disk.lock().unwrap();
        let mut at = 0;
        let mut groups = 0;
        while at < disk.trace.len() {
            let (op, handle, start, _) = disk.trace[at];
            if op != Operation::Write {
                at += 1;
                continue;
            }
            assert_eq!(start % RECORD as u64, 0);
            for offset in (0..RECORD).step_by(CAPACITY) {
                assert_eq!(
                    disk.trace[at],
                    (
                        Operation::Write,
                        handle,
                        start + offset as u64,
                        CAPACITY.min(RECORD - offset),
                    ),
                    "a logical write's chunks must retain exclusive transport ownership",
                );
                at += 1;
            }
            groups += 1;
        }
        assert_eq!(groups, THREADS * ROUNDS);
        assert_eq!(disk.writes, THREADS * ROUNDS * RECORD.div_ceil(CAPACITY));
    }
    drop(files);
    assert!(disk.lock().unwrap().handles.is_empty());
}

#[test]
fn short_reads_ranges_and_reentrant_callbacks() {
    let (io, disk) = setup();
    let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
    assert!(file.lock_file(true).is_ok());
    assert!(file.lock_file(false).is_err());
    assert!(file.unlock_file().is_err());
    let data = buffer(17);
    data.as_mut_slice().fill(5);
    let reentrant = file.clone();
    let completed = file
        .pwrite(
            0,
            data,
            Completion::new_write(move |r| {
                assert_eq!(r.unwrap(), 17);
                assert_eq!(reentrant.size().unwrap(), 17);
            }),
        )
        .unwrap();
    assert!(completed.succeeded());
    let read = buffer(4097);
    read.as_mut_slice().fill(0xa5);
    let c = file
        .pread(
            0,
            Completion::new_read(read.clone(), |r| {
                assert_eq!(r.unwrap().1, 17);
                None
            }),
        )
        .unwrap();
    assert!(c.succeeded());
    assert_eq!(&read.as_slice()[..17], &[5; 17]);
    assert!(read.as_slice()[17..].iter().all(|b| *b == 0));
    let calls = disk.lock().unwrap().calls;
    let c = file
        .pwrite(
            u64::MAX,
            buffer(2),
            Completion::new_write(|r| assert!(r.is_err())),
        )
        .unwrap();
    assert!(c.failed());
    assert_eq!(disk.lock().unwrap().calls, calls);
    assert_eq!(file.size().unwrap(), 17); // admission failure is not uncertain I/O
    disk.lock().unwrap().fail_flush = true;
    let c = file
        .sync(
            Completion::new_sync(|r| assert!(r.is_err())),
            FileSyncType::Fsync,
        )
        .unwrap();
    assert!(c.failed());
}

#[test]
fn uncertain_open_retires_backend_but_definite_denial_does_not() {
    let (io, disk) = setup();
    assert!(io.open_file("workload", OpenFlags::None, false).is_err());
    let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
    disk.lock().unwrap().fail_open = true;
    assert!(
        io.open_file("profile.sqlite", OpenFlags::Create, false)
            .is_err()
    );
    let calls = disk.lock().unwrap().calls;
    assert!(
        io.open_file("profile.sqlite", OpenFlags::Create, false)
            .is_err()
    );
    assert!(file.size().is_err());
    assert_eq!(disk.lock().unwrap().calls, calls);
    drop(file);
    assert!(disk.lock().unwrap().handles.is_empty());
}

#[test]
fn malformed_success_replies_retire_backend() {
    for operation in [Operation::Read, Operation::Resize, Operation::Close] {
        let (io, disk) = setup();
        let file = io.open_file("workload", OpenFlags::Create, false).unwrap();
        match operation {
            Operation::Read => {
                disk.lock().unwrap().oversized_read = true;
                let c = file
                    .pread(
                        0,
                        Completion::new_read(buffer(1), |r| {
                            assert!(r.is_err());
                            None
                        }),
                    )
                    .unwrap();
                assert!(c.failed());
            }
            Operation::Resize => {
                disk.lock().unwrap().bad_resize = true;
                let c = file
                    .truncate(1, Completion::new_trunc(|r| assert!(r.is_err())))
                    .unwrap();
                assert!(c.failed());
            }
            Operation::Close => {
                disk.lock().unwrap().bad_close = true;
            }
            _ => unreachable!(),
        }
        drop(file);
        let calls = disk.lock().unwrap().calls;
        assert!(io.open_file("workload", OpenFlags::None, false).is_err());
        assert_eq!(disk.lock().unwrap().calls, calls);
    }
}
