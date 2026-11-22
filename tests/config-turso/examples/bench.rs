//! Linux-hosted baseline. Backends are explicit; never silently fall back.
use cubit_config_turso_spike::{
    Commit, Result, Setting, Store,
    io_workload::{self, Operation},
};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};
use turso_core::{IO, OpenFlags, PlatformIO};

fn report(
    backend: &str,
    name: &str,
    bytes: usize,
    depth: usize,
    samples: &mut [u128],
    elapsed: u128,
    deferred: usize,
) {
    samples.sort_unstable();
    let deferred = if bytes == 0 {
        "na".to_string()
    } else {
        deferred.to_string()
    };
    let at = |percent: usize| samples[(samples.len() * percent).div_ceil(100) - 1];
    println!(
        "MEASURE backend={backend} operation={name} bytes={bytes} depth={depth} n={} p50_ns={} p95_ns={} p99_ns={} max_ns={} timed_ns={elapsed} peak_deferred={deferred}",
        samples.len(),
        at(50),
        at(95),
        at(99),
        samples.last().unwrap()
    );
}
fn new_file(path: &Path) -> Result<()> {
    // The parent directory is exclusively created by this executable.
    drop(
        fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)?,
    );
    Ok(())
}
fn backend(name: &str) -> Result<Arc<dyn IO>> {
    match name {
        "syscall" => Ok(Arc::new(PlatformIO::new()?)),
        #[cfg(all(feature = "linux-uring", target_os = "linux"))]
        "io-uring" => Ok(Arc::new(turso_core::UringIO::new()?)),
        _ => Err(
            "backend unavailable: use syscall, or build --features linux-uring for io-uring".into(),
        ),
    }
}
fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 3 {
        return Err("usage: bench {syscall|io-uring} NEW_RUN_DIRECTORY".into());
    }
    let name = &args[1];
    if std::env::var_os("TESTING").is_some()
        || std::env::var_os("LIMBO_DISABLE_FILE_LOCK").is_some()
    {
        return Err(
            "remove TESTING/LIMBO_DISABLE_FILE_LOCK overrides for a comparable baseline".into(),
        );
    }
    // Denied io_uring is an error, not a mislabeled syscall fallback.
    let io = backend(name)?;
    let directory = PathBuf::from(&args[2]);
    fs::create_dir(&directory)?;
    println!(
        "BENCH version=2 platform=linux backend={name} engine=0.8.0-pre.12 build=O2 storage=buffered cache=warm synchronization=FULL raw_buffers=heap-unregistered batches=fixed"
    );
    let path = directory.join("config.sqlite");
    new_file(&path)?;
    let start = Instant::now();
    let store = Store::open_with_io(io.clone(), path.to_str().ok_or("non-UTF8 path")?)?;
    println!("SETUP schema_ns={}", start.elapsed().as_nanos());
    let mut entries: Vec<_> = (0..20)
        .map(|i| (format!("setting{i:02}"), Setting::Text("c".repeat(64))))
        .collect();
    let mut writes = Vec::new();
    let mut reads = Vec::new();
    for revision in 0..1032 {
        entries[0].1 = Setting::Integer(revision);
        let start = Instant::now();
        let result = store.commit("com.cubit.demo.v1", "desk", revision, 1, &entries)?;
        let elapsed = start.elapsed().as_nanos();
        assert_eq!(result, Commit::Saved(revision + 1));
        if revision >= 32 {
            writes.push(elapsed);
        }
        for _ in 0..5 {
            let start = Instant::now();
            let snapshot = store.read("com.cubit.demo.v1", "desk")?.unwrap();
            let elapsed = start.elapsed().as_nanos();
            assert_eq!(snapshot.entries, entries);
            if revision >= 32 {
                reads.push(elapsed);
            }
        }
    }
    let write_total = writes.iter().sum();
    let read_total = reads.iter().sum();
    report(
        name,
        "config-commit-full-sync",
        0,
        1,
        &mut writes,
        write_total,
        0,
    );
    report(name, "config-read-warm", 0, 1, &mut reads, read_total, 0);
    let start = Instant::now();
    store.checkpoint()?;
    println!("CHECKPOINT truncate_ns={}", start.elapsed().as_nanos());
    store.close()?;
    let reopened = Store::open_with_io(io.clone(), path.to_str().unwrap())?;
    let snapshot = reopened.read("com.cubit.demo.v1", "desk")?.unwrap();
    assert_eq!(snapshot.revision, 1032);
    assert_eq!(snapshot.entries, entries);
    reopened.close()?;
    println!(
        "DATABASE bytes={} revisions=1032",
        fs::metadata(&path)?.len()
    );

    for bytes in [4096, 65536] {
        let path = directory.join(format!("raw-{bytes}.dat"));
        new_file(&path)?;
        let file = io.open_file(path.to_str().unwrap(), OpenFlags::Create, false)?;
        let blocks = 4 * 1024 * 1024 / bytes;
        io_workload::initialize(io.as_ref(), file.as_ref(), bytes, blocks)?;
        for operation in [
            Operation::SequentialRead,
            Operation::RandomRead,
            Operation::Overwrite,
            Operation::VectoredWrite,
            Operation::WriteThenFlush,
        ] {
            for depth in [1, 8, 32] {
                if matches!(operation, Operation::WriteThenFlush) && depth != 1 {
                    continue;
                }
                let mut m = io_workload::measure(
                    io.as_ref(),
                    file.as_ref(),
                    operation,
                    bytes,
                    blocks,
                    depth,
                    1024 / depth,
                )?;
                report(
                    name,
                    operation.name(),
                    bytes,
                    depth,
                    &mut m.latencies_ns,
                    m.elapsed_ns,
                    m.peak_deferred,
                );
            }
        }
    }
    println!("BENCH PASS backend={name}");
    Ok(())
}
