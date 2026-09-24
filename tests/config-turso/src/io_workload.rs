//! Backend-independent Turso File workload. No paths are authorized here:
//! callers supply their own file and I/O device. Not an O_DIRECT benchmark.
use crate::Result;
use std::{
    sync::{Arc, Mutex},
    time::Instant,
};
use turso_core::io::FileSyncType;
use turso_core::{Buffer, Completion, File, IO};

#[derive(Clone, Copy, Debug)]
pub enum Operation {
    SequentialRead,
    RandomRead,
    Overwrite,
    WriteThenFlush,
    VectoredWrite,
}
impl Operation {
    pub fn name(self) -> &'static str {
        match self {
            Self::SequentialRead => "read-sequential",
            Self::RandomRead => "read-random",
            Self::Overwrite => "overwrite",
            Self::WriteThenFlush => "write-then-flush",
            Self::VectoredWrite => "write-vectored",
        }
    }
}
#[derive(Debug)]
pub struct Measurement {
    pub operation: Operation,
    pub bytes: usize,
    pub depth: usize,
    pub latencies_ns: Vec<u128>,
    // Timed batch intervals only: excludes preparation and validation.
    pub elapsed_ns: u128,
    // Deferred operations observed after submission, NOT device queue depth.
    pub peak_deferred: usize,
}

fn pattern(block: usize) -> u8 {
    (block % 251) as u8
}

fn wait(io: &dyn IO, c: Completion) -> Result<()> {
    io.wait_for_completion(c.clone())?;
    if !c.succeeded() {
        return Err("unsuccessful I/O completion".into());
    }
    Ok(())
}

fn verify(io: &dyn IO, file: &dyn File, bytes: usize, expected: &[u8]) -> Result<()> {
    let buffer = Arc::new(Buffer::new(vec![0; bytes]));
    for (block, value) in expected.iter().enumerate() {
        buffer.as_mut_slice().fill(255);
        let c = Completion::new_read(buffer.clone(), move |r| {
            assert_eq!(r.unwrap().1, bytes as i32);
            None
        });
        wait(io, file.pread((block * bytes) as u64, c)?)?;
        if buffer.as_slice().iter().any(|b| b != value) {
            return Err("fixture content mismatch".into());
        }
    }
    Ok(())
}

/// Fully initialize and verify a caller-owned scratch file before measuring.
pub fn initialize(io: &dyn IO, file: &dyn File, bytes: usize, blocks: usize) -> Result<()> {
    if !matches!(bytes, 4096 | 65536) || !(32..=1024).contains(&blocks) {
        return Err("invalid fixture dimensions".into());
    }
    for block in 0..blocks {
        let buffer = Arc::new(Buffer::new(vec![pattern(block); bytes]));
        let c = Completion::new_write(move |r| {
            assert_eq!(r.unwrap(), bytes as i32);
        });
        wait(io, file.pwrite((block * bytes) as u64, buffer, c)?)?;
    }
    wait(
        io,
        file.sync(
            Completion::new_sync(|r| {
                assert_eq!(r.unwrap(), 0);
            }),
            FileSyncType::Fsync,
        )?,
    )?;
    if file.size()? != (bytes * blocks) as u64 {
        return Err("unexpected scratch file size".into());
    }
    verify(
        io,
        file,
        bytes,
        &(0..blocks).map(pattern).collect::<Vec<_>>(),
    )
}

/// Fixed batches, bounded reusable buffers, exact byte/content checks.
/// The caller chooses whether the backend's flush is durable or volatile.
pub fn measure(
    io: &dyn IO,
    file: &dyn File,
    operation: Operation,
    bytes: usize,
    blocks: usize,
    depth: usize,
    batches: usize,
) -> Result<Measurement> {
    if !matches!(bytes, 4096 | 65536)
        || !matches!(depth, 1 | 8 | 32)
        || !(32..=1024).contains(&blocks)
        || !(1..=4096).contains(&batches)
        || (matches!(operation, Operation::WriteThenFlush) && depth != 1)
    {
        return Err("invalid workload dimensions".into());
    }
    let writing = matches!(
        operation,
        Operation::Overwrite | Operation::WriteThenFlush | Operation::VectoredWrite
    );
    // Reset before write phases (outside timing). A successful no-op write
    // must not pass verification just because the old bytes already matched.
    if writing {
        initialize(io, file, bytes, blocks)?;
    }
    let mut expected: Vec<_> = (0..blocks).map(pattern).collect();
    let buffers: Vec<_> = (0..depth)
        .map(|_| Arc::new(Buffer::new(vec![0; bytes])))
        .collect();
    let mut measurement = Measurement {
        operation,
        bytes,
        depth,
        latencies_ns: Vec::with_capacity(depth * batches),
        elapsed_ns: 0,
        peak_deferred: 0,
    };
    let mut seed = 17_u32;
    // Warmups execute the same operation, but aren't included in measurements.
    for batch in 0..batches + 2 {
        let mut positions = Vec::with_capacity(depth);
        for (slot, buffer) in buffers.iter().enumerate() {
            seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
            let block = if matches!(operation, Operation::RandomRead) {
                (seed >> 16) as usize % blocks
            } else {
                (batch * depth + slot) % blocks
            };
            positions.push(block);
            if writing {
                expected[block] = (expected[block] + 1) % 251;
            }
            // No previous request remains active when these buffers are reused.
            buffer.as_mut_slice().fill(
                if matches!(operation, Operation::SequentialRead | Operation::RandomRead) {
                    255
                } else {
                    expected[block]
                },
            );
        }
        let outcomes = Arc::new(Mutex::new(vec![None; depth]));
        let mut pending = Vec::with_capacity(depth);
        let mut error = None;
        // Prepare vector payload before timing; this explicitly measures a
        // two-segment write, not concatenation/copy cost in the benchmark.
        let vectors: Vec<_> = if matches!(operation, Operation::VectoredWrite) {
            buffers
                .iter()
                .map(|b| {
                    vec![
                        Arc::new(Buffer::new(b.as_slice()[..bytes / 2].to_vec())),
                        Arc::new(Buffer::new(b.as_slice()[bytes / 2..].to_vec())),
                    ]
                })
                .collect()
        } else {
            Vec::new()
        };
        let batch_start = Instant::now();
        for slot in 0..depth {
            let start = Instant::now();
            let results = outcomes.clone();
            let pos = (positions[slot] * bytes) as u64;
            let submitted = match operation {
                Operation::SequentialRead | Operation::RandomRead => {
                    let c = Completion::new_read(buffers[slot].clone(), move |r| {
                        let ok = r.is_ok_and(|(_, n)| n == bytes as i32);
                        results.lock().unwrap()[slot] = Some((start.elapsed().as_nanos(), ok));
                        None
                    });
                    file.pread(pos, c)
                }
                _ => {
                    let c = Completion::new_write(move |r| {
                        let ok = r.is_ok_and(|n| n == bytes as i32);
                        results.lock().unwrap()[slot] = Some((start.elapsed().as_nanos(), ok));
                    });
                    if matches!(operation, Operation::VectoredWrite) {
                        file.pwritev(pos, vectors[slot].clone(), c)
                    } else {
                        file.pwrite(pos, buffers[slot].clone(), c)
                    }
                }
            };
            match submitted {
                Ok(c) => pending.push(c),
                Err(e) => {
                    error = Some(e);
                    break;
                }
            }
            if batch >= 2 {
                measurement.peak_deferred = measurement
                    .peak_deferred
                    .max(pending.iter().filter(|c| !c.finished()).count());
            }
        }
        // Drain accepted operations even if a later submission failed. Never
        // recycle a read buffer merely because another request was rejected.
        io.drain_completions(&pending)?;
        if let Some(e) = error {
            return Err(e.into());
        }
        if pending.iter().any(|c| !c.succeeded()) {
            return Err("I/O completion failed".into());
        }
        if matches!(operation, Operation::WriteThenFlush) {
            wait(
                io,
                file.sync(Completion::new_sync(|_| {}), FileSyncType::Fsync)?,
            )?;
        }
        let elapsed = batch_start.elapsed().as_nanos();
        let outcomes = outcomes.lock().unwrap();
        for slot in 0..depth {
            let (latency, ok) = outcomes[slot].ok_or("missing completion callback")?;
            if !ok {
                return Err("short/failed I/O completion".into());
            }
            if matches!(operation, Operation::SequentialRead | Operation::RandomRead)
                && buffers[slot]
                    .as_slice()
                    .iter()
                    .any(|b| *b != pattern(positions[slot]))
            {
                return Err("read content mismatch".into());
            }
            if batch >= 2 {
                measurement
                    .latencies_ns
                    .push(if matches!(operation, Operation::WriteThenFlush) {
                        elapsed
                    } else {
                        latency
                    });
            }
        }
        if batch >= 2 {
            measurement.elapsed_ns += elapsed;
        }
    }
    if matches!(
        operation,
        Operation::Overwrite | Operation::WriteThenFlush | Operation::VectoredWrite
    ) {
        verify(io, file, bytes, &expected)?;
    }
    Ok(measurement)
}

#[cfg(test)]
mod tests {
    use super::*;
    use turso_core::{MemoryIO, OpenFlags};
    #[test]
    fn memory_workload_checks_contents_and_sample_counts() -> Result<()> {
        let io = MemoryIO::new();
        let file = io.open_file("fixture", OpenFlags::Create, false)?;
        initialize(&io, file.as_ref(), 4096, 32)?;
        for operation in [
            Operation::SequentialRead,
            Operation::RandomRead,
            Operation::Overwrite,
            Operation::WriteThenFlush,
            Operation::VectoredWrite,
        ] {
            for depth in [1, 8, 32] {
                if matches!(operation, Operation::WriteThenFlush) && depth != 1 {
                    continue;
                }
                let m = measure(&io, file.as_ref(), operation, 4096, 32, depth, 2)?;
                assert_eq!(m.latencies_ns.len(), 2 * depth);
                assert_eq!(m.peak_deferred, 0);
            }
        }
        assert!(measure(&io, file.as_ref(), Operation::Overwrite, 0, 32, 1, 1).is_err());
        // Verification must detect corruption, not turn it into a timing.
        wait(
            &io,
            file.pwrite(
                0,
                Arc::new(Buffer::new(vec![254; 4096])),
                Completion::new_write(|_| {}),
            )?,
        )?;
        assert!(
            measure(
                &io,
                file.as_ref(),
                Operation::SequentialRead,
                4096,
                32,
                32,
                1
            )
            .is_err()
        );
        Ok(())
    }
}
