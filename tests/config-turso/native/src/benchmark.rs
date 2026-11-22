//! Real CuBit filesystem IPC timings, opt-in on the disposable probe disk.
//! This is NOT a Linux comparison, device-only timing, or a zero-copy claim.
use config_storage::io_workload::{self, Operation};
use turso_core::{File, IO};

unsafe extern "C" {
    fn cubit_probe_clock_calibrate() -> u64;
    fn cubit_probe_clock_counter() -> u64;
}

fn counter() -> u128 {
    // SAFETY: scalar-only, local serialized TSC read; Ada library initialized.
    unsafe { cubit_probe_clock_counter() as u128 }
}

pub fn run(io: &dyn IO, file: &dyn File, transfer_bytes: usize) {
    // SAFETY: the benchmark owns this thread; calibration uses existing guest
    // GETTIME/SLEEP instrumentation before the timed workload, not per sample.
    let rate = unsafe { cubit_probe_clock_calibrate() };
    assert_ne!(rate, 0, "native benchmark clock calibration failed");
    let origin = counter();
    let now = move || {
        counter()
            .checked_sub(origin)
            .expect("native counter regressed")
            * 1_000_000
            / rate as u128
    };
    cubit::debug_write(&format!(
        "TURSO-BENCH: START clock=calibrated-tsc ticks_per_ms={rate} synchronization=explicit-flush transport=serial-grant transfer_bytes={transfer_bytes} vector_layout=packed cross_cpu_tsc=assumed\n"
    ));
    for bytes in [4096, 65536] {
        io_workload::initialize(io, file, bytes, 32).unwrap();
        for operation in [
            Operation::SequentialRead,
            Operation::RandomRead,
            Operation::Overwrite,
            Operation::VectoredWrite,
            Operation::WriteThenFlush,
        ] {
            for depth in [1, 8] {
                if matches!(operation, Operation::WriteThenFlush) && depth != 1 {
                    continue;
                }
                let mut m = io_workload::measure_with_clock(
                    io,
                    file,
                    operation,
                    bytes,
                    32,
                    depth,
                    64 / depth,
                    now,
                )
                .unwrap();
                assert_eq!(m.latencies_ns.len(), 64);
                // The baseline adapter completes inside File calls; declared
                // batch depth must not be mistaken for device concurrency.
                assert_eq!(m.peak_deferred, 0);
                m.latencies_ns.sort_unstable();
                let at = |p: usize| m.latencies_ns[(64 * p).div_ceil(100) - 1];
                // No debug/serial output within measured intervals.
                cubit::debug_write(&format!(
                    "MEASURE backend=cubit-native operation={} bytes={bytes} depth={depth} n=64 p50_ns={} p95_ns={} p99_ns={} max_ns={} timed_ns={} peak_deferred={}\n",
                    operation.name(),
                    at(50),
                    at(95),
                    at(99),
                    m.latencies_ns[63],
                    m.elapsed_ns,
                    m.peak_deferred,
                ));
            }
        }
    }
    cubit::debug_write("TURSO-BENCH: PASS\n");
}
