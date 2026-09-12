# Native SMP spinlock comparison

This is a **CuBit kernel benchmark in KVM**, not a Linux emulation of IRQ
masking. Linux only builds, pins vCPU threads, runs QEMU and reads reports.

```sh
nix develop -c python3 -m unittest discover -s tests/spinlock-bench -p test_run.py
nix develop -c python3 tests/spinlock-bench/run.py --host-cpus 2,3,4,5
```

Requires a previously built CuBit runtime and `kernel/isodir/boot/initrd.img`,
KVM access, and four allowed host CPU numbers. Inspect `lscpu -e` and select
different physical cores, not SMT siblings. Pinning is not host CPU isolation.
Run sequentially with other kernel builds/headless tests. The script first
refreshes the ordinary kernel build, then stages two independent test trees
under ignored `build/experiment-*` directories. It never edits production
sources or replaces the normal desktop ISO. Keep the printed experiment path.

The baseline is the current production `Spinlocks` implementation. The other
tree changes **only its final unlock CAS** into an aligned 32-bit `movl` with a
compiler memory clobber. The owner check, acquisition CAS, read-spin/PAUSE,
shootdown service and nested IRQ exclusion remain identical. Source injection
checks its exact anchor once and fails if production code has drifted. Source
hashes and disassemblies are recorded. No release-store change is enabled in
the normal kernel by this experiment.

The test-only boot hooks run after per-CPU setup and TLB registration, before
the schedulers start. Each of four CPUs executes the production spinlock
against either a private cache-line-separated lock or one shared lock. The
payload is a non-atomic counter plus its complement, checked while owned;
each CPU performs 50,000 updates per phase. Final totals, payload consistency,
CPU ownership, nested IRQ depth and sample counts must all match. Test
barriers continue servicing TLB shootdowns while waiting. Result consumption
uses monotonic phase acknowledgments before a worker may reuse its storage.

Three repetitions of private/shared locks and throughput/acquisition sampling
run per boot. The run order is **baseline, candidate, candidate, baseline**:
six rounds per variant/workload/measurement, 9.6 million updates overall.
QEMU starts paused; all four affinities must be verified before resume. The
guest exits through a test-only debug-exit device after its final PASS. A
timeout, missing CPU result, incorrect total or malformed report fails the run.
The normal kernel gets no benchmark syscall, runtime switch, hook or device.

## What the numbers mean

- Throughput phases have no per-acquisition timestamps/histogram updates.
  Their elapsed time spans the earliest worker start to the last completion.
  `aggregate ticks/op` is that interval divided by **all four workers'** updates:
  it is reciprocal aggregate throughput, not the duration of a single lock call.
- Acquisition phases use ordered TSC reads around `enterCriticalSection`.
  These include timestamp/acquisition overhead even if uncontended, and the
  second timestamp adds work while holding the lock. Histogram insertion is
  outside the lock. Sampling phases are separate from throughput measurements.
- Reported p99s are per-worker histogram upper bounds; their median/worst are
  **not a pooled p99**. The median of several bucket bounds can lie between
  bucket boundaries. Maximum is the largest observed individual sample.
- IRQs remain disabled for the whole microbenchmark; the real lock still
  executes nested push/pop exclusion. This isolates primitive mechanics and
  does not measure the outermost STI path, scheduling, application response,
  input latency, system-wide contention frequency or real-world OS speedup.
- Host scheduling/interrupts/SMT neighbors can still produce outliers. Compare
  raw TSC deltas on the same host; these are TSC ticks, not core clock cycles.
- Trace instrumentation is disabled. The full linked kernel is otherwise
  compiled with the normal optimized/checks-suppressed flags. No `-gnata` is
  enabled in either native kernel.

Both staged variants also run the existing **Linux-hosted** locking suite,
with host-only assertions enabled, using their actual staged spinlock body.
This covers foreign release, nested exclusion, concurrent visibility and the
production queue/lifetime regressions. These are tests, not a concurrent
memory-model proof; the production SPARK ownership policy is unchanged.

## Why this is still an experiment

The aligned store has release ordering for ordinary write-back RAM on x86,
with the assembly memory clobber preventing compiler movement. It does not
provide the same full ordering as the previous locked instruction. Any
callers relying on unlock as a WC/non-temporal/device-publication fence or
other full-barrier dependency must be audited before a global replacement.
Memory-type and ordering details are covered by Intel's
[system-programming manual](https://cdrdv2-public.intel.com/835754/253668-sdm-vol-3a.pdf).
The lock must remain naturally aligned, owner-only and non-copyable in use;
MMIO completion is a separate protocol, not something to infer from a mutex.

See [initial results](../performance/results/2026-09-11-spinlock-release-store.md).
