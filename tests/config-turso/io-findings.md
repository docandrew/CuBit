# Turso-driven filesystem work

Status: measurement foundation and native regression fixtures, not a persistent
Config deployment or an asynchronous filesystem implementation.

## What the existing code actually does

* Kernel asynchronous IPC is available, but that alone does not make storage
  asynchronous. `userspace/services/filesystem/main.adb` handles requests in a
  blocking receive loop. `ext2.adb` waits in `capCall` for block-device replies.
* The NVMe queue has multiple entries, but `nvme.adb` submits a command and waits
  before submitting the next one on this path. Queue capacity is not concurrency.
* NVMe transfers through its DMA buffer, and ext2 transfers through a staging
  grant buffer. The current path has copies; it is not zero-copy just because
  the application hands the filesystem a grant.
* Native flush now reaches NVMe. Unsupported backends reject it. Successful
  flush is not proof of ext2 metadata crash consistency; journaling remains
  separate work.
* The native Turso application currently uses MemoryIO. Its shared workload
  passes inside CuBit, but no native Turso disk throughput has been measured.

These observations are from the checked-out implementation, not assumptions
about what a microkernel or Rust async abstraction ought to provide.

## Measurement boundaries

`run-benchmarks.sh` compares Linux UnixIO and io_uring using the pinned Turso
engine and the same workloads. See the README for warm-cache/batching details.
The raw workload includes benchmark clock/callback bookkeeping; sub-microsecond
results are not a measurement of isolated syscall or device overhead.

`bench-storage` independently exercises real CuBit FS IPC: 512 measured calls
per phase, including a separate write-plus-device-flush phase. It uses an
already-created grant and excludes seek/buffer preparation. Native regression
runs on 2026-09-23 passed with QEMU TCG; those timings are not a Linux comparison.
KVM was unavailable to this environment.

Keep three costs distinct: cached data access, scheduling/IPC/completion, and
durable commit. A faster first cost does not imply a faster third cost. Likewise,
a synchronous fast path can beat queued I/O on cache hits while genuine
concurrency helps outstanding device requests.

## Initial Linux baseline — 2026-09-23

Three runs per backend, alternating order, after our builds and native tests
finished. AMD Ryzen 7 5800X, Linux 7.0.0-31, ext4 on NVMe, Nix Rust 1.98.1,
release O2. No CPU pinning, cache dropping or exclusive host-load control.
Artifacts: ignored `results/io.jWyRkf/` (including full p99 ranges and environment).
Lockfile SHA-256: `7d838f0c1c0ab34ae85e86a93de176cf6c929fbdff1231cb116c442c10e1e9ba`.

All times below are microseconds, median of the three run-level percentiles:

| Operation | UnixIO p50 | UnixIO p99 | io_uring p50 | io_uring p99 |
|---|---:|---:|---:|---:|
| Read/decode 20-setting Config profile | 83.93 | 127.32 | 84.02 | 126.02 |
| FULL-sync Config revision commit | 3487.43 | 6613.44 | 3524.87 | 6580.55 |
| Warm 4 KiB random read, depth 1 | 0.77 | 1.08 | 1.58 | 1.93 |
| Buffered 4 KiB overwrite, depth 1 | 1.25 | 1.39 | 8.27 | 12.18 |
| 4 KiB overwrite plus flush, depth 1 | 3171.79 | 5820.79 | 3197.24 | 5787.80 |

io_uring 4 KiB overwrites rose from about 452 to 1844 active-phase MiB/s
between batch depths 1 and 32, while median request latency rose from 8.27 to
37.29 µs. At depth 32, the observed pending-completion peak was 32 for io_uring
and zero for UnixIO. Even batched, io_uring did not beat UnixIO in this cached
overwrite fixture. These are neither sustained disk rates nor evidence that
asynchronous I/O is unhelpful on actual cache misses or concurrent workloads.

Interpretation: durable Config commits remain dominated by a millisecond-scale
path; changing the submission mechanism alone did not transform them. The
cached raw path favors synchronous operations here. We should preserve a cheap
immediate-completion path while enabling real downstream concurrency, then
measure both latency and throughput. The small differences between SQL results
are not a statistically established backend advantage.

## Next implementation sequence

1. **Positioned file operations — implemented first stage.** Read/write-at-offset
   now use existing authorized handles without changing the shared seek cursor.
   See [wire contract and tests](../../docs/filesystem-positioned-io.md) for
   partial completion, overflow and grant semantics. The engine remains
   serialized; this step does not itself add downstream concurrency.
2. **Metadata coherence and bounded request ownership.** The shared open-inode
   table now fixes successful-path coherence across regular-file handles;
   [tests and proof scope](../shared-file-objects/README.md) are explicit.
   Checked truncate now detaches and flushes before reclamation; uncertain
   truncate/pointer/inode publication quarantines writes and retires affected
   handle aliases. See [fault tests and limitations](../filesystem-truncate/README.md).
   Allocation and regular-file creation now check metadata publication and stop
   on uncertain writes, with 351 production-code injected-failure cases across
   this path and truncation. File-block lookup now checks indirect reads and
   cache publication; 75 injected read/reply cases plus cache eviction/remount
   tests cover that path. Checked inode/path resolution adds 165 failure cases.
   Typed device admission now distinguishes explicit absence from device/metadata
   failure, with 55 hosted admission scenarios plus invalid-session checks.
   General crash ordering and persistent recovery still need hardening before
   concurrent mutations.
   Use existing async IPC and grants, with
   explicit request identity/generation and a completion state machine. Reject
   queue exhaustion before acceptance. An accepted operation retains its file
   and buffer lifetime until terminal completion; closing a handle cannot
   retarget an in-flight request to a reused slot. Cancellation requests are
   not proof that DMA stopped. Test stale completions and process teardown.
3. **Actual downstream concurrency.** Separate FS admission from completion
   dispatch, then allow multiple NVMe requests with separately owned DMA
   storage and command IDs. Preserve overlapping-write and flush ordering;
   don't parallelize mutable ext2 metadata blindly. Measure queue occupancy,
   copies, wakeups and CPU cost to locate bottlenecks before removing buffers.
4. **Native Turso File adapter.** Bind to scoped filesystem authority, not
   ambient paths. Implement read/write-at, reliable vectored completion,
   truncate, size, meaningful flush and exclusive database ownership. No fake
   sync/lock success, POSIX emulation or background policy bypass. Run the
   same raw workload and existing transaction-error fixtures through it.
5. **Fair comparison.** First compare correctness and counters, then repeated
   native/KVM measurements with matched data, cache state, synchronization and
   queue depth. Include sequential/random and mixed traffic, large working
   sets, background load, latency distributions, CPU cost and sustained
   end-to-end throughput. For physical-disk comparisons use equivalent device
   conditions, not a warm host cache versus an emulated guest disk.

Authority checks remain on handle/grant admission and required lifetime
transitions; fast paths do not manufacture broader authority. These contracts
should be tested before concurrency is widened, with SPARK proofs for bounded
state/accounting logic where feasible. Rust/Turso, DMA ordering, hardware flush
behavior and crash recovery are not proved by these regression tests.

A claimed 1–2% advantage needs much stronger experimental control and
repeatability than a few workstation runs. Nothing here establishes that CuBit
beats Linux; it establishes a way to find out without weakening security.

## Data-only overwrite request reduction — 2026-09-23

Measured the production Ext2 code against the same hosted sector-device fixture
before and after skipping unchanged-inode publication. This is an exact request-
count comparison, not a simulated timing claim or a Turso disk benchmark.

| Workload | Before block calls | After block calls |
|---|---:|---:|
| 4 KiB overwrite, 1 KiB filesystem blocks | 7 (2 reads, 5 writes) | 4 writes |
| 4 KiB overwrite, 4 KiB filesystem blocks | 4 (2 reads, 2 writes) | 1 write |
| 7-byte overwrite spanning two 512-byte sectors, either geometry | 7 (4 reads, 3 writes) | 4 (2 reads, 2 writes) |

Previously every nonempty write published the inode even when its entire value
was unchanged: a group-descriptor read followed by inode-sector read/modify/write.
Now a bounded, typed 128-byte inode comparison avoids those three requests.
No metadata cache, deferred writeback, omitted durability barrier, wider authority,
or new concurrency is involved. Future changed timestamps are included in the
comparison automatically; CuBit does not currently maintain those on every write.

Growth and sparse-hole mapping changes still publish metadata. Existing quarantine
behavior is retained for uncertain metadata writes. 195 injected overwrite failures
cover error/short/malformed replies before, during and after I/O, with exact
completed-prefix and no-follow-on-I/O checks. This exposed missing flags/reserved
validation in write/RMW replies, now checked consistently with reads.

The new hosted `tests/filesystem-truncate/build/overwrites` test verifies the entire
image outside the intended data range remains unchanged on successful overwrites.
It also covers read-only, quarantined and empty writes, EOF extension into already
allocated storage, and sparse-hole mapping publication. This does not establish
full sparse-file allocation accounting or crash consistency.

After the change, both native four-vCPU TCG `storage-grants` and `bench-storage`
passed, including multi-handle coherence and the separate overwrite-plus-flush
phase. No KVM or physical-device comparison was available in this session.

Remaining obvious amplification: existing-data writes still split at each Ext2
block even when mappings are contiguous. Reads already coalesce contiguous blocks.
Measure bounded write coalescing next, retaining safe partial-failure accounting
and avoiding speculative allocation. End-to-end loan/DMA copies and actual queue
concurrency remain larger subsequent projects.
