# Turso-driven filesystem work

Status (2026-09-25): native typed Config publication/recovery is implemented
and regression-tested; desktop defaults still use volatile byte settings.
The native Turso File adapter remains synchronous and copy-based. The dated
sections below preserve historical checkpoints, not current integration status.

## NVMe waiting and acknowledged pointer-cache contents — 2026-09-25

Further attribution now reaches the real userspace NVMe driver. Default builds
contain no new counters or timing reads; `CUBIT_NVME_IO_PROFILE=on` enables a
diagnostic build. Every 64 device flushes it reports read/write/flush command
counts, wait ticks, exhausted spin budgets and sleep calls. Reproduction:
`tests/config-turso/native/run-nvme-profile.sh` under Nix and build.lock. Its
exit cleanup rebuilds ordinary driver/boot artifacts and the normal Turso probe.

The measurement confirms that the 65,536-iteration spin budget frequently
expires and is followed by a 1 ms sleep. A comparable second 64-flush interval
before/after retaining acknowledged indirect-pointer blocks has:

| Command count | Discard after write | Retain acknowledged contents |
|---|---:|---:|
| Read | 6,565 | 4,976 |
| Write | 4,151 | 4,151 |
| Flush | 64 | 64 |

That is 1,589 fewer reads (24.2%), with no removed writes or barriers. The
existing read cache now receives the complete, successfully written single or
double-indirect block contents. Failed pointer publication still invalidates
all entries and quarantines mutation. This is not a write-back cache. The
hosted production-code tests verify immediate payload-only reads, endpoint
isolation, and independent raw-disk read-back after injected failures without
resetting the caches. No new whole-filesystem/cache SPARK-proof claim.

These two diagnostic runs **do not demonstrate an elapsed-time speedup**:
Store commit p50 was 21.8 ms before and 51.5 ms after, and the second interval
had 1,062 vs 3,780 actual sleep calls. Scheduling/completion timing variation
dominates. Neither the 64-flush intervals nor their totals exactly cover the
129 timed SQL commits (the first interval includes boot and the final tail is
not printed). Do not subtract NVMe totals from SQL transport totals. Each
diagnostic run passes independent exact-129-revision SQLite and e2fsck checks.
Raw baseline `/tmp/cubit-nvme-wait-profile/` uses numeric kind labels in the
initial experimental output; the final named-label format and strict parser
are exercised by `/tmp/cubit-pointer-warm-profile/` and
`/tmp/cubit-pointer-warm-waits.md`. New parser tests reject malformed/truncated
reports. Ordinary, uninstrumented public Config runs are reported separately.

Three **uninstrumented public Config** runs then passed exact native replies,
SQLite/WAL revision history and e2fsck. Median of run-level percentiles, in ms,
against the previous three-run coalesced-descriptor baseline:

| Public operation | Previous p50 | Warm-pointer p50 | Previous p99 | Warm-pointer p99 |
|---|---:|---:|---:|---:|
| Cached Get | 0.059 | 0.058 | 0.094 | 0.085 |
| Committed Set | 61.096 | 31.150 | 158.645 | 126.492 |
| Get with Set outstanding | 0.134 | 0.133 | 0.177 | 0.191 |
| That overlapping Set | 43.972 | 48.488 | 157.914 | 161.673 |

All 192 overlapping reads completed before write replies. This is **mixed**
timing evidence: committed Set improved, overlapping Set did not. Runs remain
unpinned four-vCPU KVM, with uncontrolled host load and calibrated TSC assumed
consistent across vCPUs. There are only 64 samples per phase (p99 is its max),
no Linux/hardware comparison and no latency guarantee. Full logs, source hashes
and report: `/tmp/cubit-pointer-warm-public/`. No NVMe diagnostic lines appear
in these runs; the default driver object contains no diagnostic marker.

Validation also includes all 13 hosted filesystem executables, the new
post-fault raw-disk/cache comparisons, 45 Rust tests, six storage-oracle tests,
SQL and NVMe profile-parser tests and the Ada/CBOR cross-check. Existing SPARK
accounting/path/mapping analysis remains **87 checks, zero unproved** (not a
cache or NVMe proof). Logs: `/tmp/cubit-pointer-warm-{suite,cache-faults,proof,rust-tests}.log`.
Native `storage-grants` also passes (`/tmp/cubit-pointer-warm-storage.{serial,log}`).
Normal NVMe/probe/initrd/ISO artifacts were rebuilt after profiling; the normal
NVMe object has no diagnostic marker. Build log:
`/tmp/cubit-pointer-warm-final-build.log`. Shared build lock released.

### Next interrupt-driven storage step

`createIOQueues` sets command dword 11 to 1: PC=1, **IEN=0**. The old comment
incorrectly said interrupts were enabled; corrected without changing behavior.
IEN is bit 1 in the [NVMe base specification, Create I/O Completion Queue](https://nvmexpress.org/wp-content/uploads/NVM-Express-Base-Specification-Revision-2.1-2024.08.05-Ratified.pdf).

Merely setting that bit and replacing Sleep with the existing all-activity wait
is not sufficient. Unconsumed service requests also make that wait return
immediately, which can turn a pending I/O into a busy loop. The clean next step
is a bounded single-outstanding-command driver state machine that drains
requests/events, saves the exact reply slot (not only caller PID), retains loan
ownership, validates CQ phase/CID/status, and atomically sleeps until work or
its absolute deadline. New requests while busy need explicit bounded admission
or a busy reply. Uncertain DMA timeout must still quarantine its buffer.

Interrupt bring-up also needs checked MSI/MSI-X configuration and routing, not
blind reuse of the existing legacy shared IRQ setup. Devmgr/kernel ownership
must be coordinated with the other agent before changes. None of that routing
or scheduler logic is changed by this round. Longer busy-spinning is not being
shipped as a substitute for the event-driven design. Allocation reservation
batching remains a separate, measured next opportunity; preserve exact failure
and publication semantics before batching any metadata writes.

## Commit attribution and descriptor read coalescing — 2026-09-25

The opt-in native `sql-bench` profile measures the production Store library
through the real CuBit filesystem transport, separately from public Config
IPC. All 129 commits use declared native-object storage, check a read-back, and
are verified independently from the closed SQLite file and ext2 image. The
transport wrapper only exists with the `measure` feature or hosted tests;
the production Config worker retains no metrics overhead.

Baseline: **99.83% of measured commit time was inside filesystem calls**.
129 transactions issued exactly 129 packed writes (1,648,000 bytes) and 129 flushes,
with **zero reads, opens, size or resize calls** in those commit intervals.
Writes consumed 17.674 s in total; flushes 25.464 ms. Thus the main problem in this
workload is the growing-file Write path, not SQL compilation, CBOR, excessive
Turso calls, or expensive device flushes. Transport time includes filesystem,
driver, IPC, scheduler and wait time; this does not isolate device latency.

One narrowly scoped optimization now reads neighboring 32-byte ext2 block-group
descriptors in one 512-byte scan-local snapshot. Allocation previously read
the same sector once per skipped descriptor. The snapshot ends at return:
there are no writes during search and every mutation branch returns, so no
descriptor is reused across a metadata write or subsequent allocation. Bitmap,
descriptor and superblock mutation order, uncertainty quarantine and flush
semantics are unchanged. No persistent metadata cache was added.

Deterministic hosted evidence: reaching descriptor 31 now takes 2 descriptor
reads, not 32. The 7 bitmap/metadata calls after selection are unchanged. All 513
focused boundary/failure cases and the full 13-executable filesystem suite pass,
including malformed/partial reads, metadata publication, resize, reclaimed
block reachability, contiguous overwrite and cross-indirect boundaries.

First before/after native Store diagnostic (one run each, **not** a guarantee):

| Measurement | Per-descriptor reads | Coalesced descriptor reads |
|---|---:|---:|
| Commit p50 | 118.448 ms | 62.477 ms |
| Commit p99 | 317.358 ms | 162.417 ms |
| Total write transport time | 17.674 s | 9.465 s |
| Total flush transport time | 25.464 ms | 61.729 ms |

Three further **public Config IPC** runs use the uninstrumented production
worker with the optimized filesystem. All pass native value/revision checks,
independent SQLite/WAL history validation and e2fsck. Median of three run-level
percentiles (milliseconds), compared with the preceding three-run baseline:

| Public operation | Old p50 | New p50 | Old p99 | New p99 |
|---|---:|---:|---:|---:|
| Cached Get | 0.055 | 0.059 | 0.082 | 0.094 |
| Committed Set | 98.811 | 61.096 | 305.446 | 158.645 |
| Get with Set outstanding | 0.430 | 0.134 | 0.520 | 0.177 |
| That overlapping Set | 100.542 | 43.972 | 361.716 | 157.914 |

All 192 overlapping reads arrived before the write reply. Ordinary cached-read
timings did not improve; do not generalize this into an across-the-board speedup.
Raw `/tmp/cubit-config-scan{1,2,3}.{serial,log}`, aggregate
`/tmp/cubit-config-scan-report.md`. Public phases still have only 64 samples
(p99 is the maximum); the separate Store profile has 129 samples.

Both independent Store SQLite/exact-129-revision/e2fsck checks pass. Counts and bytes
at the public filesystem interface are identical; the reduction is inside
ext2's metadata scan. Flush timing still varies with host scheduling. These
are unpinned 4-vCPU KVM runs with uncontrolled host load, calibrated TSC and
assumed cross-CPU agreement. No throughput/hardware/Linux or power-cut claim.

Artifacts: `/tmp/cubit-sql-profile1.{serial,log}`, its `-report.md` and
`-disk/disk.img`; corresponding optimized paths are
`/tmp/cubit-sql-profile-scan1.*` and `-disk/disk.img`. Baseline source/binary
hashes are `/tmp/cubit-sql-profile1-sources.sha256`. Reproduction and the
strict independent checker are in `tests/config-turso/native/run-sql-profile.sh`.

Remaining: allocation still updates bitmap/group/superblock metadata for each
new block. Any future reservation batching or metadata cache must retain
failure/quarantine and data-before-pointer publication behavior. Measure that
work before widening concurrency or changing the allocation algorithm.

Final validation: all 45 hosted Rust tests and both strict profile-parser tests
pass; native `storage-grants` and Linux/CuBit/Linux `bench-storage --check-ext2`
pass (inode reuse, 64 KiB content, e2fsck). Existing focused filesystem SPARK
analysis discharges all 87 checks (49 runtime, 9 functional, 29 flow/termination).
It does **not** prove the new scan's raw disk I/O or the whole allocator; those
are covered by the actual-production-code hosted fault tests and native runs.
Logs `/tmp/cubit-allocscan-{suite,storage,interop,proof}.log`. The normal,
noninstrumented Turso probe was rebuilt after profiling.

## Native public Config transaction baseline — 2026-09-25

New `tests/config-object-client/native-app/run-benchmark.sh` exercises the actual
Config endpoint from an app with only Config authority. It uses separate write
and read-only handles, no client SQL/CBOR/filesystem access. Three KVM boots
pass the in-guest value/revision checks and independent SQLite/WAL/ext2 checks
of the exact type plus all 129 committed integer revisions. No production
service changes were needed for this benchmark.

Microseconds, median of three run-level nearest-rank percentiles:

| Public operation | p50 | p95 | p99 |
|---|---:|---:|---:|
| Cached Get | 55.29 | 74.92 | 82.40 |
| Set through acknowledged commit | 98810.93 | 271536.78 | 305446.36 |
| Get with a Set outstanding | 430.01 | 472.15 | 519.62 |
| That overlapping Set | 100542.43 | 291143.11 | 361715.62 |

All 64 overlapping reads in each run arrived before the write reply and
returned a consistent adjacent committed snapshot. This demonstrates useful
cached-read responsiveness during this workload, not a scheduling guarantee
or proof of the server's internal ordering. The write result makes the next
priority clear: persistent commits need considerably more work even though
ordinary cached Config reads are fast. This is a new workload, not evidence
that the preceding packing optimization caused a regression.

The earlier File benchmark overwrites preallocated data; this workload grows
the WAL and exercises SQL transactions and allocation metadata. Source review
shows ext2 rescans block-group descriptors from group zero on each allocation
and synchronously updates bitmap/group/superblock metadata, with no write-back
cache. Those are candidates to measure, **not yet attributed causes** of the
~100 ms median commit. Next: count/timestamp actual filesystem operations per
transaction and contrast WAL growth with preallocated overwrites before
changing allocation policy, caching, batching, or scheduling. Do not weaken
flush semantics or uncertain-write retirement for benchmark speed.

Each phase has only 64 samples (p99 is the maximum), unpinned 4-vCPU KVM,
128 MiB guest, host CPU model and uncontrolled host load. Counter calibration
checks rate stability; cross-vCPU agreement is assumed. Logging is outside
the timed loops. These are service-acknowledged commits on nonjournaled ext2,
not arbitrary power-cut atomicity, physical-hardware guarantees, or a Linux
comparison.

Reproduce using the native-app README. Validated raw logs:
`/tmp/cubit-config-bench1.{serial,log}`,
`/tmp/cubit-config-bench2.{serial,log}`,
`/tmp/cubit-config-bench3-retry.{serial,log}`; aggregate
`/tmp/cubit-config-bench-report.md`, environment/source/binary hashes
`/tmp/cubit-config-bench-environment.txt`. The original bench3 guest finished
but its host postcheck was disrupted by an in-place runner help-text edit;
that run is excluded and was replaced with a fresh full run. Never edit even
your own running Bash harness.

Host reporter corruption tests and independent SQLite oracle tests pass.
Client regression suite remains 891/98/358/20 checks; focused SPARK analysis
remains 43 checks discharged in message/dispatch/startup units, not a proof
of the benchmark, complete Config implementation, FFI or database engine.

## Direct vector packing — 2026-09-25

The adapter now fills one owned grant from up to 32 borrowed slices, splitting
at descriptor or byte capacity. Headers/pages are copied straight to the loan;
there is no temporary concatenated Rust payload and no extra payload copy.
Scalar batches retain the scalar path. The native build's disassembly shows
segment memmoves targeting `native_storage__object`, and the generic channel
test checks that its fill callback receives the grant address itself.

Exact hosted request-count checks: two 2 KiB buffers take one exchange instead
of two; `[24,4096,24,4096]` takes one instead of four; 65 one-byte segments take
three bounded batches (32,32,1). Empty segments disappear; partial segments
continue in the next batch. Every failed/short batch stops further writes and
completes the parent once. Batching is neither atomicity nor retry permission.

Three native KVM runs again pass content, typed publication, SQLite and e2fsck.
Compared with the preceding 64 KiB **segmented** adapter, depth-1 results
(microseconds, median of three run-level percentiles):

| Vectored workload | Segmented p50 | Packed p50 | Segmented p99 | Packed p99 |
|---|---:|---:|---:|---:|
| 4 KiB (two 2 KiB segments) | 1273.80 | 1138.96 | 3437.56 | 2291.14 |
| 64 KiB (two 32 KiB segments) | 2280.34 | 143.74 | 4601.60 | 3451.61 |

The same 64-sample/unpinned/sequential-group caveats apply; other small-I/O
phases still fluctuate/regress. Exact exchange-count reductions are stronger
evidence than these noisy short timing runs. This is NOT a measured Config
transaction speedup: a native public Config commit benchmark is the next
measurement, along with read responsiveness while storage work is outstanding.

Hosted validation: 44 Rust tests, 43 channel scenarios, and 26 checks of the
actual Ada FFI with modeled syscalls. FFI tests reject empty/oversized/bad
descriptor lengths before reading payloads, check the full 64 KiB header/page
layout, and verify retirement after malformed completion. Raw pointer validity
and single-owner callbacks remain trusted boundary obligations, not new proofs.

Raw `/tmp/cubit-vector{1,2,3}.{serial,log}`, aggregate
`/tmp/cubit-vector-report.md`. Reports identify `vector_layout=packed` and reject
combining packed/segmented runs. All payloads still flow through the same
authority-bound handle/grant protocol; no filesystem wire opcode was added.

## Larger owned transfers — 2026-09-25

`Storage_Channel` now owns one 64 KiB aligned buffer rather than one 4 KiB
page. Native Rust queries the Ada owner's capacity through a scalar FFI and
represents it as NonZeroUsize; there is no duplicated production chunk limit.
64 KiB contiguous requests need one public filesystem exchange instead of
sixteen. This is still serial, copying I/O. The extra resident cost is 60 KiB
per channel, not per file/request; application heap pages are never granted.

Three further KVM runs use the same workload/environment as the baseline below.
Median of three run-level percentiles, depth 1, microseconds:

| Operation | Old p50 | New p50 | Old p99 | New p99 |
|---|---:|---:|---:|---:|
| 64 KiB sequential read | 9276.48 | 1222.93 | 16378.63 | 3522.41 |
| 64 KiB random read | 10212.21 | 1354.73 | 15499.16 | 4664.12 |
| 64 KiB overwrite | 4434.18 | 144.81 | 18630.56 | 3478.23 |
| 64 KiB vectored write | 8711.34 | 2280.34 | 18599.91 | 4601.60 |
| 64 KiB write + flush | 12165.75 | 1285.88 | 18728.03 | 4584.47 |
| 4 KiB sequential read | 71.70 | 69.58 | 1157.39 | 1159.42 |
| 4 KiB overwrite | 74.33 | 75.59 | 1211.90 | 2292.43 |

Large transfers improve substantially in these runs; small transfers do NOT
uniformly improve. Several small-I/O distributions regressed, including the
4 KiB overwrite tail above and batch-8 results. The 64-sample phases, unpinned
host and strong millisecond-scale timing clusters are insufficient to attribute
these differences to one cause. Do not call this an across-the-board speedup
or an achieved latency bound; more sampling and wait/wakeup instrumentation are
needed. Baseline and changed runs are sequential groups, not randomized A/B.

All three runs pass exact content, typed publication, independent SQLite and
read-only e2fsck. Hosted tests pass 42 Rust cases and 42 Ada channel scenarios.
New cases check data/counts across both 4 KiB and 64 KiB transport boundaries,
exact request counts, short/error vector chunks, rejected oversized requests,
too-small read outputs and untouched trailing bytes. Existing scope, failure
retirement and lifecycle checks remain in place; this adds no proof claim.

The rebuilt production Config storage worker also passes native Create/Get/Set
and an independent read-only reboot, with exact SQLite history and e2fsck checks
after both. Logs `/tmp/cubit-large-config{,-reopen}.{serial,log}`. The reusable
grant maps its entire isolated buffer at creation; Acquire checks the requested
range and lifetime without remapping just that subrange. The filesystem peer
must be trusted with that buffer, as before. No application heap is exposed.

Raw `/tmp/cubit-turso-large{1,2,3}.{serial,log}`; full aggregate
`/tmp/cubit-turso-large-report.md`. The reporter labels the actual capacity and
refuses to aggregate runs with different capacities. Next: avoid splitting
small vector segments into separate IPC calls, without an extra concatenation
copy, and measure the real SQL/WAL workload before claiming Config commit gains.

## Native File adapter baseline — 2026-09-25

Three independent CuBit KVM boots now measure the shared, content-verified File
workload using calibrated local TSC timestamps. The normal provisional Rust
Instant bridge calls Clock IPC at millisecond resolution and is deliberately
excluded from these measured intervals. Native metadata, grant handling,
filesystem/device work, scheduling and completion callbacks ARE included.

Environment: Ryzen 7 5800X, Linux 7.0.0-31 host, QEMU 11.1.0, `-cpu host`, four
guest CPUs, 128 MiB; release O2, unpinned, host load uncontrolled. Disposable raw
ext2 disk with QEMU NVMe, runner-default host caching (no explicit cache flag).
4 KiB/64 KiB operations use initialized 128 KiB/2 MiB scratch files. This is
not the Linux baseline's 4 MiB page-cache workload: do not compare the numbers
as an OS win/loss. No physical-media or power-cut durability claim.

Selected depth-1 results in microseconds, median of three run-level percentiles:

| Operation | p50 | p95 | p99 |
|---|---:|---:|---:|
| 4 KiB sequential read | 71.70 | 1139.54 | 1157.39 |
| 4 KiB random read | 96.47 | 1150.62 | 2297.05 |
| 4 KiB overwrite | 74.33 | 1143.30 | 1211.90 |
| 4 KiB vectored write (two 2 KiB segments) | 254.02 | 2303.17 | 3434.43 |
| 4 KiB write plus flush | 2250.09 | 2300.54 | 2420.40 |
| 64 KiB sequential read | 9276.48 | 15345.70 | 16378.63 |
| 64 KiB overwrite | 4434.18 | 17321.75 | 18630.56 |

Each phase has only 64 measured samples (p99 = maximum), two warmup batches,
and substantial run-to-run variability. These are exploratory baselines, not
latency guarantees. All 18 phases per run report `peak_deferred=0`: a requested
batch size of eight does not create I/O concurrency in this adapter.

Source inspection confirms each 64 KiB request becomes sixteen serialized
4 KiB exchanges; two 2 KiB vector segments become two exchanges. The timing
distribution is consistent with accumulating scheduling/IPC delays, but the
baseline does not isolate that cost from device work. Next experiment: larger
bounded grant transfers, then scatter/gather packing into the existing owned
loan. Preserve exact byte counts, exclusive handles, failure retirement and
explicit flush. Genuine pipelining needs separate queue/lifetime work through
the filesystem and block service; async submit alone does not establish it.

Validation: three native workloads plus typed publication, independent SQLite
and e2fsck PASS; 41 hosted Rust tests PASS (including clock injection and
regression rejection); parser tests cover incomplete/duplicate/corrupt results.
No new SPARK or asynchronous-I/O proof is claimed. The test probe now calls its
generated Ada standalone-library initializer, as the production worker already
does; the separate unbound storage archive was removed. std-only and full Turso
probe builds still pass.

Commands: `native/run-benchmark.sh` in Nix under the build lock; details in
`native/README.md`. Raw logs `/tmp/cubit-turso-io-{baseline,repeat2,repeat3}.serial`
and corresponding `.log`; full aggregate `/tmp/cubit-turso-io-three-report.md`.

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
* The native Turso application exercises MemoryIO and native filesystem IPC.
  Both shared workloads pass, as does a Linux SQLite check of the guest-written
  ext2 database. No native Turso disk throughput claim has been measured.

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

The next amplification identified here was splitting existing-data writes at each
Ext2 block even when mappings are contiguous; the follow-up below addresses it.
End-to-end loan/DMA copies and actual queue concurrency remain larger projects.

## Bounded existing-block write coalescing — 2026-09-23

The production write path now groups physically consecutive, already allocated
full filesystem blocks into one block-device write. With a 4 KiB grant and an
eight-sector (512-byte) provider limit, a contiguous 4 KiB overwrite on 1 KiB
filesystem blocks drops from **4 data requests to 1**. Including the earlier
unchanged-inode optimization, this workload went from 7 total requests to 1.
The 4 KiB-filesystem-block case stays at one request. These are hosted exact
transport counts, not a sevenfold end-to-end latency claim.

Every coalesced batch is bounded by the request, original EOF, supported writable
mapping range, grant capacity and provider transfer limit. Fragmentation and holes
end a run. A failed/malformed speculative mapping read aborts the not-yet-written
batch without retry or data submission. Earlier completed batches remain the
acknowledged prefix; a rejected batch contributes zero bytes even if the device
may have changed part or all of it. Nothing promises atomicity or rollback.

Partial blocks, EOF growth, allocation and filesystem blocks smaller than device
sectors retain the original checked path. This avoids turning coalescing into
speculative allocation or a multi-command batch with hidden partial completions.
No new cache, wider grant, skipped authority check or weaker flush is introduced.

Hosted tests cover separate grant/provider bounds, partial starts/tails, unchanged
bytes outside the request, fragmented mappings, sparse allocation handoff, larger
device sectors, request/EOF boundaries, and direct-to-single-indirect transitions.
There are 345 aligned/unaligned overwrite fault cases and 75 coalesced indirect
fault cases, in addition to the earlier read/metadata/admission suites. The latter
check both a failed lookahead read and failures after a completed earlier batch.
Double-indirect writes remain unsupported, with a checked completed prefix at the
boundary. This is regression-tested production Ada, not a new SPARK proof.

One native four-vCPU TCG run before/after this coalescing step also showed lower
benchmark latency. The figures below are calibrated histogram bucket upper bounds,
not exact quantiles or physical-disk timings; each phase has 512 samples:

| Phase | Before p50 / p99 bucket (µs) | After p50 / p99 bucket (µs) |
|---|---:|---:|
| 4 KiB overwrite | 690 / 828 | 208 / 277 |
| 4 KiB overwrite plus flush | 828 / 966 | 416 / 555 |

Sources: `/tmp/cubit-overwrite-bench.serial` (3,800,922 ticks/ms) and
`/tmp/cubit-coalescing-bench.serial` (3,779,852 ticks/ms). These ephemeral logs
are not committed. Builds/host load were not tightly controlled and there was
one run per version. The exact request reduction is the stronger result; these
observations do not establish a hardware speedup or a Linux comparison. The
ordinary native storage and benchmark workloads both reached completion.

## Allocated-sector accounting and publication failures

Replaced EOF-derived Ext2 sector counts with allocation-derived counts. A sparse
hole contributes nothing until filled, each data block contributes its full
allocation, and a single-indirect pointer block contributes too. Extending
within an existing block and ordinary overwrites do not increase the count.
Directory growth uses the same proved inode mapping/count transformation.

SPARK discharges all 19 checks in the extracted production value/accounting
units: exact increments without overflow, unchanged output on rejection and
preservation of all unrelated inode fields. This is not a proof of allocation,
disk publication, whole-filesystem consistency or hardware behavior.

The complementary hosted tests pass 4,710 injected storage-completion failures,
including failures after earlier attachments succeed and checked out-of-space
cleanup. They exposed an unsafe free-after-failed-I/O fallback, now removed.
Ambiguous mutation stops further I/O and quarantines the volume instead of
attempting speculative rollback or publishing a partially constructed inode.
See [test and proof scope](../filesystem-truncate/README.md#allocated-sector-accounting).

## Linux interoperability and complete inode initialization

Before increasing the writable-file limit, added an independent Linux-image
round-trip. It reproduced stale extended bytes surviving reuse of a 256-byte
inode: allocation previously initialized only the original 128-byte header.
New inode reservations now initialize the full declared slot; ordinary updates
preserve the existing extended area. Inode strides must be powers of two, and
the inode-write descriptor offset is widened before multiplication.

All 18 hosted production-driver combinations (three block sizes, three inode
sizes, two feature profiles) pass pre/post `e2fsck -fn`, content checks and xattr
preservation. A separate native CuBit/NVMe benchmark round-trip passes Linux
verification of inode reuse, its complete 64 KiB payload and `e2fsck`. Another
930 injected failures cover initialization sizes through 4 KiB. Existing
overwrite request-count regressions and all 23 local SPARK checks still pass.
See [commands, scope and limitations](../filesystem-interop/README.md).

## Nonzero file sizing and safe EOF growth

Added native `Resize_Request(handle, length)` using the existing owned writable
handle, not a new grant or policy route. Shared inode metadata is updated without
moving any alias's seek cursor. Read-only handles cannot resize. Requests use
two words (handle and byte length), with strict tag validation.

The Ext2 core now handles nonzero sizes through its direct/single-indirect tree.
Shrink detaches mappings, flushes inode/retained pointer updates, and only then
reclaims storage. Emptying a file uses this same implementation. Growth remains
sparse, clearing any mapped bytes between old and new EOF; positioned writes
past EOF share that path. This closes the stale-tail exposure path following an
unaligned shrink. Ordinary aligned 4 KiB overwrites still use one block request.

Validation in Nix:

- 4,551 new injected resize/gap-write failures, with durable-detachment checks
  before every attempted free-bitmap clear.
- Existing suites pass, including the expanded 4,845 sector-attachment failures.
- 18 Linux-created images pass content extraction and clean `e2fsck -fn` after
  shrink/regrowth and a retained-block positioned-write gap.
- Native four-vCPU QEMU TCG `storage-grants` passes resize authority, alias and
  cursor checks, as well as the existing filesystem/grant tests.
- SPARK: 26 accounting/admission checks, zero unproved; the new property is exact
  subtraction with unchanged output on rejected underflow. I/O is regression-
  tested, not proved, and this is not journaling or power-loss recovery.

Logs: `/tmp/cubit-resize-regressions.log`, `/tmp/cubit-resize-final-hosted.log`,
`/tmp/cubit-resize-native.serial` (ephemeral, not committed).
Double-indirect writes/resizing, exclusive database ownership and the persistent
native Turso file adapter remain next steps. Config is not yet database-backed
on persistent CuBit storage merely because this filesystem primitive exists.

## Double-indirect overwrite stage

Extracted production block-path decoding into `Block_Paths`, a Pure/SPARK
package returning a bounded discriminated path value. The local combined proof
passes 46 checks, none unproved, including exact path ranges/indices. Hosted
tests exhaust 1,378,087 supported/boundary indices across all three geometries.

Existing double-indirect data can now be overwritten without changing inode or
mapping metadata, including coalescing across leaf boundaries. A warm contiguous
4 KiB overwrite remains one payload request. Another 105 injected completion
faults cover metadata lookup, lookahead, payload and cache recovery. All existing
regressions and 18 Linux content/inode/fsck round-trips pass.

A lock-serialized native four-vCPU TCG `storage-grants` run also passes
`FILE-DOUBLE-OVERWRITE-CHECK`: a sparse Linux-created double-indirect file is
opened, overwritten, read back with surrounding bytes checked, flushed and
closed through real CuBit IPC/grants/NVMe. Log:
`/tmp/cubit-double-overwrite-native.serial`. This is integration evidence, not
physical hardware performance or power-loss recovery evidence.

At this stage, holes/extension and double-tree resizing still failed explicitly.
Reclamation needed redesigning
without growing the current snapshot and quadratic duplicate walk to a million
blocks before enabling those mutations. The persistent Config/Turso adapter
remains gated on those semantics and exclusive database ownership.

## Double-tree allocation and bounded reclamation — 2026-09-23

The growth/reclamation gate above is now removed for standard double-indirect
trees. New allocation initializes children before parent-pointer publication;
exact sector accounting includes newly allocated root/leaf blocks. A definite
no-space result can reclaim unpublished reservations, but a failed transport
completion stops further I/O and quarantines uncertain mutations.

Resize preflight inventories every data/pointer block, checks the claimed count,
and sorts the inventory to detect duplicates across tree levels. This replaces
the quadratic duplicate scan. Temporary memory is four bytes per admitted
allocated block (maximum 4,202,552 bytes), released before mutation. Reclamation
then detaches, flushes and frees leaf-sized batches. Sparse growth skips absent
subtrees; discarded data in a retained partial block is still zeroed before
exposure. These are bounded-memory and algorithmic improvements, not measured
native throughput gains. The filesystem remains synchronous and copy-based.

Validation:

- 9,105 attachment failure cases, including new/reused double roots/leaves,
  sector overflow, no-space and cleanup failures.
- 13,356 double-resize failure cases, plus 4,839 direct/single resize/gap cases.
  Every bitmap clear is checked against the durable tree's remaining references.
- Maximum-size inventory sorting/permutation test and sparse final-double-slot
  allocation across the 32-bit size boundary; LARGE_FILE is required above 2 GiB.
- All 18 Linux Ext2 content/inode/fsck round-trips pass with double allocation
  and shrink/regrowth included.
- Native `storage-grants` passes double growth, resize, alias coherence,
  zero-tail verification, flush and truncate through CuBit IPC/grants/NVMe.
  QEMU TCG, not physical-device benchmarking.
- The focused SPARK project passes 87 checks, none unproved. This proves local
  path/accounting/preservation and inventory safety properties, not heapsort
  permutation, disk-tree ownership, I/O ordering or crash consistency.

Logs: `/tmp/cubit-double-tree-regressions.log`,
`/tmp/cubit-double-allocation.log`, `/tmp/cubit-tree-inventory-rename.log`,
`/tmp/cubit-large-file-boundary.log`, `/tmp/cubit-double-tree-native.serial`.

Next is enforced exclusive database ownership, then a persistent native Turso
File adapter. It must not pretend an always-successful lock is meaningful.
Power-loss consistency, service-restart recovery and Config activation remain
separate gates; this filesystem work does not turn MemoryIO into persistence.

The existing Linux-hosted Turso regression suite was also rerun: 13 Rust tests
and two benchmark-report tests pass; formatting is clean. Log:
`/tmp/cubit-turso-filesystem-check.log`. Those tests still use their hosted
backends; they are not evidence that a native persistent adapter exists.

## Exclusive file ownership — 2026-09-24

The existing shared-inode table now enforces `OPEN_DENY_SHARING`, distinct from
create-if-absent. Admission requires write access and excludes every alias,
including same-process and read-only opens. Conflicts are detected before
truncate, return `REPLY_SHARING_VIOLATION`, and leave existing handles unchanged.
Renaming the held inode is rejected. Close or authenticated policy reset releases
it; a stale close cannot release a replacement handle. No per-I/O lock round-trip
or authority expansion is introduced.

35 local SPARK checks prove initialization, successful exclusive isolation,
unchanged state on conflicts and preservation through table operations.
3,968 hosted admission/lifecycle scenarios and 8,192 open-option encodings pass.
Native `storage-grants` passes, including `FILE-EXCLUSIVE-CHECK: PASS`, existing
rename/corrupt-directory tests, resize and grant/authority regressions.

This is individual-file exclusivity, not a persistent database-wide lease.
Parent-directory names are not pinned; sidecars need their own protected handles
and a private namespace. Crashed clients remain blocked until trusted cleanup;
there is no unsafe timeout-based stealing. Automatic process-death cleanup and
filesystem restart recovery are still open. The native Turso adapter still uses
MemoryIO and has not been switched to persistent storage in this step.

Logs: `/tmp/cubit-exclusive-proof.log`, `/tmp/cubit-exclusive-regressions.log`,
`/tmp/cubit-exclusive-native.serial`. Proof scope and lifecycle details are in
[filesystem-exclusive-ownership](../../docs/filesystem-exclusive-ownership.md).

## Native persistent Turso adapter — 2026-09-24

The isolated native probe now runs the shared Config Store on ext2 through
CuBit filesystem IPC, not `std::fs`, a POSIX shim, or MemoryIO. It owns separate
deny-sharing handles for the database and WAL. A small Ada bridge constructs
the existing typed requests and validates replies; one serialized 4 KiB grant
page copies data between Rust buffers and the filesystem. This is a synchronous
correctness baseline, not yet an async or zero-copy adapter.

The native four-vCPU TCG run passes raw File workloads, conflicting database/
WAL opens, typed revision commit/conflict, explicit checkpoint, close/reopen,
and confirmed grant retirement. After QEMU exits, `debugfs` extracts the actual
database from its disposable raw ext2 image; independent Linux SQLite passes
`integrity_check` and checks all three Config tables, the revision and exact
CBOR bytes. Read-only `e2fsck -fn` also passes. No loopback mount is needed.

The first native attempt exposed an adapter bug: successful opens have two
reply words (handle and size), unlike ordinary one-word results/error replies.
The bridge now validates that distinction instead of rejecting valid opens.

All 18 hosted Rust tests pass. The five native-adapter tests cover SQL reopen,
realistic transport exclusion, all four failed/short vectored-write chunks,
short reads, overflow admission, reentrant callbacks, malformed replies, failed
flush and uncertain open. Uncertain I/O retires the backend; cleanup closes
remain allowed. These are regression tests, not a SPARK proof of Rust/Turso/FFI.

Logs: `/tmp/cubit-turso-adapter-all.log`, `/tmp/cubit-turso-persistent.log`,
`/tmp/cubit-turso-persistent.serial`. This is clean-close persistence and standard
SQLite/ext2 interoperability, **not** power-loss safety. Metadata crash consistency,
service-death ownership recovery, private parent-namespace policy, and Config's
boot-seed/backing-store attachment are still gates before replacing `config.svc`.

## Fresh-boot persistence — 2026-09-24

Two independent four-vCPU CuBit/TCG boots now pass. The first writes revision 1;
the second starts from a copy of that saved raw disk, restores the original
typed profile, rejects a stale update, and saves revision 2 (scale 125 → 150).
Each run passes Linux SQLite integrity and exact CBOR checks afterward; both
old and new immutable revision rows are verified. Read-only ext2 checks pass
before and after each boot. Images and extracted databases are retained for
inspection, without altering the user's base disk.

This caught a fixture issue independently reproduced on Linux: `debugfs mkdir`
on an existing directory left orphaned metadata. Restaging now reuses an existing
directory, and checks ext2 before boot so such fixture damage cannot be attributed
to CuBit. Three hosted tests check the independent SQLite oracle, alongside all
18 existing Rust tests. No new formal proof or power-loss guarantee is claimed.

Reproduce with `native/run-reboot.sh` under Nix and the shared build lock.
Results: `/tmp/cubit-turso-reboot.lSNone/verified.log`, with raw images and SQLite
files under its sibling `verified/seed/` and `verified/reopen/` directories.
Both guest tests passed; the original final cleanup rebuild failed on another
session's changing shared runtime source. Cleanup now copies saved binaries;
the default probe was separately restaged from the already-built Ada archives.
# NVMe interrupt conversion constraint — 2026-09-25

Read-only follow-up to the measured slow-path latency: `nvme.adb` still uses
65,536 phase polls followed by up to 1,000 one-millisecond sleeps, with CQ
interrupts disabled. The existing `Wait_For_Activity_Until` wakes for queued
service requests as well as device events/completions. Substituting that call
inside the synchronous `pollIOCompletion` loop would therefore allow unrelated
queued requests to wake it repeatedly while DMA is unfinished; retaining the
attempt-count timeout would also turn those wakeups into a premature timeout.

The NVMe interrupt path should use the shared asynchronous-driver pattern:
retain the outstanding request and its acquired grant, drain/route event hints,
check the CQ's phase and command identity, and reply only after authoritative
completion. Use an absolute deadline rather than counting wakeups. Queue
admission/backpressure must not create a permanently-ready service queue that
defeats blocking. Preserve the current failed-command DMA quarantine; a timeout
is not evidence that the controller relinquished a buffer. Enabling MSI alone
does not solve these lifetime and wait-loop requirements. No driver/kernel
changes or performance improvement are claimed by this review.
