# Hosted allocator proof, regression and benchmark suite

Always run through Nix, from the repository root:

```sh
nix develop -c bash tests/userspace-allocator/run.sh
nix develop -c bash tests/userspace-allocator/runtime.sh
nix develop -c bash tests/userspace-allocator/benchmark.sh
nix develop -c bash tests/userspace-allocator/diagnostics.sh
nix develop -c bash tests/userspace-allocator/thread-benchmark.sh
```

`run.sh` builds arithmetic/bounds-check-enabled **hosted** tests and runs
GNATprove at level 2 with two workers, a 15-second prover timeout, and unproved
checks treated as errors. Tests use explicit checks; recursive Ghost induction
lemmas are proved, not executed as runtime assertions.
For a busy host, increase the solver budget without changing proof obligations:

```sh
nix develop -c env CUBIT_ALLOCATOR_PROOF_TIMEOUT=60 bash tests/userspace-allocator/run.sh
```

`check-proof.py` also requires nonempty proof coverage for all five selected
units/instances, no skipped analysis, and no `pragma Assume`.
The generic slab proof is run at 4 and 256 slabs; include any new
instantiation in the proof project before relying on its verification status.

`runtime.sh` tests the separate checked Ada offset adapter and Rust GlobalAlloc
boundary on Linux: alignment through 1 MiB, zeroing/reuse, realloc transitions
and failure preservation, region exhaustion and large-run fragmentation.
`tests/rust-native/run.sh` exercises Vec/String/Box inside CuBit using the same
cores, plus native IPC and opposite Clock authority outcomes. These boundary
tests are not SPARK proofs and are separate from the small-object benchmarks.

The release benchmark library uses `-O2 -gnatp -gnatn2`; the harness uses Rust
`opt-level=3`. Its codegen check rejects emitted Ghost/assertion symbols and
checks the bridge's remaining calls against an explicit core-helper allowlist. This is distinct
from asserting that a native kernel has runtime assertions enabled—it does not.
The archive audit also rejects diagnostic observer symbols and GNAT packed-bit
comparison/copy helpers, preventing test instrumentation or unexpectedly costly
packed-array slice operations from entering the release library unnoticed.

## Hardware profiling (Linux only)

Build with `benchmark.sh` above first. `perf` must permit userspace hardware
events (`perf stat -e cycles:u,instructions:u -- /bin/true`); the runner never
changes host permissions. Then use a fresh output directory:

```sh
nix develop -c python3 tests/userspace-allocator/profile.py \
  --cpu 7 --iterations 10000000 --repetitions 3 \
  --output tests/performance/results/allocator-perf
```

The benchmark optionally handshakes over perf control/ack FIFOs immediately
around the churn loop. Trace generation, payload validation, warmup, introspection
and sorting are excluded. A constant amount of handshake code remains inside
counter scope; no per-allocation profiler hooks are added. Normal benchmark
runs clear those environment variables and require no perf permissions.

The runner uses userspace-only counters, three interleaved repetitions of
CuBit/mimalloc/jemalloc across four traces, and separate branch/cache event
groups. It rejects unsupported counters and multiplexed groups, then records
cycle samples without stack unwinding. It preserves the executable, hashes,
library paths, counters, profiles and text reports in the output directory.
CPU affinity is not isolation; host frequency and SMT interference remain
uncontrolled. Counter values cover free + allocation + byte touches + harness,
not malloc alone. Profiled timings are not uninstrumented benchmark results.
Use `--baseline /absolute/path/to/old-bench` to interleave an earlier CuBit
binary; it must support the same perf FIFO gate. Both binaries are preserved.

Optional branch-miss samples:

```sh
nix develop -c python3 tests/userspace-allocator/profile.py \
  --record-only --sample-event branch-misses:u --period 1009 \
  --output tests/performance/results/allocator-perf-branches
```

Generic PMU sample IPs can skid beyond the triggering instruction. Do not treat
`perf annotate` percentages as exact attribution to individual branches or
instructions; counter totals and coarse cycle hotspots are stronger evidence.

## Independent allocation replay

`diagnostics.sh` runs a separate Linux-hosted model against the actual core.
For each of the six benchmark workloads it reproduces the deterministic PRNG,
10,000-operation integrity prefix, cleanup, warmup and 1,000,000-operation trace.
Before each allocation it predicts success and the exact returned offset, then
checks that prediction against the real allocator; releases are checked too.
Only the final million operations contribute to each CSV row.

The model uses ordinary Boolean membership and an Ada bounded vector for the
64-entry cache, rather than the core's packed bitmap and explicit stack array.
It implements the same refill/eviction policy independently and keeps its own
counts, classes and slab hints, never reading private allocator state. This
checks policy correspondence, not an independent derivation of the policy.
Slab selection models the eight-slab lookahead followed by complete class and
empty scans. Probe counts include the initial hint, lookahead, and repeated
positions in the full fallback; they are not counts of unique slabs touched.
The obsolete private bitmap-search observers remain removed.

`page_misses` and `page_probes` describe model slab selection. `cache_hits`
and `refills` distinguish allocation with a cached entry from batch refill.
Neither distinguishes fresh payload from previously used payload. Old full-stack
reuse/fresh counters and scalar bitmap-search counters no longer apply.
These are **not hardware events or a proved cost model**. The model is
regression-tested, not SPARK-proved; no instrumentation enters the release core.

## Regressions

- A bitmap pool undergoes 20,000 operations against an independent Boolean
  model, including a non-power-of-two capacity and releases outside that capacity.
- Capacities one and 4096 exercise exhaustion, duplicate release, refusal to
  reconfigure a live pool, and empty-pool reuse.
- Every recycled position is checked in both LIFO release orders for
  capacities 1, 2, 63, 64, 65, 127, 128, 129, 4095 and 4096: this covers full
  words, partial tail words and exhaustion. A full-capacity shuffled release
  checks complete reuse of all 4096 slots without duplicates. A sparse 65-hole
  test overfills the 64-entry cache and recovers the evicted entry in a mostly
  live slab, including duplicate releases while the cache is full.
- All request sizes and every slab-local quotient/alignment boundary are checked.
- Every offset in the 16 MiB arena is checked against ordinary integer page
  division and local remainder, including every slab boundary and the final byte.
- Four slabs are filled with 4096-byte blocks: releasing one block cannot permit
  retyping, releasing a whole slab can, and neighboring live blocks remain valid.
  The same backing then holds 16,384 small blocks.
- Slab-selection tests cover a nearby matching hole, fallback from the final
  slab to the first, and unchanged failure after exhaustion. A 256-slab fixture
  leaves a hole beyond the lookahead window and checks that it is found before
  retyping an unused slab, with all other live blocks preserved.
- Another 30,000 mixed-lifetime operations check independent live byte intervals,
  unchanged failure state, size-class stability and exact per-slab counts.
- The real release-mode Ada/host ABI is exercised from Rust: all classes are
  filled simultaneously and every requested byte is written and verified with
  distinct block identities. Tests cover zero/oversize allocation, null/foreign/
  interior pointers, immediate double frees and full reuse. Invalid-pointer
  tests apply **only to our adapter**, never to a standard reference allocator.
  A single 4096-byte class can consume the entire 16 MiB backing region.

The proof establishes exact slot membership changes, unchanged failure state,
aligned in-bounds offsets, disjoint distinct blocks, exact occupancy accounting,
no live-slab retyping, and composed heap invariant preservation. It does not establish pointer-client
lifetimes, concurrency or a general-purpose unbounded heap. The cache invariant
also proves its active indices are distinct, free and in range. See the
[allocator boundary](../../userspace/allocator/README.md).

## Single-owner comparison

```sh
nix develop -c bash tests/userspace-allocator/benchmark.sh \
  --iterations 1000000 --repetitions 7 \
  --output tests/performance/results/allocator-final
```

References are glibc malloc, mimalloc, jemalloc and **gperftools TCMalloc**.
The latter is not Google's newer per-CPU TCMalloc implementation. Nix pins their
versions; references are loaded only in disposable Linux-hosted processes.
`dladdr` verifies both `malloc` and `malloc_usable_size` resolve to the intended
library. Reference allocator tuning environment variables are removed.

Each run has a fresh process, one allowed CPU, deterministic precomputed trace,
1024 live positions, an untimed integrity pass and warmup. Job order is shuffled
with a fixed seed. Workloads cover fixed 64/256 bytes, uniform 1–128/1–4096,
size-class boundaries and a small/large bimodal distribution.

The timed unit is **free + allocate + first/last volatile payload touches**, with
harness bookkeeping included. Every 128th pair is timed separately; reported
sampled p50/p99 include clock overhead, whose median is recorded separately.
These samples are not hard latency guarantees. Throughput medians are usually
more informative than sub-clock-resolution differences between tiny samples.

Results include full run data, source/build-input/binary hashes, versions,
backend paths, CPU details and load average. The host is not isolated: frequency
scaling, interrupts and unrelated workloads remain possible. Pinning is not
equivalent to owning a physical core or its SMT sibling.
Use `--cpu 7` (for example) to select an allowed logical CPU explicitly; the
default remains the lowest CPU in the inherited affinity set. Invalid choices
are rejected. This changes benchmark affinity only, not host scheduling policy.

To compare a change against a preserved executable, copy the previous `build/bench`
to a separate location before rebuilding, then pass `--baseline /absolute/path`.
The harness interleaves its CuBit runs with current/reference runs and records
the baseline binary hash. Preserve that executable's original result JSON for
source/build provenance. Both executables must implement identical workload
semantics; the harness verifies engine/workload identity but cannot establish
trace equivalence for an arbitrary executable. Finish proofs before final
timing runs so our own proof workers do not contend with the measurements.
The summary reports the CuBit/jemalloc median ratio for each workload against
the 1.10 target, rather than averaging unlike traces into a favorable score.

`malloc_usable_size` is queried **outside timing**. Live rounding overhead is
`usable / requested - 1` at the end of the trace, not total fragmentation. It
excludes free pages, allocator metadata and retained caches. Process peak RSS
includes the trace, harness, libraries and allocator; it is not heap-only RSS.
The fixed backing arena reservation and metadata size are reported separately.

## Reference concurrency baseline

```sh
nix develop -c bash tests/userspace-allocator/thread-benchmark.sh \
  --rounds 256 --repetitions 5
```

Uses 1/2/4 threads where available, restricted as a group to different physical
cores (not individually pinned). Each round handles batches of 512 allocations
with sizes 1–4096. Local mode allocates and frees on the same worker; remote mode
passes ownership around a channel ring while the allocating workers remain
alive. Remote results include channel transfer and synchronization. The metric
is elapsed wall time divided by total pairs across workers: **aggregate
throughput cost**, not individual operation latency.

The prototype is intentionally excluded: its single-owner state is not safe to
race. No misleading global mutex wrapper is compared to production thread heaps.

## Remaining coverage

Large/over-aligned allocation, realloc preservation/failure, calloc overflow,
page acquisition/release, mixed long-lived objects, long-duration fragmentation,
NUMA, thread teardown/orphan heaps and real application traces remain future
work. Add each to this suite when its core functionality exists. Do not treat
the current small-object results as an overall allocator ranking.

Raw results live under ignored `tests/performance/results/`. A concise dated
[results note](../../docs/userspace-allocator-results.md) is kept in the docs.
