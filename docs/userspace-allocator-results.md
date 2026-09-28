# Userspace allocator evidence — 2026-09-20

## Current milestone: class-directed slab scan with bounded lookahead

The retained optimization adds **no index metadata or per-object maintenance**.
On a validated class-hint miss, the scan receives the already computed size-class
enum, hoists capacity calculation out of the loop, and returns a single bounded
slab reference instead of a padded page/Boolean aggregate. It tries the next
eight slabs before the complete matching-class scan and then the empty-slab
scan. This is a search-order heuristic, not a new source of allocator authority.
Public allocation/release/lifetime contracts and failure preservation are intact.

We first tried exact dense sets of available slabs per class and empty slabs.
Although selection eliminated searches and executable replay passed, maintenance
made fixed-size traces roughly 50–60% slower and broad mix slower too. Its
composed allocation proof also retained an unproved postcondition at the shorter
20-second budget; it was rejected for performance rather than pursued further.
The implementation and its dedicated tests were removed, with the experiment
preserved only under ignored results. A complete circular scan then improved
broad mix but added long wraparound scans elsewhere. Bounded lookahead retained
the benefit with fewer modeled probes. No compiler flags changed.

Final timing: fifteen interleaved repetitions, CPU 7, one million
free/allocate/touch pairs per run, **ns/pair including harness work**:

| Trace | Previous | Lookahead | Improvement | jemalloc | mimalloc | glibc | gperftools TCMalloc |
|---|---:|---:|---:|---:|---:|---:|---:|
| Fixed 64 | 8.324 | 8.399 | -0.9% | 7.547 | 6.797 | 6.156 | 6.308 |
| Fixed 256 | 8.970 | 9.047 | -0.9% | 9.289 | 8.730 | 6.486 | 6.807 |
| Small mix | 10.008 | 9.984 | 0.2% | 10.229 | 7.189 | 9.998 | 6.939 |
| Broad mix | 19.201 | 17.634 | 8.2% | 17.456 | 18.603 | 53.922 | 9.173 |
| Boundary | 11.831 | 11.262 | 4.8% | 12.367 | 9.401 | 19.930 | 8.153 |
| Bimodal | 10.970 | 10.869 | 0.9% | 10.950 | 8.591 | 20.185 | 7.437 |

Broad mix is now about 1% behind jemalloc on this trace and faster than mimalloc;
this is not overall parity. Five traces meet the 10% jemalloc target, with
fixed64 still 11.3% slower. Fixed-size performance is not an improvement:
the measured changes are around 1% slower. The preliminary nine-repeat run
also showed broad/boundary improvements with small changes elsewhere.
Metadata remains **262,440 bytes per 16 MiB arena**, and size-rounding waste is
unchanged. Proof/replay workers were finished before timing; CPU pinning is not
isolation, and host frequency/SMT interference remain uncontrolled.

**Proof and tests:** 254 checks (66 flow, 188 prover), zero unproved or justified,
all four selected units analyzed. The changed scan proves successful discovery
iff usable class capacity or an empty slab exists. New bounds-enabled regressions
cover nearby selection, fallback from the last slab to the first, unchanged
exhaustion, and a matching hole beyond the lookahead window that must be used
before retyping unused backing. Existing mixed-lifetime and release-ABI payload
tests pass, as does the release-code audit for Ghost/assertion/helper leakage.
No Assume/SPARK-Off escape was added to the metadata core.

Six million measured exact-address replay comparisons pass, plus setup/warmup.
Broad-mix hint misses fall from 147,525 to 91,116 per million allocations;
modeled probes fall from 4,334,571 to **1,571,928 (63.7% fewer)**. These include
the hint, lookahead and repeated probes during fallback; they are not hardware
events or unique slabs touched. Different selection order also changes future
hint hits, so the gain is not solely a faster implementation of identical probes.

Separate FIFO-gated PMU runs (three repetitions, ten million pairs, counter
groups fully scheduled) show broad-mix instructions/pair falling from **184.144
to 154.405 (16.1%)**, and cycles/pair from **87.631 to 78.911 (10.0%)**.
Branch misses remain approximately 1.26–1.27 per pair: this gain removes work,
not branch misprediction. Fixed64 instructions remain 153.219 per pair, matching
the unchanged hot-path work. The new broad-mix cycle sample attributes about
**6.4%** to slab scanning, versus roughly 17% in the previous round. Sampled
percentages are approximate attribution, not exact per-instruction accounting.

The heuristic can add eight probes in the worst case; complete fallback still
exists, and slot-cache refill can still scan 4096 positions. No constant-time,
real-time, concurrent-safety, cold/burst-latency or native-integration claim is
made. This remains the Linux-hosted single-owner pilot.

Evidence: ignored `tests/performance/results/allocator-slab-dense/` (rejected),
`allocator-slab-scalar/`, `allocator-slab-circular/`, `allocator-slab-nearby/`,
`allocator-slab-final/`, and `allocator-slab-perf/`. Final artifacts preserve
source, binaries, baseline provenance, full proof, regression and replay logs.
Retained core: 483 lines across seven files, including comments and Ghost code.

## Previous milestone: bounded free-index cache with a power-of-two stride

The full per-slab free stack is now a 64-entry cache. Empty caches refill from
the membership bitmap; returning into a full cache replaces its top entry,
leaving the displaced slot free in the bitmap for future refill. The public
success, membership, failure-preservation and lifetime contracts are unchanged.
There are still no payload links, raw pointers, C, assembly, intrinsics, Assume
pragmas or SPARK-Off sections in the core. Ownership remains sequential.

Metadata falls from **2,230,568 to 262,440 bytes per 16 MiB arena**, an **88.2%**
reduction (13.3% to 1.56% of backing). This is allocator metadata, not whole-process
RSS or total fragmentation. Power-of-two size classes still have the same
rounding waste as before; this change does not solve that separate problem.

The first tightly packed cache proved but regressed most traces by roughly
5–12%. Separating its occupancy/cache counters improved generated code; giving
pool objects a 1 KiB stride then simplified slab address calculation enough to
beat the previous full-stack implementation. The stride deliberately includes
padding; GNAT reports the unused bits and that warning is not suppressed.
No optimization flags were changed. These are measured x86-64 layout results,
not a claim that every compiler/architecture prefers the same arrangement.

Final comparison: fifteen interleaved repetitions, CPU 7, one million
free/allocate/touch pairs per run. Units are **ns/pair**, not malloc alone:

| Trace | Full stack | Cached | Improvement | jemalloc | mimalloc | glibc | gperftools TCMalloc |
|---|---:|---:|---:|---:|---:|---:|---:|
| Fixed 64 | 9.474 | 8.349 | 11.9% | 7.619 | 6.796 | 6.182 | 6.286 |
| Fixed 256 | 10.321 | 9.034 | 12.5% | 9.094 | 8.730 | 6.535 | 6.795 |
| Small mix | 11.383 | 10.085 | 11.4% | 10.241 | 7.117 | 10.042 | 6.985 |
| Broad mix | 19.958 | 18.944 | 5.1% | 17.061 | 18.534 | 52.940 | 9.068 |
| Boundary | 12.666 | 11.682 | 7.8% | 12.229 | 9.342 | 19.954 | 8.179 |
| Bimodal | 11.827 | 10.970 | 7.2% | 10.862 | 8.553 | 20.211 | 7.405 |

Five traces meet the within-10%-of-jemalloc target; broad mix is **11.0%** slower.
The earlier nine-repeat stride experiment met all six, illustrating why that
threshold should not be advertised as settled parity. Mimalloc and gperftools
TCMalloc still win substantially on several traces. Timing runs had no concurrent
proof or diagnostic workers; host frequency/SMT interference remain uncontrolled.

Separate FIFO-gated PMU runs (three repetitions, ten million pairs, counters
100% scheduled) corroborate reduced work. Instructions/pair fall from
160.219 to 153.219 on fixed64, 160.226 to 153.701 on small mix, 189.691 to
184.144 on broad mix, and 159.226 to 152.697 on bimodal. Measured cycles/pair
also fall on all four. Cache bookkeeping raises small-mix branch misses from
about 8 to 25 per thousand pairs, so this is not another branch-miss elimination
result. Broad-mix cycle samples still attribute about 16.8% to slab scanning;
sample attribution is approximate and does not prove causality per instruction.

**Proof and regression:** 244 checks (66 flow, 178 prover), zero unproved or
justified; all four units analyzed. Cached entries are proved distinct, in range
and non-live. Refill is proved to find an entry whenever capacity remains;
cache eviction cannot lose allocatable space. Bounds-enabled hosted regressions
include sparse cache overflow, duplicate release while full, complete shuffled
reuse, partial-tail capacities, slab retyping and independent live intervals.
Release codegen rejects Ghost/assertion machinery and unexpected helpers.

A separate model using ordinary Boolean membership and Ada bounded vectors
matched six million measured allocations at exact addresses, plus setup/warmup.
Refills per million: fixed64 **0**, fixed256 **13**, small **160**, broad **5**,
boundary **61**, bimodal **129**. These are model replay counts, not PMU events.
The broad mix still misses the slab hint 147,525 times per million and performs
4,334,571 model slab probes: indexing slab availability remains a useful next
experiment. No concurrency, large-object or native-runtime support was added.

**Slow-path caveat:** refill starts at slot one and can scan all 4096 positions.
Repeated cold fills rescan live prefixes. Warmed 1024-live-object churn does not
establish cold/burst latency, general fragmentation behavior or real-time bounds.
These should be measured before presenting the cache as production-ready.

Evidence under ignored `tests/performance/results/`: `allocator-batch-64/`,
`allocator-batch-separated/`, `allocator-batch-power2/`, `allocator-batch-final/`
and `allocator-batch-perf/`. Final artifacts preserve core sources, binaries,
baseline provenance, proof and replay logs. Core size is 461 lines across seven
files including Ghost code/comments; this is not a whole-library size comparison.

## Previous milestone: search-free slot stack with bitmap membership

Replaced each slab's next-bit hint and bitmap searches with a bounded stack of
16-bit free-slot indices. Initial slots are ascending; returned slots are reused
LIFO. The bitmap remains the exact live-membership map for checked release and
the population invariant. No payload links, raw pointers, C, assembly, intrinsics,
extra defensive guards or weakened public contracts were introduced.

Full proof: **231 checks (66 flow, 165 prover), zero unproved or justified**,
all four units analyzed. The reduced count reflects removal of search helpers;
the public allocation/release/lifetime contracts remain unchanged. The new
Ghost invariant proves active free-stack entries are in range, non-live and
pairwise distinct. All existing regressions pass, plus shuffled release/reuse
of all 4096 slots and six million exact-address comparisons against an independent
model using a bump cursor for fresh slots and a stack of returned slots. The
obsolete private bitmap-search observers were replaced, not left disconnected.

Fifteen interleaved repetitions, CPU 7, one million free/allocate/touch pairs,
against the immediately preceding unsigned-geometry binary:

| Trace | Bitmap baseline ns/pair | Stack ns/pair | Improvement | jemalloc | mimalloc |
|---|---:|---:|---:|---:|---:|
| Fixed 64 | 8.789 | 9.473 | -7.8% | 7.578 | 6.809 |
| Fixed 256 | 9.330 | 9.894 | -6.0% | 9.357 | 8.714 |
| Small mix | 17.322 | 11.400 | 34.2% | 10.334 | 7.196 |
| Broad mix | 23.687 | 20.276 | 14.4% | 17.424 | 18.609 |
| Boundary | 18.801 | 12.754 | 32.2% | 12.372 | 9.405 |
| Bimodal | 16.514 | 11.911 | 27.9% | 11.085 | 8.626 |

The first nine-repeat comparison measured the same tradeoff: mixed-trace gains
13.8–34.3%, fixed-size regressions 6.2–8.0%. Three traces are within 10% of
jemalloc; small mix is 10.3% slower, broad mix 16.4%, fixed64 25.0%. This is not
overall parity. A separate direct pool-capacity predicate experiment proved but
hurt several traces; reverted completely before the final run.

Separate gated hardware counters, three shuffled repetitions of ten million
pairs (all groups 100% scheduled), confirm the intended work reduction:

| Trace | Instructions/pair before -> after | Branch misses/1,000 pairs before -> after |
|---|---:|---:|
| Fixed 64 | 161.219 -> 160.219 | 7.890 -> 7.904 |
| Small mix | 189.609 -> 160.226 | 672.173 -> 8.054 |
| Broad mix | 210.167 -> 189.691 | 1,787.831 -> 1,263.241 |
| Bimodal | 180.897 -> 159.226 | 647.671 -> 140.386 |

The small mix loses about 98.8% of its branch misses and 15.5% of its executed
instructions. Fixed-size cycles increase despite one fewer instruction per
pair: eliminating a search that almost never happened does not repay the stack's
extra memory traffic/dependencies. Cycle samples show no slot-search helper;
the remaining slab scan is roughly 16% of broad-mix samples. Attribution is
approximate, not exact per-instruction cycle accounting.

**Space cost:** per-arena metadata rises from 135,464 to **2,230,568 bytes**
(13.3% of the 16 MiB payload backing), because every slab reserves 8192 bytes
for indices regardless of its current class. Initialization/reconfiguration
also writes more metadata. This remains a bounded hosted research baseline,
not a production/native integration decision. Next experiments should address
that footprint and indexed slab availability; a tree is not needed for selecting
an individual fixed-size slot. Core source is 430 lines across seven files,
including comments/Ghost code; not a whole-library size comparison.

Evidence under ignored `tests/performance/results/`: `allocator-free-stack/`,
`allocator-free-stack-capacity/` (rejected), `allocator-free-stack-final/`, and
`allocator-free-stack-perf/`. Final artifacts preserve exact core, binaries,
proof/regression logs and replaced observers. Profiler and proof workers had
finished before final timing; CPU frequency and SMT interference remain
uncontrolled. No compiler flags changed; no new concurrency or raw-pointer
client-lifetime guarantee is claimed.

## Previous milestone: unsigned geometry behind bounded integer interfaces

Page/local-offset decomposition and the size-class quotient now use unsigned
32-bit arithmetic internally. Exact integer division/remainder contracts remain
the interface to callers. No bitmap algorithm, validation, metadata layout or
compiler flag changed. Release code loses the signed-rounding corrections;
`ca_free` shrinks from 246 to 214 bytes. `ca_malloc` remains 531 bytes.

Full proof: **256 checks (72 flow, 184 prover), zero unproved or justified**,
all four selected units/instances analyzed, with the 60-second solver budget.
The quotient equivalence took up to 52 seconds. Exhaustive checks of every
offset in the 16 MiB arena were added; quotient/alignment, lifetime/membership,
release payload and six-million-operation independent replay tests also pass.
No Assume, SPARK-off, extra guards or weakened contracts were added.

Interleaved timings against the exact preceding binary, CPU 7, one million
free/allocate/touch pairs, fifteen repetitions (proof/profiling finished first):

| Trace | Baseline ns/pair | Unsigned ns/pair | Improvement |
|---|---:|---:|---:|
| Fixed 64 | 9.325 | 8.658 | 7.2% |
| Fixed 256 | 9.789 | 9.063 | 7.4% |
| Small mix | 18.631 | 17.255 | 7.4% |
| Broad mix | 24.890 | 23.811 | 4.3% |
| Boundary | 20.153 | 18.800 | 6.7% |
| Bimodal | 18.024 | 16.550 | 8.2% |

The first nine-repeat run improved five traces by 5.2–9.1%, but broad mix
regressed 1.3%; do not promise a broad-mix win from this alone. Separate gated,
interleaved hardware counters (ten million pairs, three repetitions) reduced
executed instructions/pair: fixed64 169.219 -> 161.219, small 197.609 -> 189.609,
broad 218.667 -> 210.167, bimodal 189.023 -> 180.898. Branch misses stayed roughly
unchanged. This is an instruction reduction, not a bitmap-search optimization.

Evidence: ignored `allocator-unsigned-geometry/`, `allocator-unsigned-geometry-final/`
and `allocator-unsigned-geometry-perf/` under `tests/performance/results/`.
The final directory preserves baseline/candidate binaries, exact core, proof
report and regression logs. Hosted prototype only; frequency and SMT activity
remain uncontrolled. No native CuBit allocator integration or production parity
is claimed. The profiling runner now accepts `--baseline` for interleaved
comparisons with an older benchmark that supports the same FIFO gate.

## Hardware profiling: bookkeeping and branches, not a demonstrated cache deficit

No allocator or release compiler options changed in this round. Added optional
FIFO gating to the Linux-hosted Rust benchmark and a `profile.py` runner, so
hardware counters exclude trace construction, integrity checks, warmup and
final sorting. Existing timers, payload touches and loop bookkeeping remain
inside scope, as does constant handshake overhead. This is not malloc-only cost.

AMD Ryzen 7 5800X, CPU 7, ten million free/allocate/touch pairs per process,
three shuffled repetitions for each counter group. All counter groups ran at
100% (no multiplexing). Medians:

| Trace | Allocator | Cycles/pair | Instructions/pair | Branch misses/1,000 pairs | L1 data misses/pair |
|---|---|---:|---:|---:|---:|
| Fixed 64 | CuBit | 41.71 | 169.22 | 7.88 | 1.47 |
| Fixed 64 | mimalloc | 31.28 | 104.94 | 28.64 | 1.76 |
| Fixed 64 | jemalloc | 35.56 | 114.01 | 9.10 | 1.55 |
| Small mix | CuBit | 85.38 | 197.61 | 670.11 | 3.03 |
| Small mix | mimalloc | 33.10 | 81.56 | 21.63 | 3.65 |
| Small mix | jemalloc | 47.21 | 114.23 | 11.17 | 2.47 |
| Broad mix | CuBit | 111.71 | 218.67 | 1,787.11 | 3.15 |
| Broad mix | mimalloc | 87.40 | 222.55 | 762.07 | 4.84 |
| Broad mix | jemalloc | 79.99 | 171.53 | 204.08 | 4.95 |
| Bimodal | CuBit | 82.43 | 189.02 | 644.95 | 1.84 |
| Bimodal | mimalloc | 38.99 | 99.57 | 221.82 | 3.06 |
| Bimodal | jemalloc | 49.77 | 122.03 | 31.89 | 2.18 |

Separate cycle-sampling runs (no lost samples) attributed approximately:

- Small mix: 40% allocation body, 27% release, 18% bitmap search,
  13% harness. Allocation body excludes the out-of-line search helper.
- Broad mix: 37% allocation body, 17% release, 12% slab search,
  10% bitmap search, 22% harness.
- Fixed 64: 48% release, 32% allocation body, 16% harness.

These are sampled instruction-pointer shares, not exact cycle accounting or
call-tree inclusive costs. Generic branch-miss samples were also collected,
but their diffuse attribution does not reliably identify the offending branch;
do not infer per-instruction causality from skid-prone sampling. The counter
totals establish substantially more branch misses in our mixed workloads.

The evidence favors targeted work, not an immediate wholesale redesign:

1. Simplify release-path offset geometry while preserving validation. Assembly
   still includes signed division/remainder correction sequences for values
   whose domain is nonnegative. A proved unsigned implementation behind the
   existing bounded interface is a candidate, not a measured improvement yet.
2. Reduce bitmap search branches. Full-word skipping is present, but a partial
   word still uses a bit-at-a-time loop; masked bit selection is worth testing.
3. Revisit indexed slab availability for broad size mixes, retaining empty-only
   retyping and failure/frame invariants. Prior cache experiments did not show a
   reliable gain, so additional metadata must earn its keep.

CuBit has fewer measured L1 data misses per pair than mimalloc on these traces;
this does **not** exclude cache latency, store effects or dependency-chain stalls,
but does not support blaming the gap on a simple excess of L1 misses either.
No lock/concurrency cost exists in this single-owner prototype. O3's previous
lack of a consistent win still stands; no LTO/PGO or new compiler claim is made.

Raw evidence: ignored `tests/performance/results/allocator-perf-gated-v2/`
(counters/cycle samples) and `allocator-perf-branches/` (branch samples, plus
an identical preserved executable). The former records the pre-hardening
runner hash; the final runner additionally preserves its executable and rejects
unsupported/multiplexed counters automatically. CPU pinning is not isolation,
frequency/SMT effects remain uncontrolled, and these instrumented runs do not
replace uninstrumented performance comparisons. Release codegen and payload
regressions passed; allocator sources were unchanged, so no new proof run or
additional proved property is claimed.

## Previous milestone: compact typed search results

This round retains one small change: internal bitmap searches return a bounded
`Slot_Search_Result` (`No_Free_Slot` or a valid one-based slot), rather than two
outputs that GNAT packed into a return value. The public allocation API and all
lifetime/membership/failure contracts are unchanged. No C, assembly, unchecked
conversion, extra cache or runtime guard was added.

The observed benefit is modest and workload-dependent. Two interleaved runs
(nine, then fifteen repetitions; one million free/allocate/touch pairs per run;
CPU 7) compared against the **previous word-bitmap implementation**, not the
older flat-bitmap baseline. The final run, nanoseconds per pair:

| Trace | Word-bitmap baseline | Compact result | Improvement | jemalloc | mimalloc |
|---|---:|---:|---:|---:|---:|
| Fixed 64 bytes | 9.276 | 9.255 | 0.2% | 7.613 | 6.851 |
| Fixed 256 bytes | 9.541 | 9.626 | -0.9% | 9.175 | 8.782 |
| Uniform 1–128 bytes | 18.464 | 17.859 | 3.3% | 10.345 | 7.249 |
| Uniform 1–4096 bytes | 24.375 | 24.357 | 0.1% | 17.618 | 18.700 |
| Class-boundary sizes | 19.282 | 19.171 | 0.6% | 12.469 | 9.488 |
| Small/large bimodal | 17.691 | 17.179 | 2.9% | 11.077 | 8.690 |

The first run measured 3.8% and 3.1% gains on small/bimodal, and a 1.0% fixed-256
regression. This is not an across-the-board improvement. Host load was lower
than during the preceding milestone (about 2–3), so compare within each run,
not absolute nanoseconds across rounds. The CPU/SMT sibling and frequency remain
uncontrolled. Proof workers finished before timing. All four reference backends
were included; mimalloc is still substantially faster on several workloads.

Verification: **243 checks: 72 flow, 171 prover, zero unproved or justified**,
all four units analyzed, with the 60-second solver budget. Existing exhaustive
hole/boundary tests, independent membership/lifetime regressions, release payload
tests and six-million-allocation scalar replay pass. Release inspection verifies
that the output-packing sequence disappears; `Scan_Free` shrinks from 453 to 413
bytes and `ca_malloc` from 551 to 531 bytes on this build. This is not a size claim
about the whole library. Core source grows from 463 to 469 lines, including blank
lines, comments and Ghost code; per-arena metadata remains **135,464 bytes**.

Research and rejected experiments:

- Mimalloc's [page layout](https://github.com/microsoft/mimalloc/blob/main3/include/mimalloc/types.h)
  deliberately groups frequently used fields, and its
  [allocation path](https://github.com/microsoft/mimalloc/blob/main3/src/alloc.c)
  attempts a local page allocation before falling back to generic allocation.
  These were architectural references, not copied C code or new dependencies.
  The inspected v3 source is distinct from the Nix-pinned benchmark library;
  benchmark versions were not changed.
- Moving our counters/hint before the bitmap added no state, but slightly
  regressed mixed traces. Reverted after proof and timing.
- Attempting the current slab before separately checking capacity passed runtime
  regressions but left the composed preservation contract unresolved. Early
  return and a single whole-heap failure-state lemma were also tried. The lemma
  proved, but the full frame obligation did not; all of this variant, including
  its Ghost snapshot/assertion, was removed. No timing claim is made for it.

Evidence is under ignored `tests/performance/results/allocator-compact-search/`
and `allocator-compact-final/`; the latter preserves the exact retained core,
candidate and baseline executable/provenance, proof report and regression logs.
The unsuccessful layout comparison is in `allocator-header-first/`.
The implementation remains a bounded single-owner Linux-hosted pilot. Its small
source size should not be compared as though it already implements mimalloc's
concurrency, large/over-aligned allocations, realloc or OS-backed heap growth.

## Previous milestone: proved word-oriented bitmap and bounded indexing

Retained a 64-bit-group bitmap, full-word skipping, and unsigned index arithmetic
behind proved bounded-integer interfaces. The membership implementation stays
behind the bitmap API, so callers reason about its contract rather than nested
array/modular details. Allocation/release/lifetime contracts are unchanged.
No C, inline assembly, unchecked conversion, assumption or SPARK-off core was
introduced. This is still a single-owner **Linux-hosted** prototype, not native
CuBit runtime integration or a production general-purpose allocator.

Whole-word skipping with signed indexing alone was approximately a wash. The
combined word/index implementation improved all six median comparisons in two
interleaved runs, but exact gains vary substantially under unrelated host load.
The repeat used CPU 7, eleven independent runs per cell, 1,000,000 pairs per run,
and the preserved pre-experiment executable. All four reference allocators ran;
proof workers had finished before timing. Nanoseconds per free/allocate/touch
pair, including harness bookkeeping:

| Trace | Baseline | Word bitmap | Improvement | jemalloc | mimalloc |
|---|---:|---:|---:|---:|---:|
| Fixed 64 bytes | 13.41 | 11.45 | 14.6% | 9.32 | 8.55 |
| Fixed 256 bytes | 14.03 | 11.60 | 17.3% | 10.63 | 11.63 |
| Uniform 1–128 bytes | 23.79 | 20.73 | 12.9% | 11.93 | 8.64 |
| Uniform 1–4096 bytes | 37.00 | 36.40 | 1.6% | 20.85 | 24.85 |
| Class-boundary sizes | 24.23 | 22.08 | 8.9% | 14.40 | 11.46 |
| Small/large bimodal | 22.47 | 19.96 | 11.2% | 12.29 | 11.18 |

The preceding nine-repeat run measured gains of 13.8%, 9.7%, 12.1%, 15.0%, 1.8%
and 11.6% respectively. In particular, do not promise the mixed trace's earlier
15% gain. Host load averages during these runs were around 7–9; CPU pinning is
not isolation, and neither frequency nor SMT interference was controlled.
Fixed-256 happened to be within 10% of jemalloc and tied mimalloc in the repeat;
that is **not** an overall target achievement. Small mixed traces remain roughly
twice or more mimalloc's cost. These measurements support retaining the simpler
word/index change, not claiming production parity.

Verification: **238 checks: 72 flow, 166 prover, zero unproved or justified**,
all four units analyzed. The 15-second suite passed earlier; a later rerun left
the unchanged alignment lemma unresolved. Rechecking with a 60-second budget
passed without a source/contract change. Use the documented timeout override
on a busy host; do not suppress unproved checks.

Regressions check every sole-hole position in both search directions at ten
boundary capacities, plus the existing geometry, independent membership,
mixed-lifetime and release-payload tests. The scalar oracle matched all six
million allocation outcomes/offsets, independently of the word-search logic.
Its counters remain scalar reference distances, not native word-operation
counts. Release inspection shows native 64-bit full-word comparisons and
shift/mask indexing, with no packed-slice runtime helpers or Ghost machinery.

Word alignment increases metadata from 134,436 to **135,464 bytes** (+1,028),
still about 0.8% of the fixed 16 MiB backing. Size classes and rounding waste
are unchanged. A separate enum-indexed geometry-table experiment removed some
generated branches but left proof obligations unresolved; it was reverted, not
shipped or counted as performance evidence. An exposed unsigned-index attempt
was stopped when proof search became expensive; the retained bounded interfaces
fully prove the conversion instead.

Evidence: `tests/performance/results/allocator-word-scan/` (signed grouping),
`allocator-word-index/` (nine repeats), and `allocator-word-final/` (eleven).
The final directory preserves the candidate, unchanged baseline/provenance,
proof report, diagnostics and logs. No kernel API or native allocator changed.
Next candidates are denser slab metadata/availability lookup and reducing
slow-path result/call overhead; finer size classes are also needed for the
rounding gap. None is presumed faster before measurement and full proof.

## Previous experiment: search diagnostics, no retained allocator change

We tested additional slab/slot caches and packed-bitmap block skipping. None
delivered a reliable overall win, so **all experimental core changes were
removed**. The core source hashes match the preceding hot-path refinement.
Retained changes are a passive diagnostic replay, explicit benchmark CPU
selection, and stronger release-code dependency checks. This remains a
Linux-hosted prototype; no CuBit runtime or kernel API changed.

The replay checks its predicted allocation outcome and exact offset against
the real implementation for six million measured operations, plus setup.
Baseline observations per million allocations:

| Trace | Slab hint misses | Slot scans | Mean slab probes | Mean slot probes |
|---|---:|---:|---:|---:|
| Fixed 64 bytes | 0 | 0 | 1.00 | 1.00 |
| Fixed 256 bytes | 391 | 252 | 1.00 | 1.02 |
| Uniform 1–128 bytes | 148 | 269,344 | 1.00 | 4.50 |
| Uniform 1–4096 bytes | 147,525 | 213,890 | 4.33 | 2.72 |
| Class-boundary sizes | 17,166 | 296,130 | 1.20 | 3.29 |
| Small/large bimodal | 109 | 222,783 | 1.00 | 3.05 |

These count logical predicates, not machine instructions or memory accesses.
The observer validates selection behavior, not the optimizer's execution cost.

- A second validated slab hint reduced mixed-trace full searches from 14.75%
  to 4.46%, but did not produce a consistent timing improvement.
- A speculative second slot hint reduced some scans without a broad timing win.
- A proved-free slot cache removed the bitmap read on cache hits, including all
  measured fixed-64 allocations. Even that did not improve fixed-64 timing and
  regressed several mixed traces. Its added invariant fully proved before timing;
  it was rejected for performance, not weakened to satisfy the prover.
- Packed-array slice skipping introduced GNAT bit-comparison/copy helpers rather
  than cheap word operations, failed the standalone benchmark link, and left one
  functional obligation unproved. It was discarded, not treated as a candidate.

The decisive cache repeat used CPU 7, eleven independent runs per cell and one
million free/allocate/touch pairs per run, interleaving unchanged baseline,
candidate and all four reference allocators. Proofs finished before timing.
The CPU was quieter in a brief host snapshot, **not isolated**; frequency,
interrupts and SMT interference remain uncontrolled. Nanoseconds per pair:

| Trace | Unchanged baseline | Rejected free-slot cache | jemalloc |
|---|---:|---:|---:|
| Fixed 64 bytes | 10.4 | 10.4 | 7.6 |
| Fixed 256 bytes | 10.8 | 10.7 | 9.1 |
| Uniform 1–128 bytes | 21.4 | 22.8 | 10.3 |
| Uniform 1–4096 bytes | 26.2 | 26.6 | 17.6 |
| Class-boundary sizes | 21.6 | 23.7 | 12.4 |
| Small/large bimodal | 20.2 | 21.4 | 11.0 |

After reverting, the Nix suite again passes **211 checks: 67 flow, 144 prover,
zero unproved or justified**, with all four units analyzed. Six-million-operation
diagnostics, model/lifetime tests, release payload checks and the codegen audit
pass. The final 1,000-pair smoke run is a build/integrity check, not performance
evidence. The 10% jemalloc goal remains unmet.

Next investigate word-oriented bitmap representation and generated instructions,
including the fixed-size fast path that never scans. Fewer logical probes alone
are not evidence that an optimization is faster. Diagnostic evidence is archived
under ignored `tests/performance/results/allocator-search-diagnostics/`; timing
data for the rejected cache is in `allocator-free-cache-cpu7/` alongside the
other experiment directories. The earlier proof and performance milestones below
remain historical evidence.

## Previous milestone: measured hot-path refinement

The within-10%-of-jemalloc goal is **not yet met**. The current implementation
retains the dynamic-slab lifetime guarantees and improves several traces by
replacing request-class branches with a 256-byte lookup, splitting rare scans
out of the inline hint paths, using proved alignment masks, and replacing
slab-local division with exact fixed-point scaling. The release default remains
O2: an interleaved same-source O2/O3 comparison found no consistent broad gain.

The full Nix suite passes **211 checks: 67 flow, 144 prover, zero unproved or
justified** across all four selected units/instances. The changed count reflects
different implementation obligations, not removed lifetime/membership contracts.
No skipped units, `pragma Assume`, or SPARK-off core were introduced. The model,
mixed-lifetime, geometry, release-mode payload and codegen audits pass. Ghost
proof machinery remains absent from the archive.

### Interleaved comparison

Seven independent runs per workload/backend, 1,000,000 pairs per run, same
non-isolated Ryzen 5800X host and reference versions described below. Proofs
completed **before** timing. Both CuBit executables use O2. The previous dynamic
slab executable was preserved and interleaved with current/reference runs.
Units are nanoseconds per free/allocate/touch pair, not per malloc.

| Trace | Previous slabs | Current slabs | jemalloc | Current / jemalloc |
|---|---:|---:|---:|---:|
| Fixed 64 bytes | 17.2 | 10.2 | 7.4 | 1.38 |
| Fixed 256 bytes | 12.6 | 10.7 | 9.0 | 1.19 |
| Uniform 1–128 bytes | 24.7 | 21.0 | 10.1 | 2.08 |
| Uniform 1–4096 bytes | 26.7 | 25.7 | 17.0 | 1.51 |
| Class-boundary sizes | 28.7 | 21.3 | 12.0 | 1.77 |
| Small/large bimodal | 22.5 | 19.7 | 11.2 | 1.76 |

Host variability remains visible even with interleaving: the same previous
64-byte executable measured 12.3 ns in the earlier O3 comparison, versus 17.2 ns
here. Do not promote that cell's apparent 41% speedup into a general claim.
The other old/new comparisons show roughly 4–26% improvement in this run.
The closest jemalloc comparison is still about 19% slower, and the small mixed
trace is about twice its cost. Rounding overhead and metadata consumption are
unchanged; the mixed trace still wastes 33.8% through size-class rounding.

Further gains need measurement of scan frequency/cost and investigation of
availability indexing/bitmap search, plus finer size classes. Richer native
backing-region and large-page support is a separate planned boundary; this
preallocated hosted test cannot demonstrate its benefit. No native runtime or
kernel API has been changed by this optimization round.

Raw data for all six backends (including glibc, mimalloc and gperftools TCMalloc)
is under `tests/performance/results/allocator-hot-final/`. It includes a copy
of `baseline-bench` and matching `baseline-provenance.json`; the earlier
executable's hash is `0eb7d851470c5094155609dbb7ea7422c22fa54f8b0b1bc71f4c88ff9f866fff`.
The final suite summary is in `build/slabs/gnatprove/gnatprove.out` beneath
`tests/userspace-allocator`. `allocator-hot-path-o2-v-o3` compares the same
source at O2 and O3; its `cubit-baseline` means O3, not the older slab source.

Unsigned bitmap indexing and general unsigned shift/division experiments were
discarded when their complete proofs did not discharge. Their intermediate
benchmark directories are experiments, not verified release candidates.

## Earlier milestone: dynamically assigned slabs

The fixed-per-class implementation below has been **replaced**, not retained as
a second backend. The current core shares 256 × 64 KiB slabs between all nine
size classes and permits reassignment only after the last live block is freed.
It remains a single-owner **Linux-hosted** prototype, not CuBit runtime wiring.

Evidence from `nix develop -c bash tests/userspace-allocator/run.sh`:

- **212 checks: 60 flow, 152 prover, zero unproved or justified.** Four selected
  units/instances have nonempty proof coverage; no skipped units or assumptions.
  These are checks, including repeated instantiations, not 212 separate properties.
- The bitmap's live count equals its actual population. Allocation/release have
  exact membership effects. A nonempty pool cannot be reconfigured; failed
  operations leave state unchanged. Composed slab operations preserve live
  allocations and their classes, with aligned, disjoint, in-bounds block geometry.
- Tests cover every request size and slab-local geometry boundary, 20,000 bitmap
  model operations, 30,000 mixed-lifetime operations against independent live
  intervals/counts, partial-slab refusal and empty-slab reassignment.
- Hosted release-mode payload/FFI tests fill the whole arena, verify every byte,
  reject invalid/interior/immediate duplicate frees, and reuse it. A single
  class can now consume the full backing region rather than hitting a quota.
- Release codegen inspection finds no Ghost/assertion machinery. Constant-divisor
  geometry and cross-unit inlining are used; successful free updates the next
  slab hint, which is still validated before allocation.

Backing is 16,777,216 bytes, metadata 134,436 bytes on the tested x86-64 host
(about 0.8%). Empty-slab reuse is not OS decommit/reclamation. Allocation slow
paths still scan slabs/bitmap slots; finer classes and availability indexing
remain future work. Power-of-two rounding remains 33.8% on this mixed trace.

### Current comparison

Same host/methodology and reference versions as the historical run below:
seven independent processes per cell, 1,000,000 pairs each, CPU 0 affinity,
non-isolated host. **Nanoseconds per free/allocate/touch pair**, lower is better.

| Trace | glibc | mimalloc | gperftools TCMalloc | jemalloc | SPARK slabs |
|---|---:|---:|---:|---:|---:|
| Fixed 64 bytes | 6.2 | 6.8 | 6.3 | 7.5 | 12.4 |
| Fixed 256 bytes | 6.5 | 8.7 | 6.7 | 9.1 | 12.8 |
| Uniform 1–128 bytes | 10.0 | 7.0 | 6.8 | 10.3 | 25.1 |
| Uniform 1–4096 bytes | 54.0 | 18.3 | 9.0 | 17.4 | 27.1 |
| Class-boundary sizes | 19.9 | 9.3 | 8.0 | 12.3 | 29.1 |
| Small/large bimodal | 20.1 | 8.6 | 7.4 | 10.9 | 22.9 |

Dynamic reuse costs performance relative to the fixed-partition pilot. The
current implementation beats glibc on the mixed trace, but trails the other
references there and trails all references on the other traces. This is a
capacity/lifetime correctness milestone, **not a blanket speedup**. Further
optimization and full runtime semantics are needed before recommending adoption.
Sampled slab p99 ranges 40–90 ns with a 20 ns timer floor, not a latency guarantee.

Initial dynamic runs exposed extra helper calls, variable division and an
allocation hint that did not follow successful release. Optimized results are
recorded in `tests/performance/results/allocator-dynamic-hint/`; earlier runs
(`allocator-dynamic-slabs`, `allocator-dynamic-inline`,
`allocator-dynamic-optimized`) used shorter traces and saw significant host
variation, so their raw differences are not controlled speedup measurements.
The final proof report is `tests/userspace-allocator/build/slabs/gnatprove/gnatprove.out`.
All builds, regressions, proofs and comparisons used Nix.

## Historical fixed-partition pilot

The remaining sections document the earlier implementation and measurements.
They are retained as experimental history, **not claims about the current core**.

### Status and conclusion

The bounded single-owner SPARK core is implemented, proved and exercised through
a real hosted address/FFI adapter. It is **not** a production allocator and is
not installed in CuBit's Rust or GNAT runtime.

It is competitive on some narrow allocation traces after a code-generation fix,
but is not an overall winner. Finer size classes and dynamic page assignment are
more important next steps than claiming victory from a fast fixed-size loop.

## Correctness evidence

- GNATprove reports **132 checks: 40 flow, 92 prover, zero unproved/justified**.
  These include repeated generic instances, not 132 distinct security properties.
- All five requested units/instances are analyzed, with nonempty proof coverage,
  no skipped subprograms and no `pragma Assume`. The core contains no SPARK-off
  implementation. A checker rejects vacuous/skipped proof success.
- Allocation/release preserve exact slot membership and validity; failed
  operations preserve the state; distinct blocks have disjoint byte ranges;
  starts are aligned/in bounds; every valid block start decodes exactly.
- Hosted assertion/overflow-check-enabled tests pass: 100,000 model operations,
  capacity one, all request sizes, all block starts, each class exhausted/reused,
  interior offsets and immediate duplicate release.
- Release-mode Ada/host ABI test passes: all 18,432 slots allocated together,
  every payload byte filled/verified with block-specific patterns, invalid
  foreign/interior/double frees rejected, then full capacity reused.
- Release archive inspection finds no Ghost/assertion machinery; the core hot
  path is inlined into the bridge. The x86-64 release path has no padded-result
  stack temporaries after the representation fix.

This does not prove raw-pointer caller lifetimes, platform page management,
concurrent mutation, arbitrary client writes or correctness of the toolchain.
An old pointer can still target a new allocation at the same address. See the
[explicit proof boundary](../userspace/allocator/README.md).

Tools: GNATLS 16.1.0, GNATprove FSF 15.0 / Why3 1.7.1+git, CVC5 1.3.4 and
Z3 4.16.0; Rust 1.98.1. Builds, proofs and benchmarks run through Nix.

## Single-owner comparison

AMD Ryzen 7 5800X, Linux x86-64, CPU 0 affinity, frequency scaling enabled,
non-isolated host. Each cell is the median of seven independent processes,
1,000,000 allocation/free pairs per process. All five backends execute identical
precomputed traces with 1024 live positions, an untimed integrity pass and warmup.
Each pair includes free, allocation, first/last volatile payload touches and
harness bookkeeping. Lower is better; **nanoseconds per pair**, not per malloc.

Versions: glibc 2.42, mimalloc 3.3.2, jemalloc 5.3.1,
**gperftools TCMalloc 2.18.1**. This is not Google's newer per-CPU TCMalloc.
All references use their packaged defaults, not matched hardening policies.

| Trace | glibc | mimalloc | gperftools TCMalloc | jemalloc | SPARK pilot |
|---|---:|---:|---:|---:|---:|
| Fixed 64 bytes | 6.0 | 6.7 | 6.1 | 7.5 | 6.7 |
| Fixed 256 bytes | 6.3 | 8.5 | 6.6 | 8.9 | 7.2 |
| Uniform 1–128 bytes | 9.7 | 7.1 | 6.7 | 10.0 | 12.9 |
| Uniform 1–4096 bytes | 51.8 | 17.8 | 8.7 | 16.5 | 13.0 |
| Class-boundary sizes | 19.3 | 9.3 | 7.9 | 12.2 | 18.7 |
| Small/large bimodal | 19.6 | 8.4 | 7.2 | 10.7 | 12.6 |

These comparisons omit heap growth, large allocations, over-alignment, realloc,
concurrency and long-running fragmentation. The pilot reserves a fixed arena;
the references support far broader workloads. Do not call this an overall
allocator speedup, or extrapolate it into a native application speedup.

The pilot's sampled p99 values range from 30 to 50 ns here. The timer-only median
is 20 ns, and each pair is sampled only every 128 operations. These are coarse,
host-dependent observations including clock overhead—not latency guarantees or
evidence that all allocator slow paths have been exercised.

A second full run (`allocator-confirmation`) retained the code-generation gain
but exposed non-isolated-host variability: the pilot's 64-byte/mixed medians
were 7.0/12.9 ns, while its 256-byte median rose to 10.5 ns. Some reference
medians also rose substantially (for example, glibc mixed: 51.8 to 76.2 ns).
Do not cherry-pick a universal ranking from the first table. A dedicated,
frequency-characterized host is needed for publication-quality comparisons.

### A real code-generation improvement

The initial composed core incurred padded temporary copies for its tagged
decode result, even after inlining. An explicit eight-byte record layout kept
the variant semantics and removed those temporaries. The same 132 checks pass.

| Trace | Before compact result | After | Unit |
|---|---:|---:|---|
| Fixed 64 bytes | 19.2 | 6.7 | ns/pair |
| Fixed 256 bytes | 20.7 | 7.2 | ns/pair |
| Mixed 1–4096 bytes | 24.2 | 13.0 | ns/pair |

This was representation/codegen overhead, **not executable proof overhead**.
Ghost predicates were already absent. Earlier changes enabled explicit
cross-unit inlining and removed variable integer division from release using
proved constant-divisor equivalents; those alone produced much smaller gains.

### Memory cost remains a significant weakness

At the end of the mixed trace, `malloc_usable_size` reports the following live
rounding overhead: `(usable / requested - 1) * 100`.

| glibc | mimalloc | gperftools TCMalloc | jemalloc | SPARK pilot |
|---:|---:|---:|---:|---:|
| 0.4% | 8.4% | 7.1% | 8.4% | 33.8% |

This is **not total fragmentation**: it excludes metadata, free pages and caches.
The pilot also reserves 16,744,448 payload bytes and 147,492 metadata bytes,
with a fixed limit of 2048 slots in each class. It cannot move unused capacity
between classes. Whole-process RSS includes about 16 MB of trace data at this
run length and must not be presented as heap-only memory use.

## Concurrency reference baseline

Separate experiment: five runs per cell; 256 rounds of 512 mixed-size allocations
per worker. Workers share a CPU affinity set using physical cores 0–3, without
individual pinning. Remote mode transfers batches around a channel ring so a
different live thread frees each producer's allocations.

Numbers are elapsed time divided by **all pairs across workers**: an aggregate
throughput metric, not per-operation latency. Remote mode includes channel and
synchronization cost. The SPARK pilot is excluded because it is single-owner.

| Scenario | glibc | mimalloc | gperftools TCMalloc | jemalloc |
|---|---:|---:|---:|---:|
| Local, 1 thread | 49.2 | 31.6 | 9.9 | 58.3 |
| Local, 2 threads | 25.9 | 19.9 | 7.0 | 31.4 |
| Local, 4 threads | 22.7 | 15.4 | 6.7 | 23.6 |
| Remote free, 2 threads | 98.1 | 24.3 | 16.5 | 44.9 |
| Remote free, 4 threads | 67.8 | 14.5 | 10.1 | 27.7 |

Batch behavior differs from immediate churn; these tables are not directly
interchangeable. The measurements reinforce the need to design remote frees
explicitly, but do not isolate allocator contention from channel/cache effects.

## Reproduction and evidence locations

See [commands and methodology](../tests/userspace-allocator/README.md).

- Optimized single-owner run: `tests/performance/results/allocator-compact/`
  (210 independent processes; full JSON and summary).
- Pre-compact comparison: `tests/performance/results/allocator-final/`.
- Independent optimized repeat: `tests/performance/results/allocator-confirmation/`
  (another 210 processes; variability discussed above).
- Concurrency references: `tests/performance/results/allocator-threads/`
  (100 independent processes).
- Proof report: `tests/userspace-allocator/build/gnatprove/gnatprove.out`.

Raw measurements/build outputs are intentionally gitignored. JSON records
include backend resolution, tool/library versions, CPU information and hashes;
the working tree was not committed. No native allocator integration or kernel
changes were made for this pilot. The unused `rlsf` experiment was removed;
the native Rust probe still builds offline and its two hosted API tests pass.

Next: dynamically assigned slabs and finer size classes, followed by backing
extent failure/reclamation tests. Preserve the existing proof and comparison
suite while expanding the supported allocator contract.
