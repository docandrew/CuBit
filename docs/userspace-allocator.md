# CuAlloc: a portable, verifiable userspace allocator

Status: bounded Linux-hosted benchmark plus native Rust opt-in trial, September
2026. Not a general runtime replacement or dynamic backing provider.

## Goal

Develop a statically linkable allocator suitable for CuBit's Ada and Rust apps,
with a portable SPARK metadata core and small platform boundaries. The same core
should be useful on Linux, Windows and macOS; those other adapters are not yet
implemented. No C allocator implementation is a native dependency. C/C++
allocators are useful experimental references, not code to quietly ship.

This is separate from the existing kernel physical allocator proof work.
Process heap metadata, page provisioning, and physical page allocation are
different proof boundaries. None becomes end-to-end proved by association.

The working hot-path target is within roughly 10% of jemalloc on comparable
single-owner workloads, with faster results welcome. Assess every trace, not
only an aggregate average. Also track rounding waste, retained memory and
slow-path/tail behavior; buying throughput with unbounded retention or weaker
invariants does not meet the target. This is an engineering target, not a claim
that the current pilot achieves it. Ada aliasing rules may aid optimization,
but SPARK proofs do not automatically become compiler optimization facts.
The measured release build already suppresses runtime checks and erases Ghost
code; retain the proofs and inspect actual generated code.

## Architecture and invariants

1. **Portable metadata core:** sizes, alignment, slots/spans, availability and
   ownership transitions. No raw addresses or OS calls. Success yields a unique
   live block of adequate size; failure preserves existing allocations; release
   changes only the intended live block; live byte ranges do not overlap.
2. **Page provider:** acquire aligned regions, and eventually decommit/release
   them. It guarantees disjoint backing, valid lengths, checked address arithmetic
   and appropriate zeroing before cross-process reuse. The core must handle
   provider failure without losing live allocations. CuBit's grow-only heap
   interface needs a separate reclamation design before promising returned pages.
3. **Runtime adapters:** Rust `GlobalAlloc`/`Allocator` and Ada `System.Memory`
   translate their real size/alignment/OOM/realloc contracts, without imposing
   POSIX semantics on CuBit. A hosted C ABI is a test/interoperability boundary,
   not a requirement for internal implementation language.
4. **Concurrency protocol:** explicit owner heaps, remote-free transfer,
   ownership handoff, orphan collection and page retirement. Initially one owner;
   do not claim the sequential proof establishes concurrent safety.

The platform-independent part may use offsets and indices rather than exposing
SPARK access types throughout the metadata graph. SPARK ownership can be useful
in adapters/APIs, but proving allocator metadata cannot prove arbitrary raw
pointer clients avoid use-after-free. That is a separate caller obligation.

## Native backing regions: independent from the per-object hot path

Design direction, not an implemented kernel interface: CuBit need not preserve
`sbrk` semantics or imitate `mmap`. A native region request can express size,
virtual alignment, backing/residency requirements and page-size policy, returning
an owned region handle with its actual geometry. Integrate its authority and
quota checks with the existing model, not a second permission system.

Separate virtual-address reservation from supplying and retiring backing.
Consider base pages, preferred larger mappings with explicit fallback, and
required page sizes with a typed failure if unavailable. Physical contiguity
and virtual contiguity are distinct requirements; ordinary heaps should not
require physically contiguous backing unless their mapping policy needs it.
CuBit's buddy allocator can supply aligned physical extents, but the mapping
layer must actually install large-page entries to reduce TLB pressure. For
example, a 2 MiB mapping can back thirty-two 64 KiB userspace slabs.

Specify zeroing before cross-process reuse, quota accounting, all-or-nothing
publication/failure cleanup, and pin/TLB coordination before retiring backing.
Do not mix independently protected or independently revocable objects in a
large mapping without a safe splitting strategy. Preprovisioning can keep
region acquisition/zeroing out of latency-sensitive execution.

`handleSbrk` now maps individual pages with recoverable, failure-atomic growth:
unmap the newly added prefix, synchronize TLBs, release its frames, and leave
the break unchanged. Tracking headroom can grow, with policy quotas and
representability still checked. A richer provider remains useful for explicit
virtual reservation, alignment, large-page mapping and backing reclamation.
The current hosted
benchmark already has its backing reserved: richer kernel primitives will help
growth, reclamation and application memory access, but do not explain away its
measured per-object allocator costs.

## Current implementation

`userspace/allocator` now uses dynamically assigned 64 KiB slabs, each with a
packed 4096-bit membership map, runtime capacity, exact live count and a bounded
64-entry cache of 16-bit free-slot indices. A Ghost population-count invariant connects the counter to actual slot
membership: "empty" is not an unchecked cached claim. Allocation/release prove
exact membership changes; reconfiguration succeeds **only** on an empty pool.
Payloads contain no free-list links. Active cached indices are distinct, in
range and non-live; they represent a subset of free slots, not the whole set.
Popping needs no membership search; an empty cache refills from up to 64 free
bitmap positions in ascending order, then pops the last one. Release pushes its
slot, replacing the top entry when full. An evicted entry stays free in the
bitmap and is recoverable on refill. Failed/duplicate releases change no state.
The Ghost proof establishes refill can always find an entry when not full.
Allocation order is an implementation policy, not a public API promise.

`Heap_Slabs` composes those pools with nine 16–4096-byte classes. Each live slab's
class remains fixed. Different blocks have aligned, in-bounds, disjoint byte
intervals. Interior offsets and immediate duplicate releases preserve the state.
Per-class hints are checked against actual class/occupancy before use, so an
empty slab reassigned to another class cannot turn a stale hint into authority.
A successful release updates the class hint to the newly available slab.

The hosted instance has 256 slabs sharing a fixed 16 MiB backing region and
262,440 bytes of metadata (about 1.56%) on the measured x86-64 build, down from
2,230,568 bytes for complete free-index stacks. Each slab has 512 bitmap bytes,
128 cache bytes and its counters, padded to a 1 KiB object stride to simplify
address arithmetic. GNAT's unused-bits warning reports this intentional padding;
it is not suppressed. Keeping cache and occupancy counters apart also avoids
unhelpful paired SIMD updates on the tested compiler. These are measured layout
choices, not architecture-independent performance guarantees.
Initialization/reconfiguration and cold refills are not captured by steady-state
churn timings. There is no fixed per-class payload quota.
This is **not** OS-backed growth, decommit or reclamation: no large allocations,
realloc or alignment beyond 16 bytes yet. Partially occupied slabs still retain
their whole backing region; the core does not relocate live objects.

Release does constant work. Allocation uses hints, then a bounded slab scan
on a miss. The scan receives the already selected size-class enum, calculates
capacity once, and returns a single bounded optional slab reference instead of
a padded two-output aggregate. It checks up to eight slabs after the hint, then
falls back to the complete class/empty scans. This ordering heuristic introduces
no metadata or hot-path index updates and cannot hide usable capacity; it adds
up to eight probes in the worst case. The window is a measured tuning choice,
not a security boundary. Slot selection is a constant-work cache pop unless refill is needed.
A refill scans from slot one and can inspect all 4096 positions. Repeated cold
fills can therefore rescan live prefixes; this needs separate latency/burst
measurement, not extrapolation from warmed churn. Allocation is not constant-work
in the worst case. A dense per-class availability-index experiment was rejected:
its maintenance overhead outweighed eliminated searches, especially with slabs
oscillating between full and one-free-slot. Any future index must preserve the
same membership/lifetime properties and demonstrate a net workload-level gain.
The bitmap consists of fixed 64-bit packed Boolean groups for direct membership
and batch refill. The old per-allocation bitmap search helpers remain removed;
the public success/slot API is unchanged.
No unchecked representation conversion, platform intrinsic or inline assembly
is required. Its native instruction selection is checked on the hosted x86-64
build, not claimed for every future architecture.

Hot-path refinements use a 256-byte request-class lookup and split the scan
fallbacks out of the inline hint paths. Alignment masks and exact fixed-point
scaling replace the previous class-dependent arithmetic branches. The scaling
input is bounded to a slab plus its one-past-end capacity calculation, and its
contract proves equality to ordinary division without overflow. All stack
uniqueness/membership invariants are Ghost; no runtime invariant scan or extra
defensive full-count guard is needed on that path.

Bounded unsigned word/bit indexing now proves behind two small arithmetic
interfaces: their results equal ordinary integer division and remainder. This
avoids signed negative-index corrections in release code while keeping modular
arithmetic out of the composed proofs. Membership implementation details also
stay behind the bitmap API. Page/local-offset decomposition and fixed-point
quotients now use the same unsigned implementation pattern, with proved exact
integer results. The bounded quotient operands cannot wrap their 32-bit product;
this removes signed-rounding corrections without changing allocation policy.
Earlier exposed-modular and geometry-table
experiments were not retained when full proofs failed. No contract was weakened
or assumption added to rescue timings. O3 did not consistently improve
the same-source measurements enough to justify changing the O2 release default.

Ghost invariants and lemmas disappear in release code. Hosted tests retain
arithmetic/bounds checks and explicit regression checks, without executing the
recursive Ghost induction lemmas. An explicit eight-byte tagged allocation
result retains checked variant semantics without padded result temporaries.
Cross-unit inlining is measured and checked in the harness;
GNAT's [inlining controls](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html)
do not replace checking the generated code.

## Security and correctness boundaries

- Single-owner access and initialized valid state are required.
- No `pragma Assume` or SPARK-off regions in the core. Address/FFI code is
  explicitly outside that core, regression tested rather than mislabeled proved.
- A freed pointer reused after the address is reallocated can still target a new
  allocation. Detecting an immediate double free is not temporal memory safety.
- Out-of-band metadata is ordinary same-process memory, not a capability or
  hardware isolation boundary. Unsafe arbitrary writes can still corrupt it.
- Raw-pointer callers must free only their own live blocks. Guessing a valid
  block address cannot establish legitimate ownership.
- Bounded work is not a worst-case wall-clock bound: page faults,
  cache misses, preemption and the backing provider still matter.
- C ABI address validation must avoid wraparound before converting to the bounded
  offset types. Over-aligned and oversized requests must never silently truncate.
- Future realloc must preserve old storage on failure; zeroed allocation must
  reject multiplication overflow; failure policy must be explicit at each adapter.

## What the baseline tells us

The pilot is not generally faster than mature allocators. Dynamic slab reuse
adds bookkeeping and search costs absent from the original fixed partitions.
Power-of-two classes still use roughly one-third extra live capacity on the
mixed trace. Track this flexibility/performance tradeoff rather than presenting
the old pilot's timing as the new implementation's performance. See the
[dated measurements](userspace-allocator-results.md).

Keep comparing glibc, mimalloc, jemalloc and gperftools TCMalloc with the same
traces. The separate concurrency baseline is reference-only until our ownership
transfer protocol exists. Add modern Google TCMalloc as a distinct reference
when there is a reproducibly pinned build; do not relabel gperftools as it.

Useful design references, not implementation dependencies:

- [mimalloc](https://github.com/microsoft/mimalloc): page-local allocation and
  free-list sharding, explicit heaps and hardening tradeoffs.
- [TCMalloc design](https://google.github.io/tcmalloc/design.html): frontend
  caches, transfer layers and backing allocation; distinguish per-thread from
  newer per-CPU designs.
- [jemalloc](https://jemalloc.net/jemalloc.3.html): size classes, arenas,
  statistics and purge controls.
- [glibc allocation tunables](https://sourceware.org/glibc/manual/latest/html_node/Memory-Allocation-Tunables.html):
  capture defaults/settings when comparing rather than assuming identical policy.

## CuAlloc: one allocator for everything (user decision, 2026-10-05)

This allocator is **CuAlloc**. Every CuBit process uses it, whatever its
language:
- Ada, through `System.Memory` (allocators, `Unchecked_Deallocation`);
- C, through the libc `malloc` family, replacing musl's mallocng;
- Rust, through `GlobalAlloc`: Penny (the Servo browser), the fonts library,
  config-storage and the probe.

There are no other allocators. The sbrk bump allocator that Doom and SameBoy
used (`userspace/c/cubit_mem.c`) is gone with the old freestanding libc.

- **One heap per process, one owner.** The same code holds the metadata and
  the payload backing. A process mixing Ada, C and Rust (an Ada app linking
  the Rust fonts library, Penny with its C dependencies) has one heap, never
  two adapters mapping the same offsets to different memory.
- **No limits but policy.** The heap grows until the process's frame quota
  (or the owned-memory aperture) says no. There is no fixed arena size and no
  per-allocation cap of its own.

### Stages

**A. A growable process heap** (Ada, behind an address-level API):
- **Small requests** (up to 4 KiB): any number of 16 MiB arenas, each with its
  own proved `Heap_Slabs` metadata.
  - Each arena is one reservation (`SYSCALL_RESERVE_OWNED_MEMORY`), created
    on demand: its metadata first, then the payload.
  - Backing is committed with `SYSCALL_COMMIT_OWNED_MEMORY_PREFIX`, in 1 MiB
    steps, as blocks reach further into the arena.
- **Medium requests** (up to 1 MiB, alignment up to 4 KiB): any number of
  16 MiB page-run arenas, each with its own proved `Heap_Extents` metadata,
  laid out the same way.
- **Huge requests:** each in its own reservation, committed in full. Freeing
  it releases the reservation, so the memory goes back to the kernel.
- **Routing:** a directory of arena records (huge blocks included), itself
  reserved and committed as it grows, with an index sorted by address. `free`,
  `realloc` and `usable_size` find a block's arena by binary search.
- **Metadata:** reserved and committed as arenas appear, never fixed.
- **Proofs and tests:** the routing directory is to be SPARK, aiming at
  level 2; today it is regression-tested (`tests/cualloc`), not proved.
  System calls sit behind one thin boundary (the generic's `Reserve`,
  `Commit` and `Release`) that hosted tests replace with Linux memory.
- **Locking:** one lock serializes the heap at first (stage D adds
  per-thread caches).

**B. Adapters, all over that one heap:**
- `System.Memory` in the user runtime.
- The libc `malloc`, `free`, `calloc`, `realloc`, `reallocarray`,
  `aligned_alloc`, `posix_memalign`, `memalign` and `malloc_usable_size`, as
  Ada exports (musl's legacy `valloc` calls `memalign`; musl has no
  `pvalloc`). musl's malloc is dropped through
  `replaced-by-ada.txt`.
- A thin Rust `GlobalAlloc`, which no longer manages its own backing.
- CuAlloc is linked once per program.

**C. Kernel policy instead of arbitrary bounds:**
- One reservation is capped at 2 GiB and one commit at 16 MiB. The heap
  works around the commit cap by committing repeatedly.
- A single allocation above 2 GiB needs the reservation cap to become a
  quota policy. That is a kernel change, made with tests and a proof.

**D. Tuning against jemalloc and mimalloc:**
- per-thread caches, with remote frees batched to their owner;
- finer size classes;
- returning backing to the kernel;
- measured on the existing traces, multithreaded traces, and traces from
  real programs (Penny, `cc1`, the CCL console, fonts), reporting tail
  latency, retained memory and waste as well as throughput.

## Next milestones, in order

1. **Refine the proved dynamic slabs.** Empty-only reassignment is implemented.
   Introduce finer, explicit size classes to bound rounding waste; measure
   partially occupied slabs and adversarial search costs. Consider a proved
   availability index if measurements justify it, not an unchecked cache.
2. **Backing extent lifecycle.** Aligned provider interface with deterministic
   failure injection; acquire, commit, decommit and release. Prove rollback and
   no overlapping/live extent reuse in the metadata core. Test real OS boundaries.
3. **Complete runtime semantics.** Large and over-aligned allocations, checked
   zeroed allocation and realloc; Rust host adapter tests before native wiring.
   Validate GNAT's exact allocator ABI and exception/OOM behavior separately.
4. **Concurrency one protocol at a time.** Begin with explicit owner heaps,
   bounded remote-free batches and safe orphan adoption. No lazy reclamation that
   can recycle a page still reachable by a producer. State memory-ordering and
   lifetime assumptions separately from sequential SPARK contracts.
5. **Native opt-in trials.** Rust probe, then one Ada userspace workload; verify
   quota/OOM behavior, startup/teardown, IPC buffer lifetimes and latency. Do not
   replace all runtime allocators in one change or conflate this with the kernel.
6. **Production evidence.** Long-running fragmentation, adversarial lifetimes,
   multithreaded/NUMA tests, real fonts/parser/application traces, tooling support
   and cross-platform adapters. Evaluate guard pages/quarantine/zeroing as explicit
   security modes with documented performance and memory costs.

The earlier `rlsf` Cargo dependency experiment was not integrated and has been
removed. The native Rust probe now uses the bounded SPARK-core adapter; see the
[implementation boundaries](../userspace/allocator/README.md#native-rust-trial).
This advances milestones 3/5 without claiming milestone 2's OS backing lifecycle:
Incrementally acquired 1 MiB small-object backing chunks, a separate sbrk-backed
large arena, aligned large runs, zeroing and failure-preserving realloc are
implemented. Incremental large-object backing commitment,
additional arenas/reclamation, GNAT ABI integration and scalable
concurrency remain open. The large-run core is proved; the Ada/Rust adapter,
pointer copying and synchronization are regression-tested, not proved.
