# Portable SPARK userspace allocator — bounded pilot

This is a bounded allocator with a **native Rust opt-in trial** as well as the
Linux-hosted benchmark. The Rust probe uses it through `GlobalAlloc`; GNAT
`System.Memory` and other applications are unchanged. No C allocator
implementation is linked into CuBit by this project.

The purpose is to establish a small, executable, functionally proved metadata
core and an honest performance baseline before introducing production heap
growth and concurrency. Empty-slab reuse is now implemented. See the
[design and roadmap](../../docs/userspace-allocator.md) and
[test/benchmark instructions](../../tests/userspace-allocator/README.md).

## Boundaries

| Component | Responsibility | Evidence |
|---|---|---|
| `Heap_Classes` | Size selection and constant-divisor geometry | SPARK contracts + exhaustive boundary tests |
| `Heap_Bitmap` | Bounded free-index cache, packed membership and exact occupancy | SPARK contracts + independent model tests |
| `Heap_Slabs` | Dynamic size-class assignment; recycle only empty slabs; preserve live blocks | SPARK contracts + mixed-lifetime tests |
| `Heap_Extents` | Aligned contiguous page runs for large requests; exact ownership and failure preservation | SPARK contracts + checked extent tests |
| `host/Heap_Bridge` | Static arena, machine addresses, exported hosted test ABI | Tested, **not SPARK-proved** |
| `runtime/Heap_Runtime`, Rust `cubit-allocator` | Offset routing, GlobalAlloc, zeroing/realloc, singleton serialization | Hosted/native tests, **not SPARK-proved** |
| Rust/Python harness | Identical allocation traces and reference comparisons | Regression checks, not formal proof |

The core uses integers, arrays and records, not raw/access pointers, system
calls or architecture intrinsics. It is intended to carry across ordinary
32-/64-bit GNAT targets with 8-bit storage units. Only Linux x86-64 has been
executed on the host; the native trial also runs on CuBit x86-64. The tagged allocation result has an explicit eight-byte layout to
avoid compiler-generated padded temporary copies; its variant semantics remain
unchanged and are included in the proof.

The hosted benchmark accepts 1–4096 bytes, rounds to one of nine power-of-two classes,
provides 16-byte alignment, and shares 256 slabs of 64 KiB between classes.
Any class can use all backing capacity. A slab changes class only when empty;
free space trapped in partially occupied slabs cannot serve another class.
The adapter reserves 16,777,216 payload bytes and 262,440 metadata bytes on the
tested host. This is a fixed backing arena, **not OS heap growth/reclamation**.
Reserved virtual bytes are not RSS. Each slab has a 64-entry, 16-bit free-index
cache and a full membership bitmap, in a deliberately padded 1 KiB object.
Metadata is about 1.56% of backing, down from 13.3% with complete index stacks.

Allocation checks validated per-class slab hints first. On a miss it tries up
to eight following slabs, then at most two complete passes: matching non-full
slabs before arbitrary empty slabs. The lookahead is a heuristic, not an
authoritative availability index; no new metadata or per-object index maintenance
is needed, and the full fallback preserves discovery of all usable capacity.
Within a selected slab, allocation pops a free
index from its cache; an empty cache refills with up to 64 free bitmap positions.
Release validates membership and caches the returned index. If full, it replaces
the most recently cached index; the displaced slot remains free in the bitmap.
Release is constant-work, but allocation can scan up to 4096 positions on refill:
this is not a constant-time or real-time allocation guarantee. Cold fills and
burst workloads need separate performance assessment. No payload free-list links.

The hot path uses a 256-byte request-class table, validated slab hints,
and a separately compiled slab-scan fallback. Slab-local division uses proved exact
fixed-point scaling; alignment uses a proved mask. These are implementation
choices behind the original allocation/lifetime contracts, not weaker safety
requirements. See the dated results for workload-specific performance; passing
small-object churn targets does not establish overall allocator parity.

Each heap has one owner. Concurrent calls, remote free, reinitializing a heap
with live references, and using an allocation after release are outside its
contract. The adapter must be initialized before use. Do not install it as a
process-wide `malloc` replacement.

## Native Rust trial

`userspace/rust/allocator` has two independent 16 MiB logical regions:
one for slabs and one for the page-run allocator. Slab payload is supplied in
separate 1 MiB chunks on demand; the large region is acquired on first use. Native CuBit
backing comes from failure-atomic `sbrk`; Linux tests use `System` only to supply
arena backing, not to serve individual allocations. Requests up
to 4096 bytes/alignment use slabs; larger/over-aligned requests use page runs.
Maximum individual size is 16 MiB; maximum alignment is 1 MiB. Exhausting one
region does not borrow from the other. The large path uses bounded first-fit
search, not the tuned small-object fast path; it is not performance-benchmarked.

The Rust boundary serializes the single-owner metadata with an acquire/release
spinlock, including one-time initialization. This does not provide native CuBit
threads, scalable concurrent heaps, reentrancy, or signal/interrupt-handler use.
Failed allocation returns null; failed realloc retains the original allocation.
`alloc_zeroed` clears requested payload bytes. Rust fallible containers can
report exhaustion; infallible allocation remains subject to Rust's abort policy.

No payload arena is stored in BSS. A small-object backing acquisition requests
1 MiB plus 4095 bytes of alignment slack, rather than eagerly committing 16 MiB.
Every slab lies within one chunk, so the proved logical offsets are unchanged.
The Rust adapter maps offsets to chunk pointers and finds the owning chunk on
release (a bounded scan of at most 16 entries). Hosted runtime tests exercise
the same segmented layout, including all 4096 page-sized objects and reuse.
The large region still commits 16 MiB plus up to 1 MiB of alignment slack;
incremental large backing, more arenas and returning backing remain future work.
A failed provider request returns the tentative metadata allocation before
returning null. Regions remain available for reuse until process exit. The prior hosted benchmark
timings do not measure this new Rust boundary/lock or the large-allocation path.

```sh
nix develop -c bash tests/userspace-allocator/runtime.sh
nix develop -c bash tests/rust-native/run.sh
```

## What the proof does not say

An immediate duplicate release or interior offset cannot damage the metadata.
It does **not** follow that raw-pointer clients become memory safe: a stale
pointer to an address that has been reallocated can release the new allocation.
The allocator cannot distinguish those lifetimes without a different API or
additional mechanism. It also cannot stop an unsafe client overwriting live
payloads or guessing another live block's exact address.

The proof assumes initialized valid metadata and exclusive ownership; it does
not cover FFI, address conversion, compiler correctness, physical page supply,
OS mapping, memory reclamation, synchronization or payload accesses. Out-of-band
metadata reduces accidental coupling to payload corruption; it is not a memory
protection boundary within one process.

All proof predicates/lemmas are Ghost. Release builds suppress runtime checks;
hosted Ada tests retain arithmetic/bounds checks and use explicit test checks.
Recursive Ghost lemmas are proved rather than executed in tests. The core has no
`pragma Assume` or `SPARK_Mode => Off`. Explicit validity preconditions describe
the inductive invariant, rather than inserting linear validation scans into
the allocation path.
