# Physical allocator verification

## Objective and scope

The kernel must execute the core implementation we verify. A separately proved
model is useful only when its refinement to the implementation is also justified.
Absence of runtime errors is necessary but does not establish non-overlap,
ownership, reclamation safety or concurrency correctness.

Work proceeds one proved property at a time, with host tests of the same code
and native regressions before moving to the next representation change. Do not
add assumptions or disable proof to make an obligation disappear. Kernel builds
keep runtime assertions disabled; host harnesses enable them.

## First boundary: XOR bitmap geometry

The existing allocator's XOR bit distinguishes buddy-pair states during split
and coalesce. Its physical maximum is inclusive, but its layout used
`Last_Frame / Pair_Span` bits per order instead of
`Last_Frame / Pair_Span + 1`. The last pair's bit could therefore alias the next
order's slice. This is a metadata layout defect, not an attribution of previous
desktop freezes; currently excluded boundary blocks may mask particular cases.

`Buddy_Bitmap` uses a private array of cumulative boundaries. Adjacent slices
share an endpoint, so there are no separately mutable length/offset fields.
The maximum frame number is bounded by x86-64's 52-bit physical address format
with 4 KiB frames. Orders are bounded by that frame-number width, not by the
current kernel configuration's smaller maximum allocation order.

Required first properties:

- Every pair containing a tracked frame has an entry, including partial final pairs.
- Every lookup remains in the selected order's half-open slice.
- Different orders' slices do not overlap.
- Word allocation covers every bit; index/offset arithmetic does not overflow.

Physical word overlays and locking remain outside this core. The adapter must
provide the initialized layout, valid tracked frame numbers, the corresponding
reserved metadata storage and serialized mutation. Proving indexing under those
conditions does not prove the entire adapter supplies them correctly.

First milestone implemented: the kernel's bitmap sizing and alloc/free bit
lookups now use this core. All 87 focused SPARK obligations pass, including
ghost proofs of cross-order separation and word coverage. Host tests include
the historical collision and exhaustive small layouts. See
[commands and exact evidence](../tests/buddy-bitmap/README.md).

Shift refinement completed: the production lookup again uses a right shift,
with no integer division or helper calls in the generated kernel code. The
proof keeps pair indices as bounded machine words, then uses ghost ordering
lemmas for the conversion to integer offsets. An additional CVC5 integer encoding
discharges the mixed-theory bridge that stalled under the default encoding.
The proof script and codegen regression make this reproducible. All lemmas,
layout observations used only by proofs, and test-only boundary state are Ghost;
none of those helpers appear in the kernel object. There is still only one
production implementation. Actual allocator throughput/latency benchmarks
remain separate work; instruction inspection is not an end-to-end measurement.

Native four-vCPU KVM tests passed for the initial bitmap-layout implementation:
`capability-security` verified stack/heap operation and eight partial-load
rollbacks with PID reuse; the multi-app `desktop-doom` fixture verified
Workbench/NetSurf launches and closure, DOOM game pixels and responsive Apps
menu input. The kernel passed its 2048-byte per-function stack limit. These
are regressions, not proof of end-to-end allocator correctness or latency bounds.

The final shift/Ghost refinement also passed the four-vCPU native
`capability-security` regression, including all eight rollbacks and PID reuse,
and the kernel's stack/codegen checks. The documented proof script reproduces
all 87 obligations with zero unproved or justified checks.

## Second boundary: authoritative block-head state

`Buddy_Blocks` is the production SPARK transition core, with a private two-byte
descriptor per physical frame, reserved before boot memory enters the allocator.
States distinguish reserved memory, block interiors, detached heads, listed heads,
live allocations and retiring allocations. Orders are recorded at heads, not
in caller-writable payloads. For an 8 GiB physical address span this adds 4 MiB
of metadata; holes in that span also require entries, as with the existing bitmap
and per-frame pin/owner arrays.

All **18 focused analysis obligations pass**: four functional contracts, five
runtime checks, four initialization checks and five termination checks; zero
unproved or justified obligations. The contracts prove successful transitions
and exact unchanged-state behavior on rejection. There are no assumptions,
suppressed obligations, or SPARK-Off sections in this core. The kernel still
disables runtime assertions. No additional proof-only executable helpers were
needed for this step.

The physical adapter now:

- Separates boot admission from runtime free, checks the whole admitted span is
  still reserved, and initializes interiors before publishing a block.
- Validates state and exact order before removing a head or releasing an allocation.
  Wrong-order, interior, reserved and duplicate frees cannot follow the normal
  release path. Invalid internal allocator requests fail-stop; this is not a
  recoverable userspace free API.
- Splits detached heads into two detached children and publishes only the unused
  child. Allocation commits the retained head before unlocking and zeroing it.
- Validates the XOR-selected buddy's listed state before reading its payload
  links, removes it, and merges two detached heads, retiring the upper head to
  interior state.
- Routes either order-zero free entry point through pin-aware retirement. Only
  final reclamation returns a retiring head to the free lists. Larger-block free
  checks every constituent pin/owner before releasing the allocation; callers
  must finish DMA teardown first. Claiming a frame requires a live allocation
  containing it, including constituent pages of larger DMA blocks.

Host tests exercise every state/order/operation combination and over four million
split/merge combinations with enabled postconditions. The four-vCPU KVM security
regression passed, including eight partial-load rollbacks and subsequent PID
reuse. The multi-app four-vCPU desktop/DOOM regression also passed, including
Workbench/NetSurf launch and closure, DOOM game pixels and responsive Apps-menu
input. The final kernel passes its per-function stack limit and bitmap codegen
check; the block-state object has no undefined runtime dependencies. See
[commands](../tests/buddy-blocks/README.md).

**This does not prove the whole allocator.** In particular, the descriptor
transitions do not prove that the physical adapter supplies two actual adjacent
buddies, that intrusive next/previous links agree with descriptor membership,
that firmware/boot reservations are correct, or that the lock and TLB lifetime
boundaries are sound. The descriptor is authoritative for admitting a removal,
not a formal proof of the entire linked-list representation. These are explicit
next obligations, not implicit consequences of the transition proof.

## Third boundary: physical split/coalesce geometry

`Buddy_Geometry` represents aligned spans in physical-frame units. The kernel
supplies power-of-two lengths derived from its orders. Keeping explicit spans
in this core avoids coupling elementary range/alignment proofs to exponent
arithmetic, and does not introduce a second runtime allocator representation.
These short-lived values describe the operation's geometry; `Buddy_Blocks`
remains the authoritative lifetime ledger.

All **46 focused analysis obligations pass**, including:

- Splitting a valid even span produces equal, aligned, non-overlapping children
  whose union exactly covers the original span.
- Merging requires equal-sized adjacent spans aligned to their combined length.
  Adjacent frames 1 and 2, for example, are not order-zero buddies.
- The resulting parent exactly covers both children. A Ghost composition proof
  establishes that split followed by merge restores the original descriptor of
  the span (both start and length).

The actual allocator now derives split-child addresses from this core. During
coalescing, XOR only proposes a buddy candidate: both ranges are admitted and
their geometry is checked before removing the candidate from its list. The
merged address comes from the core, replacing the unchecked parent-address mask.
Metadata range/alignment admission uses the core's `Fits` predicate too.

Host regressions include 390,000 deterministic full-width split cases, inclusive
boundaries, invalid pairs and non-dyadic even spans. The core intentionally
proves aligned-span arithmetic, not the adapter's conversion between allocation
orders, addresses and lengths. The physical overlays and list membership
correspondence still need proof. This milestone therefore establishes **local
partition preservation**, not global allocation non-overlap.

The four-vCPU KVM `capability-security` regression passed, including eight
partial-load rollbacks and subsequent PID reuse. Kernel stack-limit and codegen
checks passed: the geometry object's Ghost helpers and assertion runtime are
absent, and its split routine contains no division or helper calls. This is
instruction inspection, not an end-to-end allocator latency measurement.
The final multi-app `desktop-doom` regression also passed Workbench/NetSurf
launch and closure, DOOM game pixels and responsive Apps-menu input.

See [reproducible proof, host and native commands](../tests/buddy-geometry/README.md).

## Fourth boundary: constant-time intrusive splices

The kernel retains its existing linked-list representation. An experimental
packed free-set tree was proved and benchmarked separately, but was not
integrated: its hinted lookup improved the prototype, not the existing kernel
allocator. See [the experiment's status](../tests/free-block-set/README.md).

The production list writes now use `Intrusive_List_Splices`, a generic over the
reference type. It proves exact insertion/removal updates, count arithmetic and
Ghost composition checks, including singleton sentinel aliasing. Both integer
and `System.Address` instantiations pass: **44 analysis checks, no unproved or
justified obligations**. The physical adapter uses the known sentinel address
for insertion and front removal instead of reloading a redundant predecessor
value from the payload. Interior unlink still depends on valid neighbor links.

The host oracle checks the entire list after 200,000 operations. Kernel codegen
confirms full helper inlining, absent Ghost code, unchanged insertion instruction
count (32), the same existing metadata-check call and an in-place count
increment. This is no claim of an allocator performance improvement. These
proofs establish the splice primitives, not yet global physical-list membership
or the correctness of every caller's field mapping. Final four-vCPU KVM
regressions passed partial-load rollback/PID reuse and multi-app desktop/DOOM
rendering with responsive Apps-menu input.

See [proof, regression and codegen commands](../tests/intrusive-list-splices/README.md).

## Fifth boundary: Ghost list/ledger correspondence

`tests/intrusive-list-splices/buddy_list_refinement` now describes an entire
single-order list with an ordered sequence and inverse ranks. Its invariant
connects sentinel and node links, unique sequence membership, exact count, and
the production ledger's `Listed` state at that order. Other-order descriptors
are excluded, as are allocated/retiring/reserved/interior blocks.

The empty-list base case initializes only sentinel links and leaves unlisted
payload links arbitrary. Insertion and arbitrary-position removal call the
actual production ledger/splice primitives and preserve this invariant. Ghost
lemmas establish exact membership, uniqueness and traversal back to the sentinel
after exactly the recorded number of nodes. The combined proof target passes
**213 checks, none unproved or justified**, including the earlier local splice
and block-state checks. This is not a new allocator representation and does not
add any runtime code, metadata, validation scans or guards.

Host tests exercise these Ghost routines across all supported orders and every
removal position for several capacities, with independent link traversal and
negative tests for corrupted links, counts, witnesses and ledger correspondence.
The 200,000-operation production-splice oracle also now checks the actual block
ledger through publish/remove/commit/release transitions.

This is a proof of indexed logical composition, **not yet its physical-memory
refinement**. Physical address-to-ID injection, disjoint fields, valid boot
initialization, all-order/XOR composition and serialized execution are still
adapter obligations. No new native behavior was introduced by this round.

## Sixth boundary: boot-admission corrections

Reviewing the physical starting state exposed two production off-by-one bugs:

- Buddy setup admitted a whole block when its first frame equaled the boot
  allocator's inclusive high-water mark. That frame can still be boot-owned.
  The shared `Buddy_Boot_Admission.Source_Of` rule includes equality in the
  bitmap-checked prefix. Blocks crossing the prefix check only covered frames,
  never querying the boot bitmap for the unallocated tail beyond its range.
- `BootAllocator.MAX_BOOT_PFN` was the number of represented bits, not the
  inclusive last bit. Setup/search could access one word beyond the bitmap
  with checks disabled. It now comes from the proved `Last_Frame` static Ada
  expression function. The 64 MiB boot window has last PFN 16,383, not 16,384.
  Search contracts were corrected to match inclusive bounds.

Buddy setup also explicitly zeros each sentinel's count, completing the
concrete empty-sentinel writes instead of relying on `.bss` contents.

The focused core proves **13 checks, none unproved or justified**. Host tests
cover 133,120 interval/high-water cases, 2,105,344 bitmap indices and full-width
edges. Codegen retains three-instruction shift/mask getters with no helper or
Ghost calls, and checks the compiled limit against the bitmap symbol's size.
Final four-vCPU KVM security and multi-app desktop/DOOM regressions passed,
including partial-load rollback, PID reuse, game pixels and responsive input.
This does not prove boot bitmap free-count consistency, firmware region
validity/alignment, complete physical metadata mapping, or SMP integration.
See [boot-admission evidence](../tests/buddy-boot-admission/README.md).

## Seventh boundary: metadata slots and numeric addresses

The kernel now uses `Buddy_Metadata` for descriptor-address calculation and the
pin/owner/descriptor table byte/page counts. All tables take their inclusive
extent from the already-admitted bitmap layout. Descriptor representation and
array stride share a named bit size with a compile-time consistency check.

The pure core passes **20 checks, none unproved or justified**: exact inclusive
footprints, slot/block-span bounds, disjoint slots, sufficient/minimal page
rounding, and nonwrapping/disjoint addresses under a valid table-span premise.
The premise and lemmas are Ghost. Boot request sizes are explicitly admitted
before narrowing to `AllocSize`; no per-lookup validation scan was introduced.

Host tests pass 1,574,454 slot cases including full-width and address-wrap edges,
page/request boundaries, supported orders and real Ada array stride. The earlier
213-check list/ledger target still passes. Production descriptor lookup remains
433 bytes with the existing indexed `LEA`, no extra helper calls and no Ghost
code. The existing geometry alignment division remains a separate optimization
candidate; this round is not an allocator speedup claim.
Final four-vCPU KVM security and multi-app desktop/DOOM regressions passed,
including partial-load rollback, PID reuse, game rendering and responsive input.

This establishes byte-layout and numeric-address separation, **not physical
reservation ownership or complete address-overlay refinement**. Boot allocation
must still establish distinct mapped reservations, exclude metadata/sentinels
from payload, and supply the premises needed to apply the list model. See
[metadata proof and regression evidence](../tests/buddy-metadata/README.md).

## Eighth boundary: actual boot-frame reservations

The boot allocator now delegates its actual bitmap/search/mutation/high-water
state to the private `Boot_Frame_Allocator` ADT. Packed Boolean components retain
one-bit-per-frame storage while giving SPARK direct indexed state transitions.
The separate free counter is gone: repeated admission of an already-free frame
used to increment it again. Counts are diagnostic scans of the authoritative map.
Unused boot release and old private bit/search routines were removed; admission
is confined to setup, before successful reservation. Frame zero stays excluded.

The configured core plus Ghost composition/handoff proofs pass **27 checks,
none unproved or justified**. Successful requests claim exactly their previously
free span, preserve all other bits, and update the inclusive high-water mark;
failure preserves the entire state. Two successful reservations do not overlap.
Reserved frames select the bitmap-checked handoff prefix and are unavailable
there. This closes a concrete part of the reservation-to-handoff argument,
without claiming that the physical mappings or whole buddy arena are proved.

Hosted tests pass 206,022 requests against an independent interval-search oracle,
including exhaustive small maps/request pairs and production-size fragmentation.
Search completeness/first-fit and exact diagnostic counts are tested, not yet
separate functional theorems. Pinned kernel codegen retains a 2,048-byte bitmap;
the reservation routine is a leaf with no calls, divisions, copies or proof
bookkeeping. Boot setup no longer iterates outside its own 64 MiB arena. Neither
change is presented as a measured steady-state buddy allocator speedup.
The optimized kernel passed the four-vCPU KVM security rollback/PID-reuse and
multi-app desktop/DOOM fixtures. Existing buddy insertion and descriptor lookup
codegen checks also remain unchanged.

Firmware region validity/alignment/conflicting overlaps, physical mappings,
setup-phase discipline and complete metadata/sentinel exclusion remain trusted
integration work. See [boot reservation evidence](../tests/boot-frame-allocator/README.md).

## Ninth boundary: firmware pages, conflicts and duplicate owners

Both allocators now consume the same `Firmware_Frames` policy via
`MemoryAreas.Allocation_Map`. Inclusive usable byte intervals round inward to
whole pages. Reserved/ACPI/bad/framebuffer/I/O intervals round outward to every
touched page and take precedence independent of map order. Earliest usable
whole-page coverage owns duplicates, preventing double admission.

The pure core passes **51 checks, none unproved or justified**, including safe
rounding, aligned/bounded block selection, no conflicts for admitted blocks,
whole-span conflict witnesses for rejection, no singleton splitting, and unique
per-page owner admission. An explicit Boolean scan state removes an impossible
third state instead of introducing defensive guards. Uniqueness is Ghost.

Buddy setup tiles the available spans and splits at conflicts or the boot
high-water prefix; it no longer discards already-aligned boundaries/trailing
blocks. Byte-oracle tests pass 57,346 ranges, 2,008 maps and 401,354 candidates,
including duplicate-free complete traversal of the admitted small-map pages.
Partial usable fragments are deliberately not stitched into a whole page.
The final optimized kernel passed the four-vCPU KVM security rollback/PID-reuse
and multi-app desktop/DOOM fixtures. The 27-check reservation/handoff proof and
existing boot bitmap, buddy insertion and descriptor codegen checks still pass.

The adapter now initializes absent entries explicitly and rejects malformed
numeric ranges before subtraction/narrowing. Framebuffer extent arithmetic is
widened and bounded. These adapter/parser changes are not part of the focused
51-check proof. Raw firmware buffer traversal/counts/variable entry sizes, boot
module reservations and complete physical mapping/cache-mode consistency remain
open. See [firmware admission evidence](../tests/firmware-frames/README.md).

## Tenth boundary: bounded Multiboot-v1 decoding

`Multiboot_Memory_Map` replaces the mismatch between fixed-size record counting
and variable-size traversal. The pure core validates the size prefix before
reading a payload, skips extended records, bounds physical endpoints before
addition and publishes zero entries on any parse failure. All **72 checks**
are discharged, none unproved or justified. Hosted tests pass **75,085 cases**,
including truncation, malformed lengths, overflow, arbitrary array bounds and
capacity limits. Exact decoded values/tag meanings are regression-tested;
the focused functional contracts cover progression, valid intervals and safe
publication, not a complete wire-format equivalence theorem.

The kernel admits the raw map extent against its bootstrap mapping before
constructing an overlay. The decoded snapshot and raw/normalized maps live in
static boot workspace; firmware lengths no longer size the entry stack.
Normalization writes caller-owned output once for both allocators, avoiding an
unconstrained return on the kernel secondary stack. The configured 1,024-record
budget fails explicitly on excess entries; it is not a protocol limit.

The optimized kernel passed four-vCPU KVM security rollback/PID-reuse and
multi-app desktop/DOOM regressions. The adapter, parser and normalizer report
368, 144 and 224 bytes of per-function stack respectively, within the existing
2 KiB check. Proof-erasure and existing allocator hot-path codegen checks pass;
these are not a total call-chain stack proof or a throughput measurement.

Module descriptor sizing now matches the 16-byte wire format, with descriptor
and bounded-name extents checked before use. The raw adapter remains outside
the focused proof. Initial assembly/header-pointer admission (addressed in the
next boundary), real physical backing, firmware/source stability, full module lifetimes and overlapping
payloads, cache/mapping consistency and DMA remain open. Being within a mapped
numeric window is not proof of readable RAM. This is not yet a fully proved
boot path or allocator. See [decoder evidence and commands](../tests/multiboot-memory-map/README.md).

## Eleventh boundary: initial boot information

The BSP assembly entry now checks magic and the complete header's numeric
extent before its first loader-memory read. Ada takes a scalar address and
validates it before creating a byte overlay, rather than copying a by-reference
record in `kmain` declarations. The pure `Multiboot_Entry` core discharges all
**11 checks**, none unproved or justified: successful bounded-address admission,
failure-zeroed snapshots, selected sanitized-header invariants and GRUB RGB
union normalization. The raw GRUB extent is now 118 bytes, normalized into the
116-byte internal header; this corrects the manual/implementation mask offset
discrepancy exposed by strict framebuffer validation.

Hosted tests pass **169,416 cases**, including byte-by-byte field checks and
execution of the same assembly gate macro against an independent numeric oracle.
Required memory-map/framebuffer flags are checked before consuming their data;
unadvertised modules, unused fields and text-mode RGB metadata stay zero. The
adapter copies wire bytes into a sufficiently sized native record rather than
overlaying the padded record on a shorter byte buffer. The 19-bit reserved flag
field now has a matching type range. Boot assembly include dependencies are
tracked explicitly.

The entry implementation passed the four-vCPU security rollback/PID-reuse
fixture. The final kernel passed the Q35 multi-app desktop/DOOM regression and
an isolated text-mode/no-module early-boot fixture on the older QEMU chipset.
The latter exposed a legacy-FADT overread, now fixed by length-aware selection
of the legacy or extended DSDT pointer. That ACPI adapter fix is regression-tested,
not included in the entry proof. Entry admission/snapshot helpers each
report 8 bytes of stack; the raw adapter remains below 2 KiB. Focused release
proof-erasure and existing allocator codegen checks pass.

The full assembly path, raw-address/record adapter and real physical backing
are not proved by these entry checks. Video geometry validation and
module payload/metadata lifetime, overlap and later-consumption validation
remain separate work. See [entry evidence and commands](../tests/multiboot-entry/README.md).

## Twelfth boundary: boot module snapshot and retained lifetime

`Boot_Modules` owns a private, immutable-after-sealing catalog. Its **62 checks**
all discharge, with none unproved or justified: valid page-aligned spans,
pairwise page disjointness, failed-append state preservation, sealed-state
immutability, reservation-end coverage and payload-preserving padding clearing.
The consistency invariant is Ghost and absent from optimized kernel code.

The Multiboot adapter now snapshots descriptors and bounded terminated names
before allocator setup. It checks source windows/kernel exclusion before
overlays, validates firmware RAM coverage, rejects payload/kernel/metadata/video
overlaps and publishes only the completed snapshot. `Modules.setup` no longer
reads loader-owned names/descriptors; initrd selection is exact and duplicates
are rejected. A mapping failure cannot proceed to device-manager grant/resume.

The current lifetime policy is permanent reservation, not reference-counted
reclamation. Payload pages remain inside the reserved boot prefix for CPIO
indexes, process mappings and resident-initrd range recognition. Metadata is
kernel-owned, and final-page padding is zeroed before userspace exposure. Limits
of 64 modules and 64 name bytes fail explicitly rather than truncating input.
No LiveCD/ISO or CPIO protocol change is required.

Hosted tests pass **1,000,460 cases**, including an independent byte-ledger RAM
coverage oracle. The RAM walk's safety/progress are proved; its complete coverage
semantics are tested, not a separate theorem. Native security and desktop/DOOM
regressions pass, and isolated real-GRUB fixtures reject duplicate, overlong and
over-capacity declarations before allocator admission.
The rebuilt USB-only LiveCD also passes its four-vCPU optical boot/app-launch
fixture, retaining the split between 7 bootstrap files and 18 CD payload files.
Final codegen keeps capture/catalog storage in BSS, the raw adapter at 640 bytes
of per-function stack, and Ghost bookkeeping absent. Existing allocator
codegen regressions remain unchanged.

The raw adapter, effective hardware mappings, actual RAM/source stability,
DMA, container parsing and full physical allocator refinement retain their
separate trust boundaries. This closes the snapshot/lifetime gap for a
permanently resident initrd, not a proof of all those layers. See
[module evidence and commands](../tests/boot-modules/README.md).

## Thirteenth boundary: boot framebuffer admission

`Boot_Framebuffer` separates wire decoding from signed geometry admission. All
44 checks discharge, including functional descriptor consistency, page rounding,
pitch/extent preservation, byte-budget and physical-window limits, and pixel
offset bounds. The Ghost validity function is erased; release Decode uses 80
bytes of stack and Pixel_Offset uses 8, with no external/runtime dependencies.

The boot adapter caps the mapped span by CPU physical width and the direct-map
window, rejects page overlap with retained kernel/loader/module storage and
ACPI/NVS/bad memory, then publishes one descriptor. RAM-backed framebuffers are
reserved as VIDEO, and ordinary boot mapping excludes those pages before device
mapping. Consumers no longer reinterpret raw geometry; MAPFB preserves the
within-page offset. Pixel addressing now honors pitch, glyph cells no longer
overrun their right edge, and console allocation failure is handled before
renderer installation. Unsupported pixel layouts fail explicitly.

Hosted tests cover 17,427 descriptions plus 3,981,312 pixel addresses. Native
fixtures cover malformed/overlapping descriptions and RAM-backed/pitched/unaligned
mapping. These are supplements to, not proofs of, physical backing, CPUID, MMIO,
cache attributes, the complete text renderer, MAPFB lifetime/rollback or page-table
mutation. No GPU or multi-output implementation is implied. See
[framebuffer evidence](../tests/boot-framebuffer/README.md) and
[output/rendering architecture](display-outputs-and-scaling.md).

## Portability of the proof work

The splice generic needs only reference equality/assignment: no x86 address
arithmetic, instruction, endian or page-size assumptions. Its algorithms and
functional statements are architecture-independent. That does not mean a new
architecture's compiled kernel has already been verified.

The other pure cores are reusable algorithms with some concrete configuration
choices. For example, geometry uses frame units but currently bounds frame
numbers to 40 bits; the bitmap uses 64-bit words and bounds derived from x86's
52-bit physical address format with 4 KiB frames. Review/parameterize those
limits and rerun proofs when changing target geometry.

Physical/virtual mapping, page-table formats, boot memory discovery, interrupt
handling, atomic lock operations, memory ordering and TLB invalidation remain
target-specific boundaries. Porting requires implementing and validating those
boundaries and the Ada runtime/ABI. Hosted tests and current codegen checks are
not evidence of a completed ARM or RISC-V port.

## Subsequent properties, in order

1. Complete the physical block-state/free-list refinement: transitions, local
   splices and indexed single-order membership/count correspondence are now
   proved. Metadata slot bounds and numeric-address separation are now proved
   too. The actual boot reservation core now proves disjoint successive frame
   spans and coverage by the bitmap-checked handoff prefix. Numeric firmware
   page/conflict policy and bounded Multiboot byte decoding are now proved too.
   Initial header numeric admission/sanitization is now covered too. Complete
   Module snapshot/retention policy is now covered too. Complete physical backing,
   physical mapping and payload/metadata/sentinel exclusion, plus the
   address-to-ID/field correspondence needed to apply the invariant to the arena.
2. Lift the now-proved local split/coalesce geometry into arena-wide partition
   preservation: relate addresses/orders, list membership and XOR transitions
   to authoritative block state, so coalescing consumes exactly two free buddies.
3. Allocation conservation and failure atomicity: no two live allocations
   overlap; unsuccessful operations preserve state; free plus allocated plus
   reserved memory accounts for the managed arena. Include fragmented maps.
4. Ownership and lifetime: only valid ownership transitions succeed; pending
   release admits no new pins; only the final pin can trigger reclamation.
   Include multi-frame DMA allocations and alternate free entry points.
5. Integration: buddy lock/interrupt discipline, page-table publication and
   mutation, TLB shootdown completion, process retirement and stale handles.
6. Confidentiality: zeroing completes before a frame is exposed to a new
   protection domain, including failure/reuse paths.

Test targets include deterministic exhaustion, randomized split/free sequences
checked against an independent per-frame ledger, fragmentation, invalid frees,
and native SMP reclamation. Host callbacks do not prove physical aliases or
machine instructions; all such trusted boundaries must remain explicit.

## Other audit findings

Legacy contracts incorrectly required an order-0 block to exceed one frame,
required rounding down to strictly reduce an already aligned address, omitted
`'Old` in a free-list decrement, and excluded maximum-order alloc/free despite
the implementation supporting them. Correcting these specifications is not
equivalent to proving the free-list overlays that carry them.

The old alignment helpers discarded already aligned boundary blocks. The ninth
boundary replaces them with whole-page normalization and aligned tiling, guarded
by reserved precedence and unique usable ownership. Raw parser/mapping review
and complete arena refinement remain separate work.
