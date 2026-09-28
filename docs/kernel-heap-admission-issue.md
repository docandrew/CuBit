# Kernel heap admission failure found during wallpaper bring-up

## 2026-09-20: recoverable heap growth and real allocator backing

`sbrk` no longer kills a process when acquisition fails partway through growth.
The shared `Heap_Growth.Apply` sequence unmaps only the newly added prefix,
performs an all-online-CPU TLB shootdown, then releases its tracked frames in
reverse acquisition order. The old break and existing allocations remain
unchanged. Intermediate page tables and expanded tracking-node pools remain
owned for reuse/normal teardown; failure is not a promise of byte-for-byte
unchanged global allocator bookkeeping.

Tracking capacity grows on demand instead of acting as an implicit 16 MiB heap
limit. Whole-request address, representability and explicit nonzero quota
checks still run before acquisition. The initial allowance is now accurately
named `INITIAL_HEAP_FRAME_HEADROOM`. Resource quotas/admission policy remain
important: zero quota does not promise unlimited physical memory or prevent
resource-pressure denial of service.

The native pressure test also exposed and fixed `Virtmem.tableWalk` returning
a stale PFN from a non-present leaf after unmapping. Such an entry must report
no mapping, otherwise a subsequent allocation fails with a false collision.

Evidence: checked host failure injection at every partial prefix, SPARK
analysis of the generic control-flow/arithmetic instantiation, and native
Rust testing of three failed 256 MiB growth requests in a 128 MiB guest,
unchanged breaks/preserved data, and successful zero-filled growth afterward.
This is not a proof of the hardware callbacks, TLB implementation, or the whole
concurrent memory subsystem. One executing thread per address space and the
existing lifetime pin remain required; privileged remote mapping serialization
and eventual shared-address-space threads need their own audit.

The Rust adapter now obtains each 16 MiB arena on first use through `sbrk`
(with alignment slack), instead of keeping 32 MiB of payload in ELF/BSS.
Additional arenas, smaller commitment increments and returning memory to the
kernel are still future work. No new allocation syscall was needed here.

## 2026-09-20: ELF image capacity is independent of stack/heap

The old image admission rule rejected a 32 MiB Rust allocator BSS with a
1 MiB declared stack: the frame list only allowed stack pages plus 16 MiB.
Increasing the stack declaration to make an unrelated BSS fit is not a valid
solution. Process construction now sizes frame tracking from **actual image
pages + declared stack pages + heap headroom**, including each PT_LOAD's
zero-filled tail. The raw one-page bootstrap image declares its own count.

Pure SPARK helpers calculate page counts without rounding overflow, accumulate
image pages without mutating the count on failure, and reject unrepresentable
combined counts before process allocation. There is no separate fixed MiB
ceiling on BSS. Existing address/segment checks, fallible frame acquisition,
physical-memory constraints and unpublished-process cleanup remain in effect.
The current Natural-sized tracker represents up to roughly 8 TiB of 4 KiB
frames on this target; this is an implementation representability limit, not
promised available memory. Tracking-node resources also remain finite.

BSS is still eagerly backed and zeroed at launch. Sparse/demand-zero BSS and
admission against system-wide commitments remain separate work. Test coverage includes
32 MiB, 1 GiB and 64 GiB admission arithmetic (host-only for the latter two),
count boundaries/overflow, and a native Rust probe with 32 MiB of heap backing
in BSS and a 1 MiB declared stack (before switching that probe to sbrk backing).

## Original issue and follow-up history

2026-09-12: requesting a fourth 1920x1080 framebuffer in `desktop.svc`
panicked the kernel with `EXCEPTION: Exceeded list capacity`. The image was
not delivered as a passing build. The wallpaper implementation now uses the
existing compositor scene/drag/cursor layers without that extra allocation.
This avoids the trigger; it does **not** fix kernel allocation failure handling.

Evidence: `/tmp/cubit-usb-live.dg0xz4j8/serial.log` (ephemeral local test log).

`Process.create` sizes its physical-frame tracking list as the requested stack
reservation in pages plus `MAX_HEAP_FRAMES` (16 MiB). For a 16 MiB ELF stack,
that yields 8192 entries. Code/data/heap/committed stack pages all consume
entries. `Process.addPage` allocates a frame then inserts without capacity
admission; `LinkedLists.insertFront` raises when the list is full.

The original `Syscall.IPC.handleSbrk` checked a nonzero policy quota, but not tracking-list
capacity. Its quota-limited partial allocation also returns the original heap
end as if the entire request succeeded. Both are security/correctness issues:
a userspace request must not panic the kernel or silently under-allocate.

## 2026-09-13: whole-request heap admission implemented

`Heap_Admission` now returns either a typed, complete growth plan or a rejection
reason. `handleSbrk` validates the entire request before its allocation loop:
initialized/page-aligned heap origin, unsigned addition without wrap, the lower
of the reserved grant aperture and stack boundary, actual tracking capacity and
any nonzero policy quota. Rejection returns all-ones and leaves the break and
page ownership unchanged. Quota exhaustion no longer reports partial success.
An increment of zero is still a query; growth within an already committed page
does not consume another tracking entry.

The pure planner's admitted range, exact page accounting and capacity/quota
postcondition passed GNATprove (20 proved checks; no unproved checks, warnings,
Assume or SPARK-off code in the helper). Assertions remain disabled in the
kernel. Host tests exhaust all 4096 offsets with increments 0..8192 and cover
quota/capacity edges and invalid ranges. The four-vCPU native capability test
rejects wrapping and repeated 64 MiB requests, verifies an unchanged break,
then allocates, checks zeroing, writes memory and continues authority tests.

Current lifetime/execution rules allow one executing member per user address
space; `Process.create(thread => True)` explicitly rejects the dormant shared
address-space path. This preflight is NOT an SMP reservation protocol for future
shared address spaces. That feature needs coordinated owner accounting.

### Fallible runtime allocation and demand-stack checks

`Process.tryAddPage` now returns typed failure results. Heap growth and user
page faults use it rather than the raising `addPage` interface. The common
`Page_Allocation.Acquire` sequence releases an unpublished data frame and its
tracking node on failure. A failed page-table allocation can leave intermediate
tables linked to the address space; they remain owned for reuse and normal
teardown. It does not free published pages without TLB/pin coordination.

Slab resource exhaustion no longer raises while holding the slab lock.
`tryAllocate` returns null after unlocking; the legacy GNAT storage-pool
`Allocate` wrapper raises only after unlocking. `hasFree` is documented as a
hint, not a promise or reservation. `LinkedLists.tryInsertFront` checks capacity
before allocation. Circular-list insertion and removal now maintain both links
and clear head/tail when the last node is removed.

The pure `Page_Admission` helper uses half-open stack and heap reservations,
checks the lower canonical user limit, tracking capacity, and quota. Previously
`stackTop` and `heapEnd` were admitted inclusively. These are fault-admission
checks, not byte-level protection inside an already mapped partial heap page.
The helper does not promise that physical memory is available.

The earlier heap failure semantics (partial-growth termination is superseded
by the recoverable implementation above) were deliberately explicit:

- Whole-request admission rejection: return error, break unchanged, no allocation.
- Acquisition failure before the first new page is published: return error,
  break unchanged; any intermediate page tables remain owned by the address space.
- Failure after some pages are published: request process-local termination
  through the existing retirement path. Do not return a partial success or leave
  writable pages beyond the reported break in a continuing process.
- Rejected or unsatisfied user page fault: notify the supervisor and stop the
  affected process, rather than invoking the kernel last-chance handler.

That version was **not** fully transactional multi-page sbrk. A recoverable
all-or-nothing implementation needs reservation or coordinated rollback.

The [fault-injection suite](../tests/allocation-failures/README.md) exercises
the actual slab/list code and shared acquisition sequence, including every
acquisition failure stage, repeated failures and successful retries. The native
regression adds a 64 KiB demand-stack exercise before the heap/authority checks.
Formal claims are restricted to admission and the acquisition result invariant;
callback ownership and hardware behavior are not covered by those proofs.

Validation on 2026-09-13: the new proof suite completed 15 obligations
(3 functional contracts, 10 initialization checks, 2 termination checks),
with no unproved checks or warnings. Host fault-injection/model tests passed.
Four-vCPU KVM `capability-security` passed, as did `desktop-doom` with the
multi-app option (CCL Workbench, NetSurf, DOOM game pixels and responsive Apps
menu). The kernel build passed its per-function stack-usage limit; this is not
a worst-case call-chain proof. All builds/tests/proofs ran through Nix.

### Unpublished construction and ELF admission

Process creation now returns `NO_PROCESS` on PID exhaustion, requested-PID
collision, guarded-stack backing exhaustion, guard-table allocation failure,
and initial user-page allocation failure. Requested-PID rejection releases its
lock. The duplicated raising `addStackPage` and `addPage` routines are removed;
all process data-page acquisition uses the shared fallible implementation.
The raw bootstrap path still treats inability to create the initial process as
a fatal boot failure, after cleanup; there is no usable OS to return to there.

`discardUnpublished` handles only a builder-owned, never-admitted, suspended
process. It removes the kernel half from its page tables before releasing user
page-table storage, frees tracked data frames and list nodes, restores/frees its
guarded kernel stack, clears capability state, and returns the PID last. It must
never replace the retirement protocol for a previously admitted process.

The loader snapshots bounded program headers into temporary kernel-owned
physical memory (not the small kernel stack). Integer-only wire records avoid
interpreting unknown program-header tags or malformed extents as Ada enums or
constrained signed values. The snapshot is validated before process construction
and cannot change between validation and mapping. Header storage is freed on
normal success and expected rejection, including failed construction.

The current CuBit ELF profile requires at most 128 correctly sized program
headers, page-aligned PT_LOAD virtual addresses, coherent alignment, supported
R/RW/RX flags, file extents within the supplied image, filesz <= memsz, and room
below the grant/stack boundary for the image-to-heap guard. The entry point must
belong to an executable load segment. The initial image plus one stack page must
fit the actual frame tracker. Overlapping mapped segments are rejected by page
acquisition, with rollback of the unpublished process.

[Native construction tests](../tests/process-construction/README.md) cover
pre-construction rejection and eight repeated partial-load rollbacks followed
by a valid launch; the checker explicitly verifies reclaimed PID reuse.

Validation for this construction pass: 23 SPARK obligations completed with
zero unproved checks (4 functional contracts, 5 runtime checks, 10 initialization
checks and 4 termination checks). Host allocation/ELF tests and the four-vCPU
native rejection/rollback/reuse test passed. The final four-vCPU multi-app
desktop smoke test also passed, including CCL Workbench/NetSurf launches,
DOOM game pixels and a responsive Apps menu. These proofs cover the pure helper
logic, not `discardUnpublished`, source pointer accessibility, or shared kernel
page-table correctness. No runtime assertions were enabled in the kernel.

### Checked SPAWN source reads

SPAWN now validates the claimed unsigned ELF range before length conversion,
copies its header/name into kernel storage, and reads program headers and needed
segment bytes through `Process.User_Memory`. The adapter checks present/user
bits at every page-table level, rejects unsupported huge mappings, and pins
caller-owned frames before copying via the kernel physical alias. Explicit
initrd mappings retain their existing boot reservation instead of a process pin;
the exception is confined to actual initrd bytes. Failed segment reads use the
unpublished-process rollback path. No user pointer is directly dereferenced in
this launch path.

[Host tests and focused SPARK evidence](../tests/user-memory/README.md) cover
chunking, page-walk geometry and name boundaries: 36 obligations, zero unproved.
They do not prove raw page-table overlays, allocator synchronization or payload
immutability. The interface initially rejects received grants and large pages.

Native four-vCPU KVM validation passed after this change: capability-security
verified eight partial-load rollbacks and successful PID reuse; desktop-doom
with the multi-app fixture verified Workbench/NetSurf launches and closure,
DOOM game pixels and a responsive Apps menu. The kernel also passed its 2048-byte
per-function stack limit. This remains distinct from a call-chain stack proof
and from authorized native bad-pointer injection, which is still pending.

**Remaining critical boundaries:** filling the tracker with heap pages can
leave insufficient capacity for later stack growth (now rejected locally).
Other syscall buffers still need review. The header snapshot is not a signed or immutable payload
admission scheme. Other privileged page-table writers, including MAP_INTO and
the shared kernel guard-page mapping code, need a common
address-space mutation/serialization audit; an execution pin alone does not
serialize a remote authorized manager. No claim of complete exhaustion
containment, general SMP mapping safety, or kernel crash-proofness is made.

Follow-up work:

- Exercise true physical exhaustion at each construction stage and verify
  frame/page-table reclamation, beyond deterministic overlapping-map rejection.
- Add authorized native SPAWN bad-pointer/failing-source regressions; the host
  tests and native malformed-ELF tests cover different boundaries. Audit other
  syscall buffers and freeze/authenticate payload bytes for signed admission.
- Migrate the remaining ELF file-header wire overlay to raw integer fields
  before decoding enums, as already done for program headers. Checked copying
  alone does not validate arbitrary scalar representations in file metadata.
- Define whether declared stack size reserves bookkeeping/physical capacity or
  only an address range; do not let implicit assumptions substitute for policy.
- Audit every page-table writer and address-space teardown for common
  serialization and lifetime rules, including remote privileged mutation.
- Decide whether the internal bookkeeping limit should remain bounded, grow,
  or be represented differently. Do not merely increase the constant or catch
  a raised list exception after allocation.
- Add native adversarial physical/slab/map-failure and stack-exhaustion tests
  demonstrating process-local termination and subsequent reclamation. Host
  injected failures and native normal stack growth do not replace these tests.
