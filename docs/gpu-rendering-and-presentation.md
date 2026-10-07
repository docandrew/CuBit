# GPU rendering and presentation boundaries

## Rendering-to-presentation checkpoint (2026-10-04)

This checkpoint supersedes the dated bring-up status below, not the historical
test evidence. The user has observed the animated 20-teapot gallery on the NUC
at approximately 29 FPS. That is the complete application/presentation path,
not a GPU timestamp measurement. The roughly five-second hitch is unresolved.
The v60 candidate retains v59's gallery binary and timing instrumentation;
its allocation changes do not establish a performance fix.

Source-only update (2026-10-05): gallery interval-peak diagnostics now report
work, presentation and pre-submit durations belonging to the same slowest
frame. Previously the component maxima could belong to different frames.
Extracted C regression tests and the Linux-hosted gallery pass, including
invalid-clock negative controls. These are CPU intervals, not GPU timestamps;
the hitch is not yet diagnosed. The unchanged v60 image still uses the older,
independent component maxima and must be interpreted accordingly.

Periodic-work audit (2026-10-05): the inspected source does not establish a
five-second timer as the hitch source. Intel diagnostic publication is paced
at 100 ms; Desktop's `maybePrintStats` threshold is 1000 ms; gallery title and
timing reports occur every 60 frames (about 2.1 seconds at 29 FPS, not five).
The title update performs a synchronous Desktop call. Driver `Capture` and
Desktop statistics also use `debugPrint`, so asynchronous logstore publication
does not establish that diagnostic work is nonblocking. These are candidates
for measurement, not proof of a stall or reasons to remove retirement waits.
Use correlated peak-frame stages to distinguish submission/wait, validation/
presentation, and pre-submit/reporting gaps before changing synchronization.
The driver's main-loop 10 ms activity deadline is not an unconditional sleep:
queued IPC or completions wake it. No FPS gain follows from deleting that wait
without first measuring the actual scheduling path.

The present source path is:

```
ANV GPU color/depth rendering
  -> completed image-to-buffer transfer + fence wait
  -> CPU readback validation
  -> Mesa_Gallery_Surface.Frame: CPU copy/scale into Client_Frame_Pair
  -> immutable Desktop frame publication
  -> Desktop composition -> display service -> existing output backend
```

`tests/mesa-teapot/render.h` owns the GPU resources and waits before invoking
its synchronous consumer. `tests/mesa-anv/native-gallery-present.h` maps the
completed readback buffer; `mesa_gallery_surface.adb` copies it into a frame
pair. Successful publication withdraws the application's writable frame
access. This is real native hardware rendering with copy-based presentation,
not a hardware Desktop compositor, Mesa WSI, or direct Intel scanout.

### Provider boundary still required

#### Firmware writer handoff audit (2026-10-04)

The boot discovery flag is not a display lease. `Sysinfo.setInfo` currently
accepts arbitrary `GPU_IS_PRIMARY` values, including a return to zero after
native publication. Its nonzero case retires the kernel `Boot_Output`
renderer, but does not revoke mappings in userspace. Display chooses its
backend once in `setupBackend`; an existing firmware mapping survives later
publication. Only the registered device manager (or kernel-mode caller) may
set this device configuration through `handleSetSysinfo`.

The mapping admission audit found three paths in `kernel/src/syscall-ipc.adb`:

| Path | Existing admission | Handoff gap |
| --- | --- | --- |
| `handleMapFB` | Device-memory capability, physical-conflict check | No native-takeover check; partial mapping attempts may leave aliases |
| `handleMapDevice` | Physical device-memory capability with requested access rights, physical-conflict check | No framebuffer-specific lease check for a capability covering that range |
| `handleMapInto` | Process grant authority for the destination, physical-conflict check | Physical mapping can bypass a MAPFB-only gate |

These are potential routes subject to their existing authority checks, not
evidence that an ordinary application possesses those capabilities. All three
hold `Process.Owned_Memory.Lock` while mapping and then lock the destination
address space. This is an existing serialization point to evaluate, not a
license to add an independently ordered display lock. A check before acquiring
the common lock would still allow an admitted mapper to race takeover.

The implementation must close admission for the actual framebuffer physical
range, account for already published and partially published aliases, stop the
current writer, remove its mappings, and confirm the required CPU TLB drain
before native ownership becomes usable. Revoking a capability alone does not
remove existing page-table entries. Kernel renderer retirement is a separate
drain. Failure or uncertainty must retain the old backing and withhold native
write authority; clearing a discovery flag cannot restore a firmware mode.
There is no implemented runtime Display writer-drain protocol in this audit.

A private candidate makes boot publication boolean and monotonic. Its extracted
setter regression and guard-removed negative control pass; its native Sysinfo
unit compilation also passes. This is neither SMP synchronization nor mapping
revocation and has not been promoted to the primary kernel or the v60 image.
Before integrating takeover, tests must exercise both mapper/takeover orders,
all three admission paths, partial map failure, stale process incarnations,
and delayed remote TLB acknowledgment. Hardware plane writes remain disabled
until the complete ownership handoff exists.

The driver now has `Intel_GPU_Plane_Decode.Plan_Linear_Flip`, a pure
same-geometry linear-buffer planner. It uses the existing representation-clause
surface record and footprint decoder, rejecting unstable/unsupported current
state, non-page-aligned or wider-than-32-bit addresses, incomplete target
ranges and overlap with the old plane range. Its synthetic prospective live
address exists only to validate geometry; it is not a hardware observation.
No register-writing caller exists yet. Other planes, physical aliases,
producer completion, exclusive display ownership and flip retirement remain
separate admission requirements.

`Intel_GPU_Scanout_Inventory.Plan_Linear_Flip` composes that planner with a
fresh collection from the supplied plane/cursor observations. It rejects an
absent selected pipe, incomplete or unsupported inventory, and overlap of the
entire target allocation with any of the 24 possible active objects. The
selected-plane decode uses the same observations as the inventory. Hosted
tests cover all 20 selected-plane indices, their absent-pipe rejection, all
24 collision targets, adjacency, allocation-tail overlap, missing/changing
observations and invalid targets. These are GGTT-address checks: distinct
GGTT addresses can still alias physical backing. The caller must keep the
observations current under its power/serialization contract; this wrapper
does not provide that contract or perform a register write.

Sources: Intel IHD-OS-TGL-Vol2c-12.21 printed pp840–841 and848
(`~/Downloads/intel-gfx-prm-osrc-tgl-vol-02-c-command-reference-registers-part-2.pdf`,
PDF pages870–871 and878) describe SURF[31:12], flip arming and the live address.
The [Linux plane implementation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/skl_universal_plane.c.html)
was cross-checked: `icl_plane_update_arm` places CTL immediately before SURF;
`tgl_plane_min_alignment` distinguishes ordinary linear surfaces from DPT and
async-flip requirements. This moving source view is not the pinned v6.16
audit: both v6.16 URL fetches failed this session. The planner is not a
complete modeset sequence and does not enable async flipping.

`Intel_GPU_Buffer_Views.Share_Retained` now supplies one driver-private
lifetime building block: a trusted coordinator can create a read-only,
nonforwardable CPU reader from an existing registry pin, even after the BO
name closes. It takes an independent pin before grant creation and releases
it only after confirmed reader retirement (or rejection before any creation
attempt). Uncertain creation/revocation retains the pin. It does not reopen
the BO name or infer delegation rights from a pin. Recipient authorization,
producer completion and exclusion of writes remain coordinator obligations.
Hosted `view_retention_tests` exercises the actual registry/view code with
mock grant operations; this is not a new wire endpoint or GPU image import.
The native `view_retention_check.adb` also passes in the private four-CPU QEMU
fixture: real self-grants deny writable acquisition/forwarding, preserve an
already acquired read alias during revocation, and block backing release until
the reader drains. Evidence is `demand-backing.esKqSQ/serial.log` under the
private graphics workspace's `tests/intel-gpu` directory. This uses an existing
built kernel; it does not establish interprocess isolation or Intel rendering.

The following are distinct contracts, not interchangeable handles:

| Existing object | What it establishes | What it does not establish |
| --- | --- | --- |
| Intel BO presentation grant | Read-only CPU forwarding and retained backing | GPU import, image layout, GPU completion |
| Local compositor target set | Three process-local image views/framebuffers | Display authority, scanout compatibility or a latched front buffer |
| Vulkan fence completion | Completion of the associated GPU submission | Completion of Desktop/display reads |
| Desktop publication reply | Acceptance under the frame protocol | Permission to overwrite a still-consumed image |

The checked sources are `Intel_GPU_Buffer_Views.Share`,
`userspace/lib/compositor/vulkan_targets.h`,
`vulkan_device_storage.h`, and `Client_Frame_Pair`. Desktop's current manifest
requests display authority but no render authority. Display still routes GPU
calls through `CAP_SLOT_GPU`; its operation list has no GPU-image import/latch
contract. Enabling the Vulkan backend or accepting a numeric VkImage would
not fill these gaps. Existing CPU-grant checks must remain intact.

The next provider implementation needs these separately testable steps:

1. Admit a compositor render session through the existing startup authority
   policy, with software startup retained when admission is unavailable.
2. Create driver-backed output targets with authenticated adapter/session
   incarnations, stable allocation identity and validated format, layout,
   plane offsets, pitch and extent. A process-local Vulkan handle is not the
   descriptor sent over IPC; pixel storage remains in the data plane.
3. Bind each target to the selected output generation. Reject incompatible
   backing/layout or cross-adapter imports rather than silently claiming
   zero-copy support. Keep rendering ownership distinct from scanout ownership.
4. Transfer a completed target to display under an explicit consumer lease.
   Acceptance, latch and old-front retirement are distinct events. Reuse
   requires all GPU, CPU and display consumers to have retired; an ambiguous
   reply or device reset retains uncertain backing.
5. Exercise stale generations, duplicate completions, pending old-front reads,
   partial import failure, resize and adapter loss before enabling the Desktop
   path. Hosted/native protocol tests cannot validate Intel scanout registers;
   that final gate requires physical hardware.

The same-device scene probe is useful for shader/scene integration, but does
not substitute for steps 2–4. Do not enable external-memory Vulkan extensions
or writable presentation grants as a shortcut. The existing Gen12 read-only
PPGTT restriction below also means a read-only CPU grant is not enforceable
GPU read-only sharing authority.

## Allocation scaling checkpoint (2026-10-02)

Hardware candidate **v26**:
`kernel/cubit_live_mesa_triangle_repeat_v26.img`, SHA-256
`62f4b8d00532bc703f8b79509f33c900c016dea036393cea624b1091cd6b5837`.
It includes rebuilt native services and the expanded grant namespace, plus a
fresh three-cycle Mesa triangle app linked in
`tests/mesa-anv/target/native-instance-link.unrjbkud`. The graphics profile now
includes CCL Console, matching the current launch menu. UEFI four-CPU QEMU
passes Desktop startup, logstore/clock replay and USB mouse input without PS/2.
The exact packaged kernel also passes full-capacity and owner/PID-lifetime
fixtures (`tests/grant-storage/build/image-v26-kernel-gates.log`). QEMU does not
validate Intel rendering; v26 still needs the NUC three-cycle blue/red triangle
test. The broader legacy menu-position test timed out and is not claimed as
passing. v25 remains the unchanged physical-hardware baseline.

### Kernel grant scaling boundary

Kernel `Memory_Grants` and runtime `CuBit.Grant_References` now share a
1,048,576-slot global namespace: 4096 slots per owner. Receive addresses use a
fixed 16 MiB stride from 64 TiB to the exclusive 80 TiB limit, immediately
before the bootstrap initrd. Records are committed lazily in 64-record blocks.
The cross-layer test in `tests/grant-references` checks every slot, both codecs'
bounds and separation from the initrd and owned-memory aperture at 88 TiB.
The Mesa mapping bridge uses the same validator rather than its old literal
4095 ceiling. This changes slot geometry; rebuild native consumers together.
Committed metadata capacity must never become the divisor used to decode IDs
or calculate receive addresses: growing storage cannot move a live mapping.

The native kernel debug layout measured on 2026-10-02 is 56 bytes for `Grant`,
544 for the default 16-child forwarding `State`, and 40 for `Parent_Link`.
The original arrays reserved all three for every possible identity. Merely
expanding to 1,048,576 global identities would require 640 MiB of this metadata,
before backing pages or page tables. Forwarding fanout is now an independent
generic parameter. Measurement evidence for that baseline:
`/tmp/cubit-grant-layout-size3.log`, kernel SHA-256
`ebb905d08a8e17363145d3136bef37e8ae747f0a106520f058a1814f2a17327c`.
These sizes describe this native build, not an ABI guarantee.

Forwarding scopes now use lazily allocated, stable kernel page blocks. Only an
authenticated derivation with an available child slot can allocate a block;
ordinary grant creation does not. Allocation failure returns before taking a
parent hold or installing a child mapping. Seven scopes fit in a 3808-byte
block on this native build, backed by one 4096-byte kernel allocation; the
static pointer directory was 4688 bytes at the former 4096-global-slot limit,
rather than the old 2,228,224-byte state array. The expanded namespace increases
directories, not eagerly constructed records. Published blocks are retained for
the kernel lifetime. Scope reset still
requires completed child retirement, so reusing an identity cannot reset live
neighbours. This reduces eager storage; it is not kernel metadata reclamation.
The subsequent namespace expansion removes the 16-grant owner ceiling.
Grant records and parent
links now use their own lazy retained blocks. Ordinary grants allocate neither
scope nor parent-link storage. Child-link storage is admitted before taking a
parent hold, reserving a loan, or installing mappings. Missing links mean no
parent; acquire/return lookups are performed inside the grant lock. Retirement
clears the child's link only after unmapping and the loan-retirement transition.
Native four-CPU QEMU forwarding-retention and mapping-growth fixtures
pass. The forwarding fixture additionally passes 32 rounds with eight retained
root/child pairs spanning multiple scope blocks: neighbour acquisitions survive
new block initialization, a seventeenth grant is admitted and retired, and
reverse retirement rejects stale readers before the next reuse round.
Physical-exhaustion fault injection remains a separate check. This
pointer/allocation adapter is not SPARK-proved.

`Retained_Record_Blocks` now supplies the production forwarding storage.
Its hosted tests inject OOM before each growth, reject misaligned/wrapping
allocator responses, and check stable records across sixteen blocks. A second
fixture uses actual forwarding state and preserves a live parent across ten
blocks. Native forwarding/mapping regressions pass with this adapter; native
physical-exhaustion injection is still absent. In-place initialization avoids
a whole-page stack temporary caught by the kernel's stack-usage gate.

Parent-link integration evidence: `tests/grant-storage/build/parent-links-native.log`
and native fixture directories `tests/intel-gpu/demand-backing.uan2nw` (32 rounds
of retained root/child pairs plus the three-cycle view fixture) and
`tests/intel-gpu/demand-backing.WsRMLI` (mapping growth). Both passed with kernel
SHA-256 `74a3326c4e49455337696873525be551c602256be2643efcf9032f9606bc67f9`.
This is native kernel/IPC evidence in QEMU, not a new Intel hardware test.

Main grant records now use an external per-PID retained store with 64 records
per block. `Process` no longer contains a grant array. Initialization captures
the process-life generation without allocating, rejects an old/repeated life
or any active committed record, and resets only committed records in two
passes (validation before mutation). New blocks receive that captured life.
Admission allocates before installing mappings; missing-record queries do not
allocate. All pointer lookups are under the grant lock. Owner/receiver teardown,
forwarding closure, PID-release checks, and life reset use committed-block
iteration. Admission still searches within the unchanged 16-slot namespace.

Integration evidence: `tests/grant-storage/build/grant-records-native-r2.log`,
native views `tests/intel-gpu/demand-backing.GfX5Fs`, mappings
`tests/intel-gpu/demand-backing.KYXleC`, and
`tests/grant-storage/build/grant-records-capability.log` all pass. The broader
capability test verifies eight failed-load rollbacks and successful reuse of
the same PID. Kernel SHA-256:
`25c70382b0e3d2af09e6023641a39c9bc5822b0a19d406acdd04bf2f9f0f270e`.
Subsequent native tests cover a dead owner with a held grant after its parent
receives the kernel retirement event: readable backing, PID reuse denied while
held, return after endpoint invalidation, exact PID/slot reuse with a fresh
generation and rejection of stale endpoints, grants and returns. The expanded
capacity fixture holds all 4096 acquisitions, checks exhaustion, retires/reuses
a block-boundary slot and drains everything. Evidence and exact hashes are in
`tests/grant-storage/README.md`; the expanded kernel is
`109b21724d28b3c6fecb8e41a4cab2f702266736bec61c5aa59760ba2c25e5ea`.
The codec has 20 proved checks, zero unproved; these native lifecycle tests
are regression evidence, not a concurrency proof. Native allocation-failure
injection, metadata reclamation and policy quotas remain separate work.

Process-lifetime audit: `Process.resetProcessRecord` currently zeroes the
entire process record with `Util.memset`, and `Process_Table` can reclaim its
containing pages after quiescence. A retained grant directory therefore must
not simply be embedded into `Process.grants`: resetting a PID would erase its
owning pointers, leak backing, and bypass the intended storage lifetime. Keep
the retained directory outside reclaimable process records, indexed by stable
PID/global-slot identity. Initialize each newly committed grant block with the
current process-life generation; reset previously committed records only after
the existing grant teardown/PID-retirement gate allows that PID to be reused.
Generation validation and committed-block iteration must be part of that
integration, including a test that reuses a PID while rejecting old references.
The external sparse store implements this placement constraint. The tested
PID reuse is failed-launch rollback; it is not evidence for the outstanding
dead-owner/held-reader scenario described above.

Do not infer grant-store validity by comparing its captured life against the
current endpoint generation on every lookup. `Process.reclaimProcess` advances
the process-table generation to invalidate endpoint capabilities **before**
`prepareGrantProtectedTeardown` and the final borrower returns. Outstanding
grant returns must still find the old retained records during that interval.
Capture the grant life at process creation, preserve it through deferred
retirement, and transition it only at the next authorized PID initialization.
The regression must cover both this intermediate invalidated-but-retained state
and subsequent PID reuse; a test of generation arithmetic alone is insufficient.

The kernel migration must use stable, lazily backed record blocks, with
forwarding scopes allocated separately on authenticated delegation. Ordinary
CPU mappings must not pay for a full array of potential descendants. Metadata
allocation failure must reject admission without publishing a grant or losing
existing records. Namespace bounds, committed storage and resource quotas are
three separate limits. Owner/receiver teardown must traverse allocated/live
records rather than scan the theoretical namespace. Dynamic lookups must occur
under the existing grant lock; several static-array renames currently precede
that lock and cannot simply become pointer dereferences. Any scope storage
reuse still requires child retirement, acknowledged unmapping and release of
the dedicated parent hold. This is a migration requirement, not implemented
full lifecycle coverage or a proof of the native locking code.

### Driver allocation path

The immediate Mesa allocation failure on the NUC was exhaustion of 16 backing
metadata slots, not exhaustion of system RAM. The new path separates allocation
identity capacity from backing bytes. Six driver registries grow together and
only admit a larger slot range after every supporting table is initialized;
the supervisor grows its extent metadata independently while retaining the
original request/reply authority. Stable CPU virtual reservations acquire
backing on demand in at most 64 KiB commit/clear steps. VM-update images are
also created on demand rather than preallocating one large image per possible
identity. Failure retains existing state and does not replay an allocation.

Hosted growth/failure/lifetime regressions and native compile/link pass. The
native QEMU fixture also passes two demand-growth rounds to 20,000 metadata
records using the production record store, growth coordinator and owned-memory
syscalls, checking retained values, typed defaults and quota rejection. Its
separate reservation lifecycle and Mesa no-provider lifecycle checks pass too.
This does not exercise the complete six-registry driver/supervisor IPC path or
Intel hardware; physical NUC Mesa allocation remains a separate gate. This is
not a SPARK proof of imported storage, placement construction or the registry.

Current limits remain explicit: the GPU backing arena is 32 MiB, an individual
BO is at most 16 MiB, and its backing uses the existing below-4-GiB allocation
path. Metadata policy permits up to 1,048,576 identities with a 64 MiB virtual
reservation per table; these are ceilings, not preallocated records or promised
RAM. Sparse VM-update image storage has its own 256 MiB CPU virtual quota.
Growing metadata does not remove those GPU backing/addressability limits.

The subsequent demand-backing implementation treats 32 MiB as an admission
ceiling, not an eager physical allocation. It acquires retained 2 MiB extents
through the end of each granted slice, and address queries only inspect the
committed snapshot. The driver fetches missing extent replies incrementally;
older views retain their immutable prefix. Internal accounting distinguishes
committed backing from live assigned bytes and remaining quota. The existing
Mesa budget reports quota, not a physical-RAM guarantee: allocation can fail
inside it. Physical blocks are not released when slices retire. Hosted tests
cover growth, failure, alias rejection and generation reuse; native hardware
execution of this demand path is not yet verified. The published v18 image
predates this change.

The supervisor's subsequent stepped path performs at most one new 2 MiB physical
allocation per service turn. Its dispatcher retains the authenticated request
and saved reply capability across pending steps; it rejects another allocation
request until the first completes, and emits only one terminal reply. This
bounds physical-allocation callbacks per turn, not total latency: first-fit
extent search remains linear and a kernel allocation may itself take time.
Hosted tests cover an eight-turn allocation and ownership loss while pending;
this later change is not included in v19.

The v20 NUC candidate includes the stepped supervisor path:
`kernel/cubit_live_mesa_triangle_repeat_v20.img`, SHA256
`6351a00cb830b99088f535d8544d93d84ed863bc6df42ae626a3d691d2220bab`.
Native service builds, image audits, UEFI USB boot without PS/2 and boot-log
delivery passed. QEMU denied Intel render admission; this is not a successful
Mesa-rendering run. The image's kernel is recorded in
`tests/mesa-anv/target/v20-temp.MuZYyN/input.sha256` and is newer than the kernel
used by the isolated allocation oracles below. On NUC, require device creation
and all three Mesa triangle cycles to retire and clean up on the same device;
the separate red driver triangle alone does not establish that result.

The newer **v21** candidate adds compact per-BO directory views, CPU-export
backing pins and pin-aware retirement preflight:
`kernel/cubit_live_mesa_triangle_repeat_v21.img`, SHA256
`2232e8a76d77d6c5680ed43a1fe6395cd95b9c0d06133f143e36606cf23710b9`.
Service builds, image audits and UEFI4CPU USB/noPS2/boot-log delivery pass;
evidence is in `tests/mesa-anv/target/v21-temp.AZ17PZ/`, including input hashes.
The staged test app was restored after packaging. v20 is preserved. No physical
NUC result exists for v21 yet; use the same three-cycle acceptance gate above.

For large discrete GPUs and terabyte-memory workstations, keep these quantities
independent:

- Object identity and per-client object quotas.
- CPU virtual reservation, committed system pages, and CPU mapping page size.
- GPU virtual reservation and the device's supported GPU page sizes.
- Device-local VRAM, system-memory backing, and current residency budgets.
- Device-visible DMA/IOMMU addresses and each adapter's address-width limits.

The **v23** NUC candidate packages the complete demand-backed directory path,
bounded duplicate index and constant-work retirement unlink:
`kernel/cubit_live_mesa_triangle_repeat_v23.img`, SHA256
`9f771296fedf11d3f7025fe29df28200d80085cfeedc34bbf7f427286f9e2b99`.
Native service builds, firmware/Mesa/image audits and four-CPU UEFI USB boot
without PS/2 plus boot-log delivery passed. Input hashes and boot evidence are
in `tests/mesa-anv/target/v23-temp.oscSBd/`. Its staged kernel differs from v22;
this packaging run does not claim to rebuild the kernel. QEMU explicitly denied
render admission, so native Intel/Mesa rendering remains a NUC gate: require
all three same-device triangle cycles to retire and clean up, not merely the
separate red driver triangle. The staged app was restored; v22 is preserved.

No registry should allocate metadata proportional to all installed RAM or VRAM.
The Mesa budget adapter has also been regression-tested using configured Mesa
types at synthetic 24 GiB, 1 TiB, 4 TiB and near-uint64-limit capacities: 320
wide cases cover exhaustion, retained bytes, metadata availability, transport
failure and overflow-safe accounting. The test oracle no longer infers byte
usage from occupied ticket count. Five production adapter compilations and ten
hosted mock-IPC fixtures pass (`/tmp/cubit-mesa-wide-budget.log`). These checks
establish arithmetic/adapter behavior only, not large physical allocations,
discrete VRAM support, cache coherence or native GPU execution.
Future heaps should reserve address space independently of physical extents,
commit backing incrementally, and use indexed extent/identity lookup rather
than scans proportional to the entire policy ceiling. Large physical extents
and large CPU/GPU page mappings are separate optimizations with explicit
alignment and fallback rules. A large CPU virtual range is not authority to DMA
to it and is not evidence of physical contiguity.

Discrete-memory migration/eviction, per-client byte quotas, allocation beyond
4 GiB, and larger BOs remain future work. Reclaim must wait for GPU completion
and any outstanding display/import references; unknown completion retains
backing. Allocation, residency and completion messages remain IPC control-plane
operations, with pixels and commands in authorized shared/device memory.

### Next scaling gates

**End-to-end limit found in the kernel:** `Memory_Grants.Grants_Per_Process`
is still 16. `Process.IPC.createGrant` scans that owner's fixed slot array and
rejects creation when no reusable slot remains. This is stricter than either
the driver table or Mesa bookkeeping. Driver metadata growth therefore does
not yet enable more than 16 simultaneous owner grants. Grant-ID encoding and
revocation decode, the received-memory aperture, and loan indexing all depend
on this constant; changing just the graphics tables cannot remove it. Kernel
grant identity/storage/VA scalability must be addressed with the shared-memory
protocol, preserving generation validation, retirement and quota enforcement.
Native storage-growth tests can retain a few real readers while retiring other
grants, but must not present that as proof of many simultaneous kernel grants.

The driver-side CPU grant table starts with 64 inline records. Mesa-side
bookkeeping growth alone does not expand the driver table. The sharing
dispatcher now rejects unknown/closed BO names before reserving a view slot,
and can reuse a slot left empty by admission stopping before grant creation.
Every reuse still consumes a fresh mapping ID. Live, retiring and failed views
are never reclaimed this way. Hosted regressions check 192 rejected requests
each for an unknown BO and an absent recipient, then successful mapping, plus
4096 writer/presentation retirement cycles. This is an admission-exhaustion fix,
not dynamic grant-table growth or cross-session GPU import.
The native Intel driver also compiles and links with this admission fix; v25
predates it. Growing the table requires stable-address storage because entries
contain limited lifetime tokens. Its quota must be independent of BO metadata:
one BO can have several CPU readers. The existing pre-receive growth gate can
leave requests and their reply authority in the kernel while bounded metadata
steps run; MAP must still execute once, not rely on blind client retries.
The table now exposes stable-prefix metadata extension with typed initialization
of new limited records. Hosted tests retain live grants through three extensions
(4, 8 and 16 KiB), reject rebasing and foreign retirement, drain delayed pins,
and reject stale mapping IDs after reuse. This storage code is not SPARK-proved.
The service loop now connects an independent record-growth controller with a
64 MiB metadata-byte quota and one-million-record policy ceiling. It begins
growth before receiving another request when no empty or retired slot remains,
and advances one phase per turn. Requests and reply authority remain in the
kernel until growth finishes. Quota rejection, commit failure or ownership loss
disables further growth without discarding the prior prefix or preventing
retirement/reuse through normal request handling. No MAP is replayed.
Hosted tests compose the real registry and growth controller, check bounded
commit work and no grant creation during growth, and inject commit failure
before successfully retiring/reusing an old slot. The native driver compiles
and links. End-to-end native grant growth is still untested; v25 predates this
driver storage/controller integration.

After v24, Mesa CPU-mapping bookkeeping no longer stops at 64 outstanding
borrows. It retains 64 inline records, compacts only confirmed-retired entries,
and grows host storage geometrically when every remaining record is outstanding.
Growth preserves ordering for newest-borrow lookup; no CPU or GPU mapping moves.
Checked size arithmetic and allocation failure precede mapping IPC. Dynamic
storage is freed only after every CPU grant retirement is confirmed; pending or
failed records retain storage and sticky device loss still prevents new work.
This does not increase physical backing, session quotas, or GPU import rights.
`tests/mesa-anv/test-mapping-growth.sh` passes under ASan/UBSan with 4096 live
mock mappings, pending drain and five injected metadata-allocation failures.
The configured Mesa regression runner also passes all 11 hosted fixtures.
These are hosted transport tests, not native execution. Matching native Mesa
archives and the real three-cycle window application subsequently compiled and
linked with strict transport-input checks in
`tests/mesa-anv/target/native-instance-link.lkiutwez/`; `inputs.json` records the
source/archive hashes. Native execution and NUC validation remain required.
The v24 image is unchanged and does not contain this Mesa-side change.

The post-v22 extent directory uses a digital-search index for duplicate DMA
admission: at most 44 key-node reads for aligned 2 MiB extents, independent of
the number of committed entries. This is not a bound on allocation latency.
Each entry is now 16 bytes (DMA address plus two 32-bit child indices), rather
than 8; 64 KiB of committed metadata holds 4096 entries plus 16 inline entries.
The index never relocates published DMA addresses or widens an existing view.
Hosted tests exercise four 4096-key orders (ascending, descending, permuted,
and high-bit addresses), duplicate rejection and retained prefixes. These are
synthetic address tests, not physical large-memory allocations or SPARK proofs.
Native devmgr and Intel-driver builds also pass, as does the actual-kernel
18-extent/36-MiB IPC allocation oracle (`tests/intel-gpu/demand-backing.qZHICF/`,
serial output and exact input hashes retained). The oracle is a one-process
test router, not cross-process authority validation or Intel GPU execution.
The published v22 image remains unchanged and predates this indexing change.
Free-slice first-fit search remains linear; discrete VRAM residency, eviction,
device-derived budgets and wider native DMA policy remain separate work.

Buffer retirement now uses reciprocal predecessor/successor links rather than
scanning the live list for a predecessor. Both neighbor links and address order
are checked before mutation; invalid linkage quarantines the pool without
freeing backing. This bounds unlink work, not allocation search or GPU wait
latency. Generation checks and the trusted all-references-retired requirement
remain mandatory. Hosted regression covers 128 buffers removed in ascending,
descending and permuted order over three generations, exact budgets and no
extra physical callbacks on reuse, in addition to the existing 256 removal-mask
tests. Native devmgr/Intel-driver builds and both allocation oracles pass:
`tests/intel-gpu/demand-backing.yksQRJ/` covers 18 extents/36 MiB and saved IPC
replies; `tests/intel-gpu/demand-backing.DfKv1p/` covers 18 MiB, 17 objects,
4112 page sentinels, middle-buffer retirement/reuse and stale-generation
rejection. Each retains serial output and input hashes. Neither fixture
publishes GPU mappings or proves real GPU/display reference retirement.

After v23, the metadata growth controller also rejects a request beyond current
capacity without mutation when its entire byte quota is already published.
This known exhaustion does not poison reuse of existing records or issue new
allocation callbacks. Tests check the unchanged snapshot and subsequent valid
request; six hosted growth suites and native service/IPC regression pass
(`tests/intel-gpu/demand-backing.VBZ7mT/`). Quota exhaustion discovered during an
already accepted growth operation remains terminal, as do ambiguous commit or
publication failures. This is a narrow preflight recovery rule, not general
allocation rollback or physical-memory reclamation.
The hosted saved-request dispatcher regression additionally checks that this
rejection consumes exactly one saved reply, performs no backing callback or
metadata commit, preserves the remaining record budget, and accepts a subsequent
allocation within initialized capacity. It uses the real allocator and growth
controller with mock reply transport, not a kernel IPC security test.

Deferred BO retirement now polls only pending candidates, not every committed
metadata slot. An intrusive FIFO performs at most one preflight callback per
service turn; waiting readers rotate to the tail, submitted/discarded candidates
are removed, and newer generations update in place. This avoids retirement
latency proportional to empty metadata capacity without relaxing GPU/VM/grant
checks. Hosted tests cover 2048 candidates, four metadata-growth boundaries,
multi-item fairness, replacement and empty-queue reuse; native driver compilation
passes. The queue storage is not SPARK-proved and this post-v23 change has not
been tested with Intel GPU execution. Callbacks must not reenter or mutate it.

Source audit: `Intel_GPU_Buffer_Reply.Extent_View` embeds an entire
`Intel_GPU_Physical_Extents.Map` by value, and each BO's `Backing` contains that
view. `Extent_Allocator.Snapshot` and `Extent_Replies.Assembly` also copy the
map. At 2 MiB per extent, a hypothetical 24 GiB arena needs 12,288 addresses
(96 KiB of addresses alone); a 1 TiB arena needs 524,288 (4 MiB). Copying that
directory into every BO is not an acceptable scaling strategy. These numbers
are capacity examples, not claims about a particular GPU's specifications.

The replacement must store the directory once per authenticated arena owner.
BO views should contain a retained directory identity, immutable committed
prefix/version, offset and length, not the directory itself. Published entries
must never change or move while any view remains; metadata growth may add
chunks but cannot redirect old views. Ownership loss invalidates access while
retaining uncertain backing. CPU addresses must remain optional for VRAM-only
heaps. Directory metadata quota is distinct from backing and BO quotas.

Implement this behind `From_Extents`, `Page_Address`, `Slice` and `Same_Arena`
before changing the allocation policy ceiling. Admission must still reject
duplicate/overlapping DMA extents, authenticate each incremental reply, and
keep old prefix views usable after growth. An opaque arena number alone is not
enough: it cannot substitute for a retained directory and endpoint incarnation.
Tests must cover two independently allocated directories with equal numeric
IDs, growth after old views exist, stale references, owner loss, sparse metadata
commit failure, and range overflow. A later indexed allocator must also remove
the current linear first-fit work; a larger directory alone does not do that.

The existing capacity and decoder count bounds now derive from the physical
extent definition, avoiding contradictory copies of the 32 MiB/sixteen-entry
limit during migration. This does **not** enlarge the current arena.

The first replacement core, `Intel_GPU_Extent_Directory`, now supports one
append-only address store with demand-committed metadata and compact root-bound
prefix views. Resolve explicitly receives the owning directory; it never
dereferences an address supplied by a client. Old views cannot see a newly
appended suffix. Reinitialization is rejected and quarantine invalidates all
views without freeing backing. Owners and metadata must outlive every view;
these are internal descriptors, not transferable capabilities.

Hosted `extent_directory_tests` passes 600 scattered extents, two metadata
commits, 16-byte views, unchanged old prefixes, foreign roots, duplicate and
misaligned addresses, quota/overflow rejection and quarantine. Addresses above
4 GiB are synthetic geometry under explicit test policy; no GPU memory is
allocated. The supervisor extent allocator now uses this directory as its
canonical retained address owner instead of a raw address array. Its existing
snapshot boundary still materializes `Physical_Extents.Map`. Driver reply
assembly now also owns an append-only directory, preserving exact incremental
reply identity/CPU-address checks and quarantining the directory on cancellation.
Its boundary still materializes a map; per-buffer views have not yet migrated.
Decoder regressions, native driver compilation and the combined native IPC
oracle pass (`tests/intel-gpu/demand-backing.ipEkQh/serial.log`).
Allocator regressions pass
ownership-loss/failure, growth, fragmentation and generation reuse, and devmgr
compiles natively. The native QEMU IPC oracle also passes with this allocator
(`tests/intel-gpu/demand-backing.WNHPQp/serial.log`): actual kernel memory/IPC,
but a one-process test router, not cross-process devmgr or GPU rendering.
The core is not SPARK proved and changes neither DMA policy nor the v20 image.
Its typed `Borrowed_View` now provides compact lookup without supplying the
directory on each call. This is a trusted lifetime boundary using an internal
Ada access value, not an owning reference: the aliased, limited service owner
and metadata must outlive every descriptor, with no owner-address reuse.
`Unchecked_Access` expresses this external retention assumption; it is not a
proved lifetime property. The descriptors cannot be constructed from IPC.
Quarantine makes lookups fail and growth cannot widen an old prefix. Both
allocator and reply assembly use this lookup to materialize their transitional
maps. Hosted checks, native devmgr/driver builds and native IPC pass
(`tests/intel-gpu/demand-backing.UCU6VE/serial.log`); per-BO copies still remain.
`Buffer_Reply.Extent_View` now stores a borrowed directory prefix rather than
an embedded `Physical_Extents.Map`. Supervisor buffer construction and driver
allocation completion use the retained owners; all page lookup, slicing and
overlap checks use the compact representation. Arena comparison requires the
same directory owner, not merely matching numeric arena IDs or equal addresses.
Quarantined directories invalidate existing views. Hosted checks enforce at
most64bytes per extent view and96bytes per backing (independent of directory
capacity), all8192 scattered PTEs, all32 interior context-buffer splits,
allocator/metadata growth, CPU export retention and transport failure cases.
This does not prove the borrowed lifetime assumption. Native devmgr and Intel
driver compilation now pass, as does the native allocation/IPC oracle with the
compact representation (`tests/intel-gpu/demand-backing.B5Hn5N/serial.log`):
17 allocations, three incremental extent replies and one interleaved request.
Its exact kernel and fixture hashes are recorded in the adjacent `input.sha256`;
this is actual kernel memory/IPC in the isolated one-process fixture, not GPU
execution or a cross-process security test. NUC rendering remains unverified.

The driver reply decoder and allocation client now also retain only directory
prefixes: `Extent_Replies.Result` is the single borrowed-view API, and accepting
the final reply no longer rebuilds a fixed-size address array. Hosted regression
checks cover all incremental prefixes, old-prefix bounds/address preservation,
quarantine invalidation, late failures, allocation identity and retirement.
The service-incarnation owner lifetime remains a trusted obligation, not a
proved property. Native devmgr/Intel-driver builds and the actual-kernel IPC
allocation oracle pass for this follow-up
(`tests/intel-gpu/demand-backing.3Bg0V3/serial.log`, exact input hashes adjacent).
This does not test Intel hardware or expand the arena's current policy limits.

The supervisor allocator and devmgr extent query now use borrowed prefixes too;
there is no remaining `E.Map` materialization in the allocation path, and the
directory's committed count replaces the allocator's duplicate count. Mutation
failures quarantine the directory consistently: old views become invalid while
all physical backing remains retained. A read-only query still cannot allocate
or reclaim anything. Hosted allocator and reply regressions pass. Native
devmgr/driver builds and both actual-kernel allocation oracles pass:
`demand-backing.Xv4D6p` (IPC) and `demand-backing.Qu9g0z` (18 MiB physical
backing, 17 objects), under `tests/intel-gpu/`, with serial logs and exact input
hashes. These are allocation tests, not Intel hardware rendering validation.
The supervisor now has trusted pre-use `Configure_Heap` policy and separate
`Extent_Capacity`/`Extend_Extents` metadata hooks. Allocation preflights metadata
capacity before invoking any physical callback; known metadata pressure can be
resolved without retrying an ambiguous physical operation. The existing bounded
`Record_Growth` controller is tested against these hooks: a 24 GiB policy, 600
synthetic scattered extents above 4 GiB, one 64 KiB metadata commit, preserved
old prefixes, metadata quota exhaustion and owner revocation. This allocates
only test metadata, not 1.2 GiB of physical backing. Production adapters still
use the default 32 MiB/below-4-GiB policy. Connect supervisor scheduling,
driver-side directory growth and matching trusted policy before enabling a
larger live arena; these hooks alone do not provide that integration.
The default-policy native devmgr/driver build and actual-kernel IPC regression
pass (`tests/intel-gpu/demand-backing.28fwBX/serial.log`, input hashes adjacent).
This does not establish native execution of the synthetic large-heap case.
The driver reply assembly now accepts the same explicit trusted quota and DMA
limit, with independent metadata extension. Counts are bounded by validated
policy rather than the old sixteen-element array. Tests decode 600 synthetic
replies across two metadata extensions, preserve old-prefix bounds, hide partial
results and reject above-4-GiB addresses under the unchanged default policy.
Hosted transport/retirement regressions and native build/IPC pass for this
decoder change (`tests/intel-gpu/demand-backing.SholgJ/serial.log`).
`Buffer_Memory` now has a Natural extent index and trusted pre-use heap policy.
Its `Tick` state machine grows a separate CPU metadata arena one phase per
turn, retaining the original allocation transaction and requesting only the
missing extent afterward. Production main supplies the actual owned-reservation
platform. Hosted tests exercise the seventeenth extent, stale-completion
rejection while waiting, reservation/commit/clear failure, timeout, owner loss,
and the bootstrap wrapper; none replays the physical allocation request.
Native builds and the default three-extent IPC regression pass
(`tests/intel-gpu/demand-backing.yYoNh7/serial.log`), but that native fixture does
not yet exercise metadata growth. Supervisor saved-reply scheduling now uses
`Extent_Growth`, which responds to explicit metadata pressure before a physical
callback and advances the existing metadata controller one phase per turn.
Hosted tests cover its seventeenth extent and reservation/commit/clear/owner
failures, retaining backing without physical replay.

The expanded native IPC fixture explicitly configures matching 64 MiB test
policies and passes with **36 MiB actual backing, eighteen extents, seventeen
saved replies and one interleaved request**. It checks both directory growth
paths, one physical block at most per turn, exactly-once reply consumption and
earlier buffer sentinels. Evidence: `tests/intel-gpu/demand-backing.dlureW/`,
serial log and exact input hashes. This is a privileged one-process loopback,
not full service admission/isolation or GPU rendering. Live service policy
remains 32 MiB; its shared quota/address policy must be updated consistently
before a larger live arena is enabled. The old sixteen-entry extent-query gate
has been removed: devmgr and the native router use the same pure admission
predicate, bounded by the actual committed prefix rather than a static array.
It authenticates sender/authority/arena and rejects malformed, uncommitted or
overflowing indices before offset arithmetic. Hosted tests cover 600 indices
and invalid envelopes/bounds; the eighteen-extent native gate passes with this
predicate (`tests/intel-gpu/demand-backing.k6EY9f/serial.log`).
The bootstrap BO policy is now centralized as `Buffer_Backing.Default_Heap`:
backing quota, DMA address ceiling and extent-metadata budget. Allocator,
driver memory client, reply decoder, supervisor metadata adapter and live BO
DMA syscall use these defaults. `Heap_Geometry_Valid` supplies the common
canonical-address/alignment/count checks for trusted configuration; it does
not discover hardware capability or grant access. Six hosted suites and the
native eighteen-extent gate pass after consolidation
(`tests/intel-gpu/demand-backing.pGPWDi/serial.log`). Defaults remain unchanged.

The combined follow-up is packaged in
`kernel/cubit_live_mesa_triangle_repeat_v22.img` (404684800 bytes), SHA256
`d7b0ae7870c63dc52a6e43a9df051ccb765ad80be8fc3b45a45afe8e66a46c94`.
Image audits, four-CPU UEFI USB boot without PS/2, and boot-log delivery pass;
evidence is in `tests/mesa-anv/target/v22-temp.fG61i6/`, including input hashes.
The QEMU run denies render admission because it has no supported Intel GPU;
it does not validate Mesa GPU execution. NUC acceptance remains device creation,
triangle-window success and all three cycles retired/cleaned on the same device.
v21 is preserved. The live quota remains 32 MiB; native tests separately exercise
64 MiB policy and 36 MiB actual backing.
Duplicate admission
still scans existing entries and needs an indexed replacement for large heaps.

Do not turn the NUC's 32 MiB arena into a multi-gigabyte eagerly backed pool by
raising constants. The next implementation steps are:

1. Replace the fixed sixteen-entry backing snapshot with a demand-committed,
   indexed extent directory. Keep allocation identity stable while directory
   storage grows; request only the missing directory ranges. Bound both lookup
   and allocation work per service turn, not merely the number of DMA calls.
2. Negotiate per-adapter address width, supported page sizes and heap types.
   Keep 64-bit checked byte arithmetic through allocation, alignment, mapping,
   accounting and transport. The current below-4-GiB policy is a bring-up limit,
   not a rule for all adapters or all system memory.
3. Account separately for reserved VA, committed backing, live object bytes,
   pinned/non-evictable bytes and uncertain-retirement bytes. Add per-client
   quotas and a device-wide pressure budget; reject excessive reservation
   metadata even if physical backing is lazy. A budget is not an allocation
   guarantee, and available system RAM does not imply available device VRAM.
4. Introduce typed allocation outcomes before retryable pressure handling.
   Currently an ambiguous/failed physical acquisition poisons the retained
   pool. Future known-no-allocation pressure may allow a later new request;
   unknown completion or lost ownership must retain/quarantine backing, never
   blindly retry. Define cancellation and partial-growth accounting explicitly.
5. Add reclaim and, for discrete heaps, migration only after cross-client
   imports, GPU completion and display retirement have reliable ownership
   accounting. CPU mappings and device DMA mappings need separate invalidation
   acknowledgments. An IOMMU mapping is not a CPU or GPU virtual address.

The first native allocation-only oracle is implemented in
`tests/intel-gpu/native/demand_backing_check.adb`. It is a privileged disposable
test supervisor, not an ordinary app or production boot option. It uses the
production extent allocator and metadata growth with actual kernel syscalls;
its checks cover 17 objects, nine demand-allocated 2 MiB blocks,
4,112 distinct page sentinels, metadata extension, slice reuse and stale
retirement rejection. Four-CPU native QEMU execution passed in
`tests/intel-gpu/demand-backing.PnS7R8/serial.log`; the native fixture README
records kernel and executable hashes. Owner unavailability is injected through
the callback, not established by a process-revocation race. The test does not
establish allocation IPC correctness, GPU DMA, GPU page-table
visibility, residency migration or hardware rendering. Production authority
checks remain unchanged.

A subsequent native loopback oracle composes the production driver memory
state machine and supervisor growth/extent allocator with actual kernel async
IPC and saved reply capabilities. Four-CPU QEMU passed 17 allocations, metadata
growth on both sides, three incremental extent replies, one unrelated request
while the allocation reply is saved, exactly-once reply consumption, and driver
zero/flush/readback without corrupting earlier buffers. Evidence is
`tests/intel-gpu/demand-backing.1CNi90/serial.log`. This uses a self endpoint and
a test router in one privileged process, not full devmgr or distinct protection
domains. Native cross-process admission/lifetime and physical NUC Mesa execution
remain separate gates; this result must not be advertised as GPU validation.

Design decision, 2026-09-27. This is the target architecture, not a declaration
that native Intel rendering, multi-adapter routing or zero-copy is implemented.
It extends [display outputs](display-outputs-and-scaling.md) and
[buffer lifetimes](display-buffer-lifetimes.md), retaining their session,
grant, completion and desktop-layout contracts.

## Two paths, one resource lifecycle

### Hardware cursor milestone (requested 2026-09-30)

Add an adapter-owned hardware cursor path through Desktop -> display -> driver,
with a software-composited fallback. Desktop retains logical pointer position,
shape/hotspot and per-output scaling policy; display resolves the output and
the driver validates cursor format, dimensions, placement and backing. Apps
must not receive cursor-register or scanout authority. Movement should update
position without uploading the shape again or triggering full-scene rendering.
Shape replacement must retain the old backing until hardware no longer reads it.

Acceptance: correct hotspots/clipping at negative and edge coordinates,
mixed-DPI/output transitions, shape replacement, fallback and driver retirement;
responsive movement while scene rendering is busy. Target a supported 240Hz
display mode, but verify monitor EDID, connector/link bandwidth and hardware
timings first. Measure input-to-visible latency separately from IPC completion
and nominal refresh rate. No hardware-cursor or 240Hz support is claimed yet.

Presentation control follows app -> desktop -> display -> adapter driver.
Rendering does not traverse that whole chain: an authorized application uses
its render context at the GPU service directly. Mesa is an application-side API
implementation, not a privileged display owner. Desktop is another render
client, with separate presentation authority.

An app publishes an authorized image reference and render-completion dependency
to its desktop surface. Desktop chooses composition or direct scanout; display
routes presentation to the output's owning adapter. IPC carries descriptors,
references, damage, dependencies and outcomes, not pixels. The shared data plane
has its own allocation, mapping and retention lifecycle. Batch submissions;
do not turn each graphics API call into an RPC.

| Component | Owns | Does not confer |
| --- | --- | --- |
| App / Mesa | Its surfaces, render contexts and authorized allocations | Modesetting, MMIO, arbitrary DMA, other clients' images |
| Desktop | Composition, placement, primary display and workspace policy | Unrestricted device access |
| Display | Output registry, presentation sessions and configuration arbitration | Authority manufactured from output numbers |
| Adapter driver | Hardware programming, GPU address spaces, engines, synchronization and scanout | Implicit permission for clients to share resources |
| Kernel / device provisioning | Enforced device, memory and IPC authority | Proof that command completion means physical presentation |

Intel, virtio and future adapters implement applicable common contracts and
report features explicitly. Firmware framebuffer supports fixed-mode copy
presentation only, not render engines or vblank fences. Hardware-specific pipes,
power domains and command formats stay in drivers and the relevant Mesa backend,
not the desktop surface protocol.

## Identity and ownership

Distinguish render adapter, allocation backing adapter, presentation output and
persistent monitor preference. Rendering and presentation may use different
adapters. EDID identity is descriptive input for preferences, never authority
or a live service address.

A live output binding names an authenticated driver endpoint incarnation and
driver-local output generation. Sessions retain that binding; driver replacement
or local output-number reuse cannot retarget an old session. Registry numbers
are selectors, not capabilities. Retired endpoint generations cannot be rebound
to outstanding completion tokens.

There is one active scanout-control owner per output. Shared pipes, PLLs and
device-wide constraints are arbitrated by that adapter's driver, validating the
complete affected configuration. Desktop chooses the main display independently
of boot-console ownership and preferred rendering GPU.

Firmware takeover has prepare, commit and retirement obligations. Before commit,
retain the working firmware path while validating the new resources/configuration.
Commit requires excluding the old writer, not merely setting a registry flag.
After takeover, never silently fall back to an obsolete firmware address.
Ambiguous takeover preserves uncertain resource ownership and reports failure.
Cross-adapter changes are not atomic merely because each adapter supports local
atomic changes.

## Image sharing without hidden pixel copies

### Teapot native test image (2026-10-02)

`kernel/cubit_live_mesa_teapot_v28.img` contains a native ANV teapot probe:
9,168 triangles in a vertex buffer, lighting shaders, a D32 depth attachment,
and a 256x256 BGRA readback presented through the existing retained CPU-grant
consumer. Three cycles use the same device. This is not the Desktop Vulkan
backend, cross-process GPU import or zero-copy scanout.

Native compilation/linking, 64/256-pixel consumer lifetime regressions, and the
ordinary native triangle relink pass. Linux lavapipe executes eight cycles with
14,147 foreground pixels, zero invalid pixels, and zero Vulkan validation
warnings/errors; visual inspection confirms an upright teapot. These hosted
checks do not establish Intel hardware execution. QEMU UEFI/4-CPU boot, log
delivery and USB input without PS/2 pass for v28. **NUC rendering is pending.**

The image SHA256 is
`60f591f3ec57e9a862c6494f31e3b7fbd0e0f33fe17aba719470cedba9d66abb`.
An additional hosted-only depth negative control intercepts pipeline creation
to disable depth testing/writes while preserving geometry, shaders and camera.
It changes 9,944 foreground pixels without changing the cleared background;
an identical-frame control is rejected by the comparison. Both eight-cycle
runs remain Vulkan-validation clean. This demonstrates that depth affects
visibility in the fixture, not a full geometric correctness proof or NUC
depth validation. Evidence: `tests/grant-storage/build/teapot-depth-control-r2.log`.
The FreeGLUT geometry/license is included at `licenses/mesa/FreeGLUT-teapot.h`
and was extracted and byte-compared against the source. The existing v26 image
and staged diagnostic app were preserved. Evidence: the `image-v28.log`,
`teapot-native-link.log`, `teapot-render-native-source.log` and
`teapot-license-triangle-regression.log` files in `tests/grant-storage/build/`.

### Native compositor submission smoke (2026-10-02)

The subsequent `--scene-link-check SNAPSHOT` gate in the native ANV linker
verifies the compositor owner's 801-input private snapshot and matching runtime,
compiles all six production C adapters, generates validated affine shaders and
retains/checks all nine Ada bridge/elaboration entry points in the linked app.
It passes with snapshot `tests/compositor/build/native-scene-_g2p_mss`; artifact
`tests/mesa-anv/target/native-instance-link.wdmdl9_c/mesa-scene-link-check.app`.
Run this gate inside `nix develop -c nix-shell
tests/mesa-anv/triangle-host-shell.nix --run 'python3 ...'` to provide both the
native toolchain and shader tools. Evidence: `tests/grant-storage/build/scene-native-link-r2.log`
and the artifact directory's `inputs.json`. This is deliberately a **link-only**
gate: it does not call Ada elaboration or scene operations, is not a display
image, and must not be presented as native compositor rendering.

`tests/mesa-anv/test-native-instance-link.py --compositor-smoke` (with the
existing triangle/logical-device options) builds an opt-in native probe using
the **unchanged production** `vulkan_submission_native.c` boundary. It begins
and ends the render pass, draws a green 8x8 rectangle over the existing Mesa
triangle, seals/submits through that boundary, and checks its completion poll
after the fence wait. The image-to-buffer barrier and readback remain explicit.
The command pool permits individual resets, as required by this backend.

This tests the C submission boundary, not the compositor's Ada scene policy,
cross-process image imports, zero-copy scanout or an enabled Desktop Vulkan
backend. It uses one app-owned target. The optional Desktop consumer still uses
the existing completed-linear-buffer presentation path and retains all resources
until its grants retire.

The Linux lavapipe oracle (`test-triangle-host.sh --compositor-smoke`) passed
eight cycles with every pixel checked, exactly 64 green pixels per cycle,
alternating consumer rejection/success and no synchronization-validation errors
or warnings. Its invalid-Vulkan negative control failed as expected. The ordinary
triangle regression also passed. Logs are under `tests/grant-storage/build/`:
`compositor-smoke-host.log` and `compositor-smoke-triangle-regression.log`.
Native compilation/linking passed (`compositor-smoke-native.log`), producing
`tests/mesa-anv/target/native-instance-link.olc3vo5x/mesa-triangle-window-repeat.app`.
**The native probe has not been executed on the NUC or packaged in v26.**

### Native import dependency audit (2026-10-02)

The current native path cannot obtain a compositor render target by turning on
writable presentation grants. `Intel_GPU_Buffer_Views.Share` deliberately
rejects `Presentation` with `Writable`, and `Native_GPU_Presentation.Forward`
creates a terminal read-only child. Preserve these rules. The ANV
`external-memory-policy.patch` also disables native FD/dma-buf/host-pointer
external-memory support and rejects external image/buffer format requests;
there is no supported Vulkan import hidden behind the CPU grant API.

### Gen12 GPU access restriction

The pinned [Linux v6.16 i915 PPGTT implementation](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/gen8_ppgtt.c#L1017-L1025)
disables read-only PPGTT support for graphics versions 11 and 12, citing
HSDES 1807136187. The source was read directly during the import audit; this is
an upstream implementation restriction, not an independently reproduced NUC
fault or a substitute for Intel's hardware specification. Our ADLN encoder
already rejects `Read_Only` instead of upgrading it to writable.

Scattered `Extent_View` binding now accepts an explicit `Page_Access` and
passes it unchanged to the VM builder. Read-only rejection leaves the offline
image unchanged, including earlier owner mappings. Ordinary owner bindings
retain their existing read/write default. Hosted tests check a 32 MiB request,
a slice crossing nonadjacent backing blocks and all 8192 existing PTEs;
contiguous-buffer regressions and native Intel driver compilation also pass.
Evidence: `tests/grant-storage/build/import-access.log`. No live GPU permission
fault was exercised and no import operation is enabled by this change.

Consequently the future import negotiation must not promise hardware-enforced
GPU read-only access on this backend. CPU read-only grants do not fix that.
An explicitly authorized, synchronized GPU read/write handoff is a different
contract; it must establish producer quiescence and control every remaining
writer before reuse. Otherwise use an explicitly reported copy into private
compositor backing or reject the unsupported import. Do not change the user's
permission request silently. The compositor's trusted rendering intent alone
is not a hardware memory-protection boundary.

The deeper dependency is allocation lifetime. `Intel_GPU_Buffer_Handles`
currently stores backing with each session-local name, rejects overlapping
registrations, and releases a closed backing reservation only on a trusted
retirement acknowledgment. Registering the same backing under a second session
would violate those assumptions. Do not remove the overlap checks to implement
import. Introduce an independently retained allocation identity before allowing
multiple names: exporter closure retires its name, not an importer's backing.
The allocation must retain all imports, pending publications, GPU VM references,
CPU loans and scanout readers until their own retirement is established.

The first internal retention primitive now exists in `Intel_GPU_Buffer_Handles`:
noncopyable `Retained_Reference` tokens keep backing reserved after its owner's
name closes. Both release and replacement reject outstanding references. A
trusted coordinator must return each reference exactly once, after its users
retire; wrong-registry and duplicate returns do not decrement another pin.
Tokens are bound to a stable registry root and exact session/name, not a wire
identifier. The registry must outlive every token; this is not a capability
that may be serialized, copied into client memory or kept across registry
reinitialization. No writable mapping or submission right follows from a pin.

`Retain_Referenced_Backing` lets a trusted serialized coordinator create an
independent lifetime pin from a validated existing token, even after the owner
name closes. It cannot reopen a name or authenticate an importer. A stale or
foreign source, active destination, exhausted counter or quarantined registry
fails without changing references. The source can then retire without ending
the destination's retention. This is the internal mechanism for separating an
admitted export/import/work lifetime, not the cross-session import API itself.

Hosted production-registry tests cover closed-owner splits, chained independent
users, stale/foreign/active rejection, incomplete retirement, replacement and
quarantine. The native view fixture additionally retains this independent pin
through two real CPU-grant retirements: backing release is denied until the
independent pin is returned. Evidence: `tests/grant-storage/build/import-lifetime.log`
and `import-lifetime-native.log`, native serial
`tests/intel-gpu/demand-backing.2Gmb0v/serial.log`. The Intel driver compiles and
links. No GPU work runs in this fixture; the address-bound registry operations
remain outside SPARK proof. This change is after the immutable v26 image.

Retained tokens now also cache their stable internal record index. Lookup and
return check its range, registry root, exact session/name and retained state
before accessing backing, avoiding a registry scan for every retained reader.
This does not turn the index into a public handle or relax lifetime validation.
Hosted handle and production CPU-export tests pass, including a pin across eight
metadata-growth boundaries, wrong-root and duplicate returns, closure and
quarantine; the native Intel driver compiles and links. Ordinary handle lookup
still scans, and cross-session GPU import remains unimplemented.

Hosted registry regressions pass two-reference closure/replacement, wrong
registry, duplicate return, quarantine and eight metadata-growth boundaries;
the native driver compiles and links with the gate. The address-binding and
imported-storage operations are not SPARK-proved, and this change has not been
executed on NUC. There is still no cross-session import/name creation or exported
target transport. Those must use the retention gate rather than bypassing the
existing overlap checks. This change is after the immutable v20 candidate.

CPU exports now consume these references in production: `Buffer_Views.Share`
pins the BO before grant creation; application mapping retirement, failed reply
delivery and session teardown pass the original registry through the sharing
coordinator. A view becomes retired/recyclable only after the kernel confirms
the grant is gone AND the backing pin is returned. Closing the BO name cannot
release or replace that backing while a reader remains. Ambiguous creation,
revocation, wrong-root return or registry quarantine conservatively retains the
pin. Known rejection before any kernel grant-creation call (bad range, forbidden
write/forward combination or mismatched endpoint) returns the unused pin and
makes the unpublished view recyclable. This is not an inference from a failed
kernel result: once creation is attempted, uncertainty retains backing.
`Buffer_Requests.Can_Retire` also checks registry pins before the coordinator
asks the supervisor to recycle a slice. The same gate is checked again at
acknowledgement; checking only after supervisor retirement would be too late.
This remains a serialized preflight, not an independent GPU/TLB completion
proof or a reservation that survives concurrent mutation.
The completed-triangle probe still uses externally retained backing and
does not acquire an application-registry pin.

The native `run-demand.sh views` fixture now exercises production allocation,
handle and view code against actual kernel self-grants. Three cycles verify
read-only write denial, shared sentinel contents, two pins after owner-name
closure, delayed retirement while an acquisition remains, release of the first
pin only after return, retention by the second grant, final release and stale
grant rejection. Evidence: `tests/intel-gpu/demand-backing.kZQEfo/serial.log`
and adjacent input hashes. This privileged disposable-VM test publishes no GPU
PTEs and proves neither cross-process isolation nor GPU completion; it does not
modify production staging or the v23 image.
The extended native fixture also derives a terminal read-only child. It rejects
write escalation and further forwarding, confirms that root revocation closes
new child acquisitions, and verifies that returning the parent acquisition does
not retire the root or release its BO pin while the child reader remains. The
child still reads the original sentinel until return; only then can root
retirement complete. All three cycles pass in
`tests/intel-gpu/demand-backing.Uq6uOa/` (serial log and input hashes). This still
uses self-endpoints, not independently scheduled application/Desktop processes.

`tests/intel-gpu/view_retention.gpr` exercises production view/registry code with
controlled grant outcomes: delayed retirement, two readers, name closure,
creation/revoke failures, wrong root, quarantine, repeat polling, slot reuse and
the probe path pass. Six pre-creation rejection cases additionally verify zero
grant calls, released backing and recyclable bookkeeping. The native Intel
driver compiles/links with all dispatcher
hooks connected. These are hosted fault tests plus native compilation, not
kernel-grant execution or a new NUC result; v20 predates this integration.

The three-target/front/pending design in
[the compositor handoff](compositor-shared-targets.md) is the intended model.
One negotiation detail needs revision before implementing its wire ABI:
**CPU mapping is optional, not a prerequisite for every GPU target.** A
device-local target may support authorized GPU writes and scanout without a
writable CPU grant. Report CPU mapping, renderer import and presentation
capabilities separately. A CPU compositor then chooses an explicitly supported
mapping or the existing copied fallback; it must not acquire extra GPU rights
merely because that fallback is selected. This matters for discrete VRAM as well
as multi-adapter machines. No new import operation or wire identifier is
implemented or advertised by this decision.

An image descriptor needs checked dimensions, per-plane offsets/strides/spans,
pixel format, storage layout, color interpretation, backing identity and rights.
A virtual address is not a transferable image reference. Imported handles are
bound to authenticated exporters and lifetimes; serialized device IDs or fence
numbers alone are insufficient.

For each source/destination pair negotiate an explicit path:

1. Direct scanout of a compatible authorized image when desktop policy permits.
2. Same-adapter composition into an acquired presentation buffer.
3. Cross-adapter import with compatible layout, memory access and synchronization.
4. Explicit transfer into a destination-compatible allocation when import is
   impossible. Account for transferred bytes/time, including staging/readback.

Import support is pair-specific, not a global zero-copy Boolean. CPU mappability,
coherency, renderability and scanout compatibility are separate properties. Never
expose device addresses to compensate for unsupported imports. Reuse grants for
shareable RAM; finish derived-loan tracking before forwarding borrowed driver
allocations through display.

Render completion, source-reader release, scanout-buffer release and physical
presentation are distinct events. Render completion does not make a displayed
buffer writable. Dependency waits must not block service dispatch. Client death
or timeout stops new admission but does not prove DMA/scanout quiescence. Retain
or quarantine uncertain backing; resetting one adapter cannot release another
adapter's outstanding import.

Public render contexts require GPU virtual-memory isolation, command privilege
restrictions, bounded accounting and verified reset behavior. An IOMMU alone
does not separate contexts inside a GPU. Trusted test submissions are a bring-up
stage, not permission to accept arbitrary app shaders.

### N100 application-command security gate (2026-09-30)

Intel TGL PRM Vol 2a, revision 12.21, `MI_BATCH_BUFFER_START`, printed
pages 971-972 (PDF pages 989-990), defines DW0 bit 8 as the address-space
indicator: PPGTT batches execute non-privileged; chained/nested batches from
PPGTT remain in PPGTT by hardware enforcement. PPGTT must be enabled in the
context. The existing `Intel_GPU_ADLN_Batch_Start.Build_At` emits this selector;
its postcondition now fixes the entire branch header for every accepted VA.
This is an encoder property, not proof of the hardware or the whole driver.

[Linux's i915 documentation](https://docs.kernel.org/gpu/i915.html#batchbuffer-parsing)
separates privileged commands, register access and privileged-memory access.
Its optional software parser can submit validated batches as secure on hardware
that needs that mechanism. This is NOT a reason to make CuBit application
batches privileged. The same document's workarounds section describes hardware
register whitelisting; inherited whitelist state must not silently become app
authority.

Before opening application admission, audit and establish:

- PPGTT-enabled context state and non-privileged batch entry on the supported
  device; no app control of ring, saved context, page tables or GuC messages.
- Explicit register whitelist configuration/readback and device-specific
  command restrictions, including GGTT memory operations and nested batches.
- Completion storage inaccessible to the app, so writing a mapped user buffer
  cannot forge the driver's retirement/fence evidence.
- Retention of all reachable mappings until genuine completion/quiescence;
  hostile loops, faults and client death need bounded recovery, not early reuse.
- If software validation is introduced, validation and execution must observe
  the same immutable command bytes, including indirect batches. Do not validate
  a writable buffer then execute it with greater privilege.

These are pending admission gates, not implemented guarantees. Current native
application admission remains closed; trusted bootstrap results do not satisfy
them. Mesa continues to generate shaders and rendering commands.

`Intel_GPU_Nonpriv_Registers` now models every FORCE_TO_NONPRIV field using
Ada representation clauses, based on TGL Vol2c-12.21 printed pp988-989
(PDF pp1014-1015). The pure evaluator handles read/write selection, all four
address-range sizes (including ignored low address bits), and deny precedence.
Reserved fields/access encodings and unsupported VF mode invalidate the whole
snapshot. `Unspecified` deliberately does not mean allowed: this evaluator
does not model the hardware's built-in non-privileged register table.

Cross-check: Linux `intel_engine_regs.h` FORCE_TO_NONPRIV definitions agree on
the address/access/range/deny bit positions. The
[inspected Linux source](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_engine_regs.h.html)
is revision v6.19-rc8-185-g2687c848e578, not the driver's pinned v6.16 reference.
It limits its list to 12 entries, while the PRM enumerates additional slots;
do not blindly expand MMIO access or claim a complete hardware snapshot from
that discrepancy. Model-specific slot availability still needs resolution.

Hosted `nonpriv_register_tests` passes bit roundtrips, range boundaries,
access combinations, deny ordering and invalid encodings. Focused SPARK checks
prove runtime safety/termination, not semantic equivalence to the GPU. No
native whitelist writes or application-admission changes have been made.

## Current code audit and change order

Display currently uses `CAP_SLOT_GPU = 9` for synchronous calls, acquisition and
asynchronous frame submissions. Virtio registers one `DRIVER_GPU`. The display
wire output selector is service-local (0..15), not persistent identity or
authority. Multiple heads behind that endpoint are supported; independent driver
instance routing is not established.

Display now captures its own process incarnation from its self-process
capability for the output-registry lifetime. Firmware outputs use that same
incarnation as their backend identity; virtio outputs capture and validate the
GPU endpoint incarnation. Failed capture prevents output registration. These
identities prevent the former constant-1 lifetime alias, but are correlation
metadata, not image-import authority or an implemented multi-adapter broker.
The endpoint slot must remain bound to that admitted backend; endpoint
replacement and cross-service output/image admission still need explicit
lifetime handling.

`Map_Backbuffer` remains unavailable. Compositor paint/transfer storage and
firmware/virtio presentation include copies. Existing session/frame validation
and retained grants must survive optimization. Native Intel rendering through
the CuBit Mesa path has hardware test evidence; its gallery currently uses
completed readback and CPU copies into Desktop frames. It is not yet a native
Intel presentation/scanout backend or a GPU-composited Desktop.

Implementation order:

1. Continue Intel power-safe register snapshot and ownership inventory, retaining
   firmware scanout. A successful mapping does not register a ready output.
2. Replace singleton routing with per-adapter endpoint bindings. Use the same
   binding for acquisition, submission and completion validation. Distinguish
   driver-local output numbers from broker registry numbers.
3. Implement exclusive native handoff and backend-owned presentation buffers;
   integrate derived loans before exposing direct CPU compositor access.
4. Add trusted offscreen Intel execution, GPU memory and dependencies; connect
   completed images to the existing presentation lifecycle.
5. Complete untrusted-context isolation and Mesa OS/WSI integration. Introduce
   cross-adapter imports with tested compatibility and lifetime contracts.

Acceptance tests must include two drivers with identical local output numbers,
one driver's death/restart while the other presents, stale completions, failed
imports, delayed readers, revocation during rendering and reset while another
adapter holds an image. Measure copies/bytes separately from IPC latency, render
completion and presentation timing. QEMU can exercise routing/lifetime failures,
not establish Intel MMIO or physical scanout correctness.

## Desktop import audit (2026-09-30)

The current implementation cannot safely present an Intel-owned BO merely by
passing its grant reference through the application's existing attachment call:

- `userspace/services/desktop/main.adb`, `OP_SURFACE_ATTACH_BUFFER`, acquires
  the grant with the authenticated application sender as its expected owner.
  Intel BO backing is driver-owned. Forwarding its numeric reference does not
  make the application its owner, and accepting a claimed owner PID would
  weaken this boundary. Preserve the existing check.
- Successful attachment immediately schedules a redraw, and composition reads
  the attached memory directly. An attachment must therefore already be safe
  to read; a later render-completion message is too late for this operation.
- `OP_SURFACE_PRESENT` validates ownership/damage and schedules redraw work.
  Its synchronous success reply does not say that composition has finished
  reading the source. It must not be used as a GPU-buffer reuse fence.
- The existing attachment is fixed linear BGRA8888 geometry. It has no storage
  modifier, render-completion dependency or per-frame reader-release token.
  Importing a tiled/compressed ANV image under that descriptor is invalid even
  if the byte span fits. CPU mappability does not imply linear image layout.

The next presentation implementation must resolve both authority and lifetime:
an authenticated driver-to-Desktop export associated with the application's
surface, or an attenuated derived loan whose original owner stays pinned; then
an explicit ready/read/release lifecycle. The display service's downstream
frame completion is not automatically an application source-reader release.
Existing CPU attachment/present semantics must remain distinct from this new
GPU image path. No import endpoint, owner-PID override or zero-copy guarantee
was added by this audit.

For an initial visible hardware probe, a deliberate copy of completed linear
pixels into an application-owned surface is a valid diagnostic option, provided
it is reported as readback/copy presentation, not the final zero-copy WSI path.
It does not replace the import/lifetime work needed for normal Mesa rendering.

### Explicit presentation mapping (2026-09-30)

Intel application-BO mapping label `0A23` now has operation 3:
`Map_Presentation`. It uses the same authenticated session, BO ownership,
endpoint-incarnation and whole-page bounds checks as ordinary mappings, but
creates an owner-opted-in, read-only forwarding root. Operations 0/1 remain
nonforwardable; operation 2 retires a mapping. Writable presentation requests
are rejected. Internal firmware/context/page-table allocations are not exposed
through this application-BO registry.

The Mesa C/Ada bridge exposes `cubit_intel_map_presentation` separately from
`cubit_intel_map_buffer`; passing 3 as the latter's writable flag is rejected.
An app must acquire the root and derive a read-only terminal child to an
authorized recipient before attaching it. The child is logically owned by the
app, preserving Desktop's sender/owner check, while the root retains backing.
Forwarding is not restricted to Desktop by the kernel: the app must possess a
recipient endpoint capability. No internal driver memory becomes forwardable.

Hosted tests cover opt-in, read-only enforcement, default nonforwardability,
authentication and wire validation. Native driver/bridge compilation is a
separate check. This does not yet connect Mesa WSI to Desktop, advertise a
hardware Vulkan device, authorize application submissions or establish GPU
completion. The ready/read/release and linear-layout requirements above remain.

`Native_GPU_Presentation` now provides a C-callable connector for that CPU-visible
linear path: forward an acquired root to the Desktop endpoint, attach the child,
then revoke/poll its retirement after replacement or destruction. It accepts no
owner PID override, does not copy pixels, and preserves caller ownership of the
child even on a rejected or uncertain attach. A malformed reply is distinguished
from a valid Desktop rejection. Attachment/present success does not retire the
child or allow reuse. The existing compositor still copies into its own output;
this is not end-to-end zero-copy scanout.

This connector is not yet wired into Mesa WSI. The `grant-forward-desktop`
native fixture uses a synthetic RAM owner with real Desktop/display services;
it cannot validate Intel output formats or GPU synchronization.

### Native application submission plumbing (2026-09-30)

The Intel service now dispatches label `0A27` to a synchronous, per-session
submission coordinator. Request words are `[1 | (BO byte offset << 32), BO
handle, raw48 GPU batch address, byte length]`; the four-word reply is
`[status, 1, completed sequence, 0]`. Completion is zero on failure. This offset
is in **bytes**, unlike the page offset in offline binding label `0A24`.

Only an authenticated session can select its retained context. The native
callbacks resolve the entire batch slice against that session's sealed VM and
BO registry, arm a fresh protected marker observation, enable scheduling,
append the driver-constructed nonprivileged PPGTT branch, notify GuC, wait for
the marker, and wait for scheduling-disable acknowledgement. Each session has
its own retained ring channel and monotonically increasing completion state.
Initialization requires the driver's observed setup marker and disable
acknowledgement, not a client assertion. Failure after execution starts closes
the session and retains backing; a lost successful reply also retires the
session rather than replaying work. Ring capacity is finite; wrap/reuse is not
implemented.

**Application admission remains closed (`Ready=False`).** This wiring is not
an enabled Mesa backend, a command-parser/security proof, or hardware validation
of application submission. BO extent validation does not constrain what a GPU
batch can subsequently fetch or do. The command-privilege, protected-memory,
hostile-work recovery, live-VM update and resource-lifetime requirements above
must still be met before enabling application execution. The current NUC image
is unchanged; native compilation and component tests are separate evidence
from actual Intel execution.

The C/Ada buffer bridge exposes `cubit_intel_submit_batch` for this protocol.
The caller supplies its previous completion (initially 1 after setup); only an
exact successor in a canonical successful reply is accepted. The previous
value is a local reply check, not a client-controlled server sequence. Output
clears on every failure, and the call never retries. Status 4 denotes local or
transport/protocol failure; once dispatched it may mean work executed, so the
caller must retire the session rather than replay. Hosted wire/C-ABI tests
cover malformed replies, stale/overflowing sequences, extent rejection and
single-call behavior. Native-runtime compilation is checked separately; no
Mesa queue/backend selection or hardware execution follows from these tests.
