# GPU requirements for the device-manager lifecycle

2026-09-28 design input for the joint device-manager plan. This describes
requirements, not implemented recovery or a proof of DMA isolation. The Intel
bring-up currently retains allocations and grants on uncertain outcomes;
automatic driver restart is not yet safe.

## Ownership and identity

### CPU virtual-address aperture (proposed, not enforced)

Reserve `[0x700000000000, 0x780000000000)` (8 TiB) per driver process for
device mappings. This is a CPU address-space convention, not a physical RAM
reservation, GPU address range, or authority grant. Separate processes can
use identical addresses; a process owning several devices needs disjoint
suballocations keyed by device-binding generation. Do not allocate backing for
the entire aperture at startup.

Source inventory as of 2026-09-30: received grants begin at `0x400000000000`
and currently span 64 GiB (`Memory_Grants`); owned anonymous memory occupies
`[0x580000000000, 0x590000000000)` (`Owned_Memory_Layout`); user stacks descend
from `0x800000000000` (`Process`). The Intel bootstrap arena already begins at
`0x700000000000`, but register/firmware mappings still use legacy low addresses
such as `0x60000000` and `0x61000000`. This inventory is not proof that arbitrary
ELF segments or legacy mapping syscalls cannot occupy the proposed aperture.

Before adopting it, centralize the layout in the common runtime/kernel contract,
reject overlapping ELF/ordinary-memory/stack mappings, and route authorized
device mapping requests through an address-space reservation ledger. Partition
MMIO, ordinary DMA RAM and CPU-visible framebuffer mappings with guards and
explicit cache attributes; avoid conflicting cache aliases. Migration must
update supervisor and driver together, rather than changing a driver constant
alone. Reservation must not confer device access or replace capability checks.

CPU VA, GPU GGTT/PPGTT VA and IOMMU DMA addresses remain independent. The
current physical-address-equals-DMA assumption must be explicit and replaced
by device-domain mappings when IOMMU translation is enabled.

### Scattered backing and large pages

Current grant status (supersedes the staged rejection notes below): root-grant
resolution now accepts 2 MiB owner leaves and pins individual 4 KiB frames.
Recipient mappings, derived resolution and retirement remain 4 KiB-only.
The main kernel compiles with this change. Native private-kernel runs cover
CPU data access, live-owner revoke/regrant and owner exit with a loan held.
A sacrificial read-only alias read succeeded and its write produced a user
write-protection fault. The corrected fault-specific runner passed in
`cubit-usb-live.hrg2n78h`; earlier runners waited for unrelated desktop/full
lifecycle milestones and did not pass. The fault run does not claim completion
of the separate full-lifecycle test.

The live 32 MiB bootstrap arena is now backed by sixteen independently allocated,
aligned order-9 (2 MiB) blocks, using retained DMA mode3 for CPU large-page
mappings. CPU and GPU virtual ranges can remain contiguous
even when the physical blocks are not. `Intel_GPU_Physical_Extents` now checks
such an extent list and resolves only the contiguous prefix of each request;
hosted tests cover all 8192 constituent pages and cross-block boundaries.
Allocation IPC now returns the CPU slice and arena identity, followed by sixteen
indexed extent replies on first use. The driver admits the complete map before
zeroing or publishing any buffer; later allocations reuse that retained map.
Native context construction and GGTT/PPGTT publication resolve actual backing
pages. Native compilation and hosted transport/mapper regressions pass; this
integration has not yet been validated on the NUC or formally proved.

Reply validation, slicing, backing identity/lifetime and mapping use bounded
allocation views. Partial allocation failures must retain/quarantine
uncertain backing, never expose a partially valid arena. Large-page selection
is separate: CPU and GPU support, alignment, permissions and cache attributes
must all permit it. Larger pages reduce TLB pressure, not the requirement to
invalidate changed translations. Stable mappings avoid repeated invalidations.

CPU large-page audit (2026-09-30): the kernel already uses 2 MiB leaves in
its physical direct map. `mapBigPage` now rejects misalignment and occupied
user slots, but preserves the kernel PCI direct-map remapping path. The
`tableWalk` large-leaf option is explicit; grant callers remain 4 KiB-only.
Process page-admission and fault collision checks opt in, so they recognize
an existing 2 MiB mapping instead of attempting to allocate a 4 KiB page over it.
Page-table deletion already skips P1 traversal under 2 MiB leaves; DMA backing
is accounted for separately and released at its original buddy order (or
retained until reboot). The native walker fixture now verifies that deletion
of mixed present/retired 2 MiB leaves and a 4 KiB leaf invokes release callbacks
only for the three page-table frames, never leaf backing. It also verifies
default-walker rejection at all 512 subpage offsets. This is not a process-exit
or grant-syscall regression test.
`unmapPage` still removes the entire 2 MiB leaf for an interior address, so it
must not be used as a subpage revocation operation. Do not enable driver large
pages in the Intel supervisor until grant rejection/splitting and teardown
policy are wired explicitly.

`ALLOC_DMA` now has explicit mode3: retained, order9 only, 2 MiB-aligned CPU
address, one 2 MiB CPU leaf. Modes0/1 keep 4 KiB mappings. Mode3 uses the same
authority and retained quota checks; current grant resolution rejects its
large leaves. The native DMA retention fixture allocates sixteen such blocks
to its second owner and verifies exit/retention/quota behavior. That child also
writes then separately verifies all 8192 constituent 4 KiB offsets and attempts
512 actual subpage grant syscalls, all rejected (native QEMU run
`cubit-usb-live.zowru7oa`). This is CPU mapping evidence, not GPU DMA evidence.
The Intel supervisor has not switched to this mode yet.

Migration constraint: driver-private mode3 is not sufficient for app-visible
buffers. Do not switch the whole live arena while CPU grants remain rejected.
The source audit identifies a path that does not require splitting the owner's
2 MiB CPU leaf: root grant resolution can resolve the containing 4 KiB frame,
then use the existing ownership-checked per-frame pin and install 4 KiB leaves
in the receiver. Derived grants already resolve the receiver's 4 KiB mappings;
grant retirement must continue resolving/unmapping only those recipient leaves,
shooting down before releasing pins. This is a proposed extension, not enabled
behavior. Its native gate must cover interior/end offsets, owner exit with an
active loan, read-only recipient access, and return/revocation without disturbing
the owner's other subpages. Preserve rejection until that gate is implemented.
An isolated four-CPU QEMU kernel with root-grant resolution opted in passed
the first lifetime gate (`cubit-usb-live.npfh_jcq`): the last 4 KiB of the first
2 MiB leaf was borrowed read-only, its sentinel verified, owner killed while
the acquisition remained held, and the acquisition returned afterward. Main
kernel behavior is unchanged pending read-only fault and live-owner revocation
coverage. Derived resolution and recipient retirement were not changed.
Live-owner revocation/regrant also passed (`cubit-usb-live.42nzc743`): after
the borrower returned its acquisition, the owner confirmed retirement, checked
all 8192 offsets were intact, and regranted the same subpage. The second loan
was verified before the existing owner-exit test. The read-only write-fault
gate remains before promotion.

The live four-word buffer reply also encodes `DMA = Arena_DMA + CPU_offset`.
It cannot represent scattered backing. Migrating it requires authenticated
extent acquisition bound to the same device/process incarnation, whole-map
validation before publication, and extent-aware slicing at every consumer.
Never use the first extent's physical address as the base of a larger buffer.

`tests/big-pages/native_walk_check.adb` passed in a private four-CPU QEMU image:
the walker checked all 512 constituent frame offsets, PAT/reserved bits,
nonpresent entries and occupied slots; CPU reads/writes through an installed
private root verified backing replacement after local CR3 invalidation. The
test runs before AP startup: four configured CPUs is NOT evidence of a
cross-core shootdown test. It allocates retained test frames, not GPU buffers.

The current `TLB_Shootdown.Register_CPU` rejects CR4.PCIDE; `Invalidate_All`
requests all online CPUs, each reloads CR3 before acknowledging, and timeout
is fatal before pin release/reuse. This covers nonglobal mappings only.
`tests/big-pages/big_page_smp_check.adb` subsequently passed in four-CPU QEMU:
three APs primed the old mapping, the BSP replaced backing and waited for
production `Invalidate_All` acknowledgments, and each AP verified all 512
replacement offsets. APs called production `Service` cooperatively with
interrupts masked: this tests remote translation invalidation and acknowledgment,
not IPI interrupt dispatch. Owner exit and rejected subpage grants still need
native tests. PCID, global mappings, promotion/demotion and CPU hot-unplug are
not supported by this contract.

- A device instance needs a manager-issued identity and binding generation,
  not merely a PCI address or PID. BDF identifies a location; a replacement
  device or restarted driver must not inherit an old generation's rights.
- One driver binding owns register programming and its address-space ledger.
  Register-page grants, DMA backing, interrupt delivery and endpoint authority
  must all refer to that binding. Do not allow independently rebindable pieces
  to form a partially old, partially new driver.
- Keep device binding separate from display ownership. A GPU may render
  without driving a monitor; a monitor may remain on firmware scanout while
  its rendering engines are initialized. Desktop chooses the primary monitor;
  neither PCI enumeration order nor driver startup order does.
- Multiple GPUs require independent bindings and buffer identities. A buffer
  usable by one GPU is not automatically DMA-addressable by another. Explicit
  import/export admission must precede cross-device use.

## Readiness is staged, not one ready bit

Expose typed state and failure information for discovery, resource admission,
register access, display preservation, engine reset, firmware authentication,
command transport, render submission and presentation. The Desktop must not
wait for render readiness just to continue using the boot framebuffer.

Current examples that must remain distinct: firmware file loaded, CPU backing
prepared, GPU mapping published, firmware authenticated/running, and successful
submission completion. None implies the next stage. Publish the stage, binding
generation and reason through logstore/inspection endpoints.

## Failure and recovery

Stopping the driver process is not proof that the device stopped DMA. A lost
reply, timeout or failed first MMIO write is an uncertain hardware outcome.
Retain affected backing, GPU address claims and ownership until a trusted
recovery operation establishes that the hardware can no longer access them.

A future recovery transaction must coordinate:

1. Stop admitting new work for the old generation; notify clients.
2. Block interrupt delivery and account for handlers already in flight.
3. Quiesce or isolate DMA through mechanisms that actually cover this device.
4. Reset the relevant engines/device with an explicit scope and outcome.
5. Retire stale completions and translations before recycling resources.
6. Establish a new binding generation and reconstruct validated state.

The precise hardware ordering belongs in the driver/platform implementation;
this list is not a universal register recipe. Resetting rendering engines does
not necessarily reset display scanout. Preserve the active framebuffer and its
backing unless the recovery contract explicitly includes display teardown.

Do not give a replacement process the former driver's grants just because its
executable has the same name. Manager-held quarantine records must outlive the
failed process. Until that is implemented, refuse automatic GPU rebind and
report that recovery requires a reboot rather than freeing uncertain memory.

## Power boundary

A proposed power service owns policy and system-wide transition coordination;
the GPU driver owns internal forcewake, pipe wells and hardware sequences.
ACPI/firmware services own platform operations within delegated authority.
There must not be two concurrent writers controlling one power transition.

Power references and pending submissions constrain device idle. A system sleep
operation needs a serialized prepare/commit/resume protocol and a new epoch for
stale replies. Driver failure during preparation is not a successful suspend
acknowledgment. Keep the per-register and per-submission hot paths local; no
power-service round trip is required for every GPU access.

## Immediate extraction constraints

The current Intel devmgr protocol uses authenticated PID/badge checks, fixed
register-page grants, one-shot transitions and retained state. Preserve these
properties while replacing fixed slots with a general binding mechanism;
don't preserve the experimental wire ABI merely for compatibility.

Before extraction, tests should cover stale-generation replies, lost grant
replies, partial mapping, driver death with possible DMA, reset failure,
concurrent suspend/recovery, and a rendering-engine reset while firmware
scanout remains active. Hosted state-machine proofs must state their hardware
quiescence assumptions; QEMU startup tests do not validate physical Intel reset.
# Native Mesa discovery integration gap (2026-09-28)

The main Intel service now implements the bounded identity/topology responder
described in `intel-render-discovery.md`, but does not expose it through an
application discovery role. `DRIVER_GPU = 17` in
devmgr is assigned to virtio display registration and must not be silently
reused as an Intel rendering role. The topology translator alone cannot
connect native Mesa to this service.

## Application-session implementation boundary (2026-09-30)

Inspection of the current driver identifies three concrete diagnostic-only
constraints that a Mesa factory must not conceal:

- `Intel_GPU_Submission_Backing` partitions the retained firmware allocation
  at fixed offsets `0x8c000..0xa7000`; it is not an application BO allocator.
- `Intel_GPU_Initial_VM.Build` describes one 2 MiB GPU window with up to 512
  4 KiB leaves. It is not a general address-space service, live mapping
  implementation or client-authorized binding interface.
- The diagnostic now reserves a 256-fence block from a driver-owned pool.
  `Intel_GPU_GuC_Context_Session` still requires exclusive channel
  ownership and does not implement a multi-context dispatcher.

The lifecycle/session initializer now requires an explicit inclusive final
fence. Intervals shorter than the four controls are rejected; notification
exhaustion rejects without wrapping, and disabling remains available using
its reserved control fence. Backpressure permits retry only under the existing
transport guarantee that nothing was published. The main probe explicitly
uses a driver-owned monotonic ledger over 100..65535 and takes its first
256-fence block before publication. Its send callback checks that held range.
Lower fences remain reserved for boot requests, including the existing probe
at 42. The ledger never resets or releases a block during the CT lifetime;
failed and abandoned contexts retain their intervals too. Exhaustion is an
explicit failure, not permission to reuse a possibly outstanding fence.

The hosted ledger test exercises widths 4..300 to exhaustion, invalid/oversized
requests, exact adjacency and absence of duplicate fence issuance, including
an allocation ending at 65535. This is numeric allocation correctness, not
hardware message ordering or a proof of client authority.

Native validation: private driver build15574 compiles and links with the
bounded lifecycle/ledger and its actual-range send check. This fixes the
previous independent callback restriction to fences 100..104, which would
reject later allocated notifications. Both main and private source have the
fix; only the private driver has been native-built while the shared lock is
occupied. The published offscreen `.img` has not been rebuilt, and no new
hardware completion is claimed.

Packaging update: `kernel/cubit_live_fence_ranges.img` in the private
`intel-presence.F8KpDB` workspace now contains the driver built above.
Image SHA256 is `2aaf480af183be57cb7566180d4c4df854e43352c24014aaef51d93a3d5e6e4b`;
its plan records driver hash
`7780f7353d28acc37fe50e90b3c5a87b1336c95893133a655be34be03957b075`.
Firmware, Mesa and image-membership audits pass. QEMU94329 passes UEFI,
four-CPU USB-flash boot plus software Mesa's 194673 geometric pixels,
animation and close (`tmp/cubit-usb-live.5ynsai7v` in that workspace).
The first run passed Mesa but failed its final verbose-xHCI log expectation;
the successful rerun used the runner's existing `--quiet-xhci` option matching
this image. No assertions or image contents were changed between runs.

NUC evidence still needed: L3 notification/completion, draw target-clear and
completion, pixel-read center `FFFF0000` with matching corners, and final
scheduling disable COMPLETE. These pixels are offscreen; passing software
Mesa on QEMU does not prove this Intel path.

Historical implementation (superseded on 2026-10-03 by the diagnostic FAST-ID
stream described in `intel-gpu-context-registration.md`; the unused range and
route units and their dedicated tests/proof fixtures have been removed):

The main driver then retained context-ID/range ownership records in
`Intel_GPU_Context_Routes`. Registration rejects duplicate IDs, overlapping
ranges, ranges with fewer than four fences, zero fences and full tables.
There is no removal operation; failed contexts retain their routing identity.
The diagnostic records its allocation before publication and verifies the
registered owner in its send callback. The current main instance has capacity
16; that is an internal resource limit, not a claim of 16 runnable contexts.
This source change is not in the already published fence-range test image.

The focused SPARK run proves all 27 checks for the capacity-16 registry,
including lookup's matching-owner contract and accepted registration's endpoint
ownership contract. Stored IDs exclude the no-owner sentinel by subtype;
unused entries are excluded by the used-entry count. Hosted tests exercise
every 16-bit fence lookup, rejected overlaps, duplicate IDs and exhaustion.
These are registry properties, not a proof of the future receive dispatcher
or of hardware execution.

`Select_Destination` now decodes an owned CT payload and distinguishes malformed,
unclaimed and context-directed messages. Scheduling acknowledgments use their
explicit context ID, irrespective of fence; failures use the fence's retained
owner. Other messages are not routed merely because their fence falls in an
allocated range. Tests cover every 16-bit fence for failures, explicit-ID
scheduling and unrelated messages, plus unknown IDs and malformed payloads.
The selector's contract proves that a context destination is registered and
not the sentinel. Both service receive loops now call the routed handler, and
the synchronous scheduling waiter requires an explicit dispatch callback.
The main diagnostic still instantiates only context7: another registered
destination faults rather than being delivered to the wrong state machine.
Unclaimed messages remain retained. A hosted waiter regression injects an
acknowledgment handled for another context before this context's acknowledgment;
the waiter must continue until its own state changes. This exercises callback
integration, not two hardware contexts.

`Intel_GPU_Context_Table` now supplies a driver-internal retained session array
with monotonic IDs, disjoint allocated fence ranges and delivery to each actual
session state machine. Its hosted test interleaves two enabled sessions, checks
separate notification fences, injects a late failure into one while the other
continues, and checks transport-wide quarantine on malformed traffic, retention
failure and ownership loss. Failed initialization keeps its ID and fence range;
there is no reuse or recovery of a broken table in the same CT lifetime.
The table itself is regression-tested, not SPARK-proved; its underlying routing
and fence allocation have the narrower proofs described above.

The table is not yet wired into main or a public IPC endpoint. It assumes
serialized trusted callers supplying exclusive context backing and the same
transport ownership predicate used by its session implementation. It does not
allocate VM/BO backing or authenticate clients. Public multi-context operation
still needs those ownership records, main integration, resource lifetimes and
physical-hardware validation. The existing diagnostic remains context7.

The submission image builder now takes the context extent's DMA start rather
than the parent firmware allocation base. All page-table addresses are relative
to that extent; the admitted bound is its Byte_Count below 4GiB. The current
native adapter still uses its existing retained firmware slice, explicitly
adding Backing.First at that boundary. No new DMA allocation is claimed.
Main image/materialization/publication tests pass, including low-address and
upper-bound extent cases; the focused main image SPARK checks pass. The private
offscreen variant preserves its draw/state/shader contents and passes its
image/materialization/publication tests and native driver build. These changes
are not in the already published NUC image. Allocation and exclusive backing
ownership must still be connected to the retained session table.

Native initial/live ring helpers now require an explicit stable CPU extent
per generic instance. They reject zero, unaligned, short or overflowing/outside
lower-canonical-user-space extents before memory access; ownership and coherence
callbacks remain required. The diagnostic selects its existing address in main,
not inside the reusable helpers. Hosted fixtures use different addresses for
initial/live paths and verify exact writes, marker reads and rejected extents.
Private L3/pixel readers retain their checks and use the same extent. The
private native driver builds. These are mapping-boundary regressions, not
proof of per-client memory isolation or concurrent hardware execution.

Submission preparation now holds one limited Buffer_State per retained context
instead of process-global Attempted/GPU_Address state. Initialize accepts its
trusted CPU/DMA extent and capacity; a failed or successful attempt cannot be
repeated on that state. The hosted native-memory fixture prepares two disjoint
images, checks their exact contents and untouched space, and rejects attempts
to repurpose already-used state. Main/private callers still use the supervised
boot allocation; private native build passes. Dynamic backing allocation remains
unimplemented in devmgr's authority-owned allocation path (022C remains
the one-shot firmware grant). The networking agent's2026-09-28 coordination
note reserves Intel branches for this task, allowing scoped allocation work
without a broad devmgr handover. This refactor neither grants DMA authority nor
establishes isolation merely from numeric address checks.

## Consolidated context-backing image (2026-09-30)

Private image: `.build-workspaces/intel-presence.F8KpDB/kernel/cubit_live_context_backing.img`
with adjacent `.img.plan.json`. SHA256
`9046a023e89bc901eb4d627fcd9e470360781dac7e34a2106635516218b5cf19`.
Recorded driver hash matches the privately staged driver:
`e80dc0aa0bf090b10c42a68ecb801ee484b3691e75e5460dfcb3f1e144811b6b`.
This includes routed single-diagnostic receive/wait, context-relative images,
explicit ring extents and per-buffer initialization state. The session table
is still not wired into the native driver. Earlier test images are preserved.

Build and firmware/Mesa/membership audits pass. QEMU36108 passes UEFI four-CPU
USB-flash boot plus native CuBit software Mesa: 194673 geometric pixels,
animation resume/pause and close. Logs are in private
`tmp/cubit-usb-live.dlx6y8xh`. This QEMU GPU is not Intel and does not verify
GuC, L3 admission or the offscreen Intel drawing batch.

Next NUC evidence: `L3 ring` completion, `L3 allocation` fields-match and
admitted URB size, `draw target-clear`/completion, `draw pixels` center/match,
`draw corners`, and final scheduling disable. The draw remains offscreen;
the expected center is FFFF0000 with four zero corner samples.

## Supervisor context allocation

The main devmgr Intel branch now accepts request0235 with one word selecting
slot1..16, only from the frozen Intel inspection PID with authority tag4947
and zero unused words. Reset authorization, frozen configuration and GGTT
grant are required. Startup-time requests receive the existing retry reply.
The supervisor selects retained256KiB allocations below4GiB and CPU windows
68000000..68400000; requests cannot choose size, address or target process.
Success replyF000 carries physical/identity-DMA address, CPU address, capacity
and slot. This remains the existing identity-DMA model, not an IOMMU mapping.

Each slot attempts allocation once. Repeated successful requests return the
same backing; failure never reallocates, and ownership loss during allocation
poisons the pool. There is no free, reset, rebind or process-ID reuse contract;
the existing frozen driver lifetime is mandatory. Kernel retained-DMA quota
still applies. Hosted tests cover all slots, disjoint CPU windows, repeated
requests, invalid physical replies and ownership loss. Native devmgr builds
under the shared lock. These tests do not validate the new branch via live IPC.
The driver request adapter and session/backing integration remain to be wired;
this change is not in the published context_backing image.

Driver-side Context_Memory.Acquire now has a native-compiled bootstrap adapter:
one attempt per slot, fixed supervisor endpoint15, bounded30ms/30000-poll wait,
monotonic-clock validation and matching completion token/status. It uses the
logger-aware completion poller and assumes no other driver request is pending.
It does not initialize, GPU-map or publish the returned allocation. The pure
Context_Reply decoder checks the exact success/retry/denial envelopes and the
expected slot, CPU address, capacity and bounded physical address. Mutation
tests pass for all16 slots; classification and decoding contracts are SPARK
proved. The adapter is not yet invoked by main and has no live-IPC execution
evidence. Its transport assumptions are not established by those pure proofs.

The next session work must use these records to dispatch late replies
to the correct retained context. Numeric disjointness alone is not authority:
the driver must bind the context, VM and BO records to the authenticated
client and device lifetime, and retain uncertain resources after client exit.
Only a trusted allocator may select DMA backing and page-table pages.

The initial public implementation can reject unsupported creation or resource
limits, but must not share the diagnostic context/page tables between clients,
accept client-supplied DMA addresses, recycle uncertain fences, or mark a
metadata-only device render-ready to make Mesa enumeration succeed. The fixed
hardware probe remains useful as a separate diagnostic while these facilities
are built. This audit does not claim they are implemented.

Next integration needs a separate render-adapter query endpoint, minted by
devmgr and delegated under the application's manifest policy through procmgr.
Receiving the query endpoint must not confer register, DMA, firmware, reset,
scanout, or command-submission authority. Queries should copy a retained,
validated snapshot, not trigger caller-directed MMIO. Return unavailable for
missing evidence; never substitute offline PCI-table topology or advertise
render readiness merely because a snapshot exists. Capability-authorized
object creation/submission remains a separate interface and readiness gate.

The response must distinguish adapter identity, valid measured topology,
supported versus initialized engines, address-space limits, memory budgets,
timestamp frequency, and device-loss state. The current decoder supplies only
part of this. A new service role must be coordinated with shared runtime and
devmgr/procmgr changes before implementation; no role/slot numbers are reserved
by this note.
