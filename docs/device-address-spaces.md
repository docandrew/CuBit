# Device address spaces and bounded Intel GGTT takeover

## Four address domains, not one global device address

| Domain | Owner / use | Lifetime |
| --- | --- | --- |
| CPU virtual address | Per-process mapping of RAM or MMIO; CPU dereferences only | Process mapping plus retained resource |
| Physical address | Kernel-owned RAM or PCI BAR resource | Physical allocation / device resource |
| Device DMA address (IOVA with translation) | Address valid for a particular device and DMA domain | Device binding, domain generation and pinned backing |
| GPU virtual address | GGTT or per-context GPU page-table mapping to device-visible backing | GPU address-space allocation and completion lifetime |

CPU VA -> CPU page tables -> physical RAM is separate from GPU VA -> GPU
page tables -> device-visible DMA address -> optional platform IOMMU -> RAM.
The precise Intel integrated-graphics translation/bypass configuration must be
validated before enabling VT-d; this table is a contract, not a claim that
every GPU transaction traverses an enabled IOMMU today.

Current CuBit Intel allocation replies contain physical addresses from
SYSCALL_ALLOC_DMA, used as identity DMA addresses. Firmware CPU VA is
0x61000000; the writable GGTT MMIO alias is 0x64000000. Neither determines
the GPU address. These fixed CPU windows are per-process implementation
details, not a cross-driver ABI or a system-wide IOVA allocation scheme.
The current publisher retains its existing below-4GiB DMA-address bound.
Removing that bound requires a separate supported-address-width audit.

## Boot-only GGTT takeover

The native owner requires frozen device authority, a successful bounded GT
reset, retained power/DC state, admitted MMIO mapping, PAT/MOCS/engine setup
and a complete scanout inventory. No concurrent modesetting or submission
owner is admitted during this boot sequence. This serialization is a current
architectural assumption, not something the numeric allocator proves.

The numeric layout separates runtime, upload and final guard. Each partition
has a retained ledger; page exclusions protect scanout. The publisher searches
software claims/exclusions, reserves the complete selected interval, checks
MMIO readability, prepares coherent backing, rechecks the exclusion gate,
writes only the exact allocation's encoded DMA addresses, reads back and
invalidates translations. Nonzero inherited PTEs do not mean allocated.
All-ones/inaccessible MMIO still fails closed. No bulk table clearing occurs.
The final guard and retained scanout are not rewritten by this operation.

Every claim remains retained, including pre-write failures. Before the first
store the attempt becomes potentially published. Partial writes, readback
failure or invalidation failure quarantine both VA and backing. Old physical
pages are never freed merely because their GPU aliases were replaced.
Published means mapping installed, not firmware authenticated or executing.

Linux reference: v6.16 gt/intel_ggtt.c (ggtt_reserve_guc_top,
gen8_ggtt_insert_entries), gt/uc/intel_uc_fw.c (uc_fw_ggtt_offset,
uc_fw_bind_ggtt), display/intel_plane_initial.c (initial_plane_vma).
Linux uses allocator reservations, not PTE-zero discovery. Gen8's normal
clear_range is a no-op; scratch_range is a distinct operation.

## Required common DMA contract before IOMMU enablement

Replace identity assumptions at the allocation provider, not by reinterpreting
CPU pointers in GPU code. A bounded DMA resource must identify its device,
domain/binding generation, direction/rights, size, CPU mapping, and
device-visible extents. The kernel/broker retains physical pages and establishes
IOVA mappings before publication. Reject stale/wrong-device resources; do not
silently fall back to identity addressing on translated systems.

Teardown must stop submission, drain or reset the device, retire GPU mappings,
complete the relevant GPU and IOMMU invalidations, then unmap IOVA and release
RAM. Failure retains/quarantines resources. A capability revocation alone does
not retract DMA already issued. IOMMU isolation groups/requester aliases must
constrain which devices can share a domain. GPU VA allocation does not itself
provide DMA confinement.

Multiple GPUs may map one authorized backing object at different GPU VAs and
different IOVAs. Do not require matching addresses across devices/processes.
Keep descriptors on IPC and bulk buffers in shared memory. No global numeric
address supplied by an application is sufficient authority.

Follow-up: centralize validated per-process device CPU mapping windows and
collision checks, adopt the common resource descriptor in devmgr/kernel, and
replace the identity-DMA reply with domain-aware mapping handles. This change
does not modify those shared ABIs or claim an implemented IOMMU backend.

Implementation status: retained-ledger GGTT takeover is integrated in main;
it no longer treats inherited nonzero PTEs as allocation ownership. NUC feedback
has reached GuC FIRMWARE-READY and first engine marker completion; repeated
submission still awaits hardware verification of the arbitration-footer fix.

The GGTT ledger now uses shared `Intel_GPU_VA_Placement` numeric search. It
can search48bit GPU windows independently of backing allocation, while the
GGTT adapter retains its existing admission and alignment policy. Search is
not a reservation, resource handle, mapping operation or authority decision.

The offline initial PPGTT builder now accepts an explicit512-leaf DMA map
within one2MiB window; zero entries stay nonpresent. This replaces the dense
input interface. Table/data aliases and invalid nonzero DMA pages are rejected,
and the existing fixed probe retains exactly its batch/completion mappings.
The separate `Intel_GPU_VM_Image` now handles multiple raw48 windows and
atomic buffer insertion, with sealed-image materialization connected to native
diagnostic submission preparation. Dynamic page-table backing, live bind/unbind,
TLB invalidation ordering and completion-safe release remain outstanding. No
application may supply raw DMA addresses through these internal builders.

## CPU grants are not DMA leases (2026-09-30 code audit)

Existing `CuBit.Memory_Grants` can carry application-visible CPU buffer views;
it is not by itself the GPU-backing lifetime mechanism. In
`kernel/src/process-ipc.adb`, grant creation pins owned frames while holding
the source address-space lock. Acquisitions delay ordinary revocation. But
`revokeAllGrantsTo` closes the receiver and calls `unmapGrantPages` during
process teardown (`process.adb` calls this path). That removes CPU mappings,
waits for CPU TLB acknowledgment, and unpins frames. Neither an outstanding
userspace acquisition nor CPU TLB completion establishes GPU quiescence.

`syscall-admin.adb:handleVirtToPhys` is a capability-gated page-table walk;
it does not create a DMA pin or an IOMMU mapping. Acquiring a grant and
translating its CPU address is therefore NOT sufficient admission for public
ANV buffer import. Do not extend that shortcut into the native Mesa factory.

The required backing hold must be owned outside the rendering process/driver
mapping lifetime and tied to the device incarnation. Client or driver exit
must prevent new submissions without releasing possibly GPU-reachable pages.
Release needs trusted completion of engine quiescence/reset and relevant GPU
and IOMMU invalidation, followed by retirement of remaining CPU views. Failed
or uncertain reset quarantines backing rather than recycling it. An ordinary
client completion message is not sufficient evidence. CPU grants can remain
the existing view mechanism instead of inventing another shared-memory API.

Before exposing import/allocation to Mesa, exercise driver exit with outstanding
GPU work, client exit/revocation, stale epochs, reset failure and delayed DMA
against this lifetime. These are implementation/verification requirements, not
claims that DMA leases or IOMMU isolation are already implemented. The present
private retained diagnostic allocation path is not a general public BO API.

The supervisor already requests `SYSCALL_ALLOC_DMA` with retention enabled
for Intel context backing. `syscall-ipc.adb:handleAllocDma` reserves from a
global16384-page retained budget (64MiB); `allocateDma` records
`retainUntilReboot`. `process-ipc.adb:releaseDMAAllocations` removes the old
process owner tags but deliberately skips buddy-free for retained blocks.
This is existing bounded quarantine, not a reclaimable DMA lease or IOMMU
mapping, and can support trusted bring-up without importing ordinary anonymous
pages unsafely. Public BO allocation still needs handle/admission/budget policy;
production reclamation needs trusted quiescence rather than retention forever.

The supervisor context pool now permanently invalidates itself if ownership
loss is observed after any allocation attempt, including at the beginning of
a cached-slot request. Previously that early return could allow a later call
to recover cached backing. A never-attempted pool can still wait for initial
admission. Hosted tests cover loss queried through all16 slots and rejection
of every subsequent slot without another allocation. The owner predicate must
still identify one process/device incarnation; unobserved incarnation changes
are not detected by this Boolean interface.

## Driver-only buffer arena provider

Devmgr now accepts Intel request `0x0236`, exact two-word/zero-flag envelope,
`[slot, page_count, 0, 0]`, only from the admitted Intel driver with authority
tag `0x4947`. Slots are1..16; counts are1..4096 pages. Startup wait handling
returns the existing retry envelope; normal success returns
`[physical, cpu, bytes, slot]`. No application grant/catalog or render-ready
feature bit is added. These address-bearing replies stay inside the privileged
driver/supervisor relationship.

The provider allocates one retained32MiB arena below4GiB, mapped at the existing
per-process DMA aperture convention `0x700000000000`. Intel firmware/context
windows are separate. A single arena avoids consuming the kernel's current
four DMA records for every buffer. It uses the existing global64MiB retained
budget; allocation can fail from budget, fragmentation or record exhaustion.
The arena request is one-shot even on failure. Suballocations are page-aligned,
nonoverlapping, immutable in size, idempotent by slot and never freed/reused.
An exhausted arena rejects new buffers without altering existing reservations.

This path is compiled into devmgr but not yet consumed by the Intel service's
public Mesa transport. Backing contents are unspecified; the driver must zero
them before exposing a CPU grant or GPU mapping. Hosted tests cover all4096
sizes, reordered slots, repeat/resize, exhaustion, failed allocation and sticky
ownership loss. Native devmgr compilation/link passed. No live arena IPC,
hardware execution, reclaimable BO lifetime or IOMMU mapping is proven by this.

The corresponding `Intel_GPU_Buffer_Reply` scalar decoder now validates exact
reply envelopes, slot and requested byte count, page-aligned CPU ranges wholly
inside the arena, and a consistent CPU/DMA offset within a possible below4GiB
arena. It returns the inferred arena DMA base for the caller's cross-reply
consistency check. Hosted tests pass65536 slot/size boundary combinations plus
malformed envelopes, underflow/overflow addresses and nonempty failure replies.
Focused SPARK checks prove the stated alignment/range bounds, successful decode
postconditions, runtime checks and termination. They do not prove endpoint
authentication, actual mappings, consistency between separate replies, absence
of overlap with previous slots or safe DMA lifetime. Those remain caller/owner
obligations before zeroing, granting or publishing buffers.

`Intel_GPU_Buffer_Memory` now connects the decoder to the driver-side supervisor
IPC exchange. It requires a serialized caller and an incarnation-specific owner
check. It rejects inconsistent arena identities and overlapping accepted ranges
before writing memory. Successful acquisition zeroes the complete buffer,
flushes CPU cache lines, and checks every word through volatile reads before
returning the backing. Uncertain transport/clock/ownership failures permanently
quarantine the pool; there is no free or reuse path. Both elapsed time and poll
count bound the request, including when the clock stops advancing.

The hosted adapter fixture passed with real mapped RAM and cache flushing but
simulated IPC: neighbor guards, repeated requests, different-arena and overlap
rejection, malformed replies/completions, submission failure, denial/retry,
timeout/frozen clock, and seven ownership-loss checkpoints. Its generic body
also compiles against the native runtime. The adapter is now instantiated by
the Intel service for its submission image, but is not exposed to Mesa. These
tests are not a proof of DMA lifetime, real IPC delivery, or hardware execution.

The native submission path reserves the first arena slot for its exact image
size and verifies its returned CPU base before using the fixed initial/live
ring instances. It no longer requests a separate 256KiB context allocation.
The superseded `0x0235` endpoint, context allocator/decoder/IPC units and their
old fixtures were removed; `0x0236` is the single buffer backing path. Both
native services build and link. The hosted image writer also passes at CPU
address `0x700000000000` with exact-size backing and neighboring guards.
The private NUC build has also been migrated, retaining its eight drawing VM
leaves and extended expected-image checks. `cubit_live_buffer_vm.img` carries
the new driver and supervisor; older named images remain available. UEFI QEMU
optical and USB-flash boots both pass the native CuBit software-Mesa pixel,
animation and close checks. QEMU does not exercise the physical Intel path.

### Offline binding of retained buffer slices

`Intel_GPU_VM_Buffer.Bind_Range` connects a retained arena backing to an
offline `VM_Image`. It accepts raw48 GPU address, buffer offset and byte count,
checks page alignment and complete source/destination bounds, and maps the
derived DMA pages through the existing atomic `Map_Pages` operation. Failed
requests leave entries and table capacity unchanged. Page-table backing aliases
and the existing unsupported read-only policy remain rejected.

This matches the separation in upstream ANV's `anv_vm_bind`: GPU address,
buffer object, buffer offset and size are distinct. The Mesa-side
`cubit_anv_prepare_binding` validates canonical address representation before
converting to raw48. Neither representation check establishes authority. The
service must obtain backing from its own authorized buffer table, never decode
application-provided DMA/CPU addresses into a trusted backing record.

Hosted regression covers slice offsets, 2MiB boundary crossing, all low
alignment bits, source/destination overflow, late mapping collisions, a full
16MiB buffer ending at the raw48 limit, inconsistent arena identity and sealed
image rejection. The native submission image builder now calls this helper
for its batch/completion slice, passing through the actual retained allocation
record. Its old separate CPU/DMA/capacity argument interface has been removed.
Hosted tests compare the entire resulting image with the prior expected image
at high CPU addresses, and native compilation/link succeeds. The helper neither
updates a published VM nor performs TLB invalidation, client authorization,
buffer release or GPU submission.

### Session-scoped buffer names

`Intel_GPU_Buffer_Handles` adds a driver-internal, serialized registry of
32-bit BO handles, matching ANV's identifier width. Resolve and close require
the authenticated session identity as well as the handle. The identity must
be assigned by the trusted session owner and never reused during this device
endpoint's lifetime; a numeric PID or a session number supplied in a request
is not sufficient authentication. This registry does not implement admission.

Registration accepts only trusted zeroed/retained backing, checks its numeric
arena bounds, and rejects overlaps including ranges whose names were closed.
Closing a handle/session retires names without freeing or reusing DMA storage.
Quarantine makes all resolution fail. This is a bounded bring-up registry,
not a claim that sixteen non-reclaimable buffers suffice for a full Mesa
desktop. Production reclamation still requires trusted GPU quiescence and
mapping retirement. No registry endpoint or new render feature is advertised.

Hosted tests cover cross-session lookup/close, whole-session retirement,
capacity exhaustion, stale handles, retained-range conflicts, malformed
backing and quarantine. The native runtime compilation also passes. These
tests do not establish caller authentication or physical DMA isolation.

Focused SPARK verification proves the declared resolve/readiness equivalence,
successful-registration count/open postcondition, close count preservation,
whole-session name retirement, and reported runtime checks. Session teardown
uses an explicit invariant over the processed entries. The proof assumes the
serialized registry API and trusted session/backing inputs; it does not prove
the still-unimplemented IPC admission or GPU reclamation protocol.

### Admission identity audit

`Process.IPC.resolveEndpointSlot` validates endpoint rights and the referenced
service generation; `capSend` and `capSubmit` replace `Message.authorityTag`
with the capability's tag. `Syscall.Admin.handleMintCap` uses a nonzero
endpoint `object.param` as an explicit policy-supplied tag. Without one it
defaults to the recipient PID. The endpoint generation protects against a
recycled service PID, not a recycled caller PID. Therefore the default PID tag
is unsuitable as a durable render-session identity.

`Intel_GPU_Render_Sessions` models two-phase admission within one service
incarnation: reserve a never-reused tag, ask the trusted capability-space
owner to mint the corresponding endpoint, then finalize from its authenticated
grant result. Only active `(sender, kernel-stamped tag)` pairs resolve to the
session key used by buffer handles. Failed grants, close and quarantine never
reactivate names. A default PID tag or a tag in message payload words is not a
substitute. The fixed tag prefix is a service-local convention, not global
authority; kernel endpoint routing and broker policy remain essential.

This ledger is not yet connected to broker IPC. Caller-exit retirement,
authenticated grant acknowledgements, mint failure handling and rights that
prevent unintended delegation still need end-to-end verification before
advertising a render endpoint. The broker must not reset tag allocation while
the GPU service and its earlier grants survive. Hosted tests exercise the
ledger transitions, not those kernel/policy integration obligations.

The ledger's focused SPARK run proves all17 reported checks (initialization,
runtime checks, termination and the successful-resolution tag bound). A hosted
composition with the buffer registry also checks retirement followed by a new
admission for the same PID: neither the retired tag nor the new tag can resolve
the old BO. Native compilation passes. This does not prove automatic process
exit detection or correct mint/ack IPC, which are not wired in yet.

### Target-incarnation gap in delayed capability minting

The shared kernel now includes syscall 120,
`POLICY_MINT_CAPABILITY_FOR_INCARNATION`: word 0 packs an expected nonzero
32-bit process generation above a 32-bit PID; the remaining arguments match
the existing mint call. The caller must obtain and retain this identity from
authenticated admission (for example, an inspected process-referencing cap),
not re-resolve a numeric PID after making the policy decision. The packed
identity conveys no authority. The existing CSPACE requirement still applies.

The handler checks the expected recipient generation under the same target
mailbox lock used for capability installation. This explicit check precedes
the CSPACE scan, so a wildcard root cannot bypass a stale identity. The bound
call also rejects occupied destination slots and unknown rights bits; it does
not overwrite an existing grant. The original bootstrap mint ABI is unchanged.
The kernel/runtime implementation is promoted, but is not yet
wired into the GPU broker. Kernel compilation is not a proof of lock/lifetime
correctness; PID-reuse stress and scoped-authority cases remain necessary.

The first native test exposed a missing raw syscall decoder entry (enum and
dispatcher alone were insufficient). That omission was corrected privately;
the failed run is retained under `tmp/recipient-mint-native.133t9y`.
The corrected private QEMU run `tmp/recipient-mint-native.v59fcf` passed the
root installation/rejection cases and an authorityless caller with a valid
recipient incarnation. The capability-security runner exited 0 with its
final fault scan passing. These are runtime regressions, not a concurrency
proof or an actual PID-reuse stress test.

The shared `CuBit.Capability_Grants` runtime wrapper now captures a recipient
from an inspected endpoint, process, or CSPACE capability and retains the
generation/PID pair for installation; reply capabilities are rejected because
their generation identifies a thread. Hosted tests cover invalid identities,
rights, syscall failure propagation and retention across a changed subsequent
inspection. Native runtime and test applications compile. QEMU run
`tmp/recipient-wrapper-native.eGNipD` exercises the real wrapper for authorized
installation, occupied-slot rejection and authorityless CSPACE rejection;
the capability-security runner exits 0. This does not establish the broker's
policy decision or authenticate a caller-selected capability as that caller.

Follow-on syscall 121, `POLICY_DELEGATE_ENDPOINT`, avoids giving GPU
drivers CSPACE privileges. It is a broker operation, taking a packed recipient
incarnation, source slot in the broker, destination slot, reduced rights,
explicit authority tag and a zero reserved word. The broker must hold both
CSPACE grant authority for the recipient and a grantable source endpoint.
The kernel copies the source object, parameter and generation through the
existing `Capabilities.mint` operation instead of resolving its PID afresh.
A stale source therefore stays stale; delegation is not an assertion that
the referenced service is live. Rights not present in the source are rejected.
Source, authorization and destination are protected together by the two
mailbox locks in ascending PID order (one lock for self-delegation).
The recipient generation must still match and the destination must be empty.

This operation does not create a revocation tree, bind a policy decision to a
mutable source slot, or prove GPU readiness. The broker must keep its selected
source slot stable across admission, and must not acknowledge installation as
successful GPU-session activation. Production broker integration remains to be
implemented. The CSPACE-requiring in-driver admission prototype was removed;
do not enable that design by granting the GPU broad minting authority.

Native self-delegation regression `tmp/endpoint-delegation-native.byRRvT`
passes grant-right enforcement, rights-subset rejection, recipient-generation
mismatch, nonzero reserved-word rejection, successful copy with source fields
preserved, occupied-slot rejection and unchanged source capability. The full
capability-security runner exits 0. Cross-process delegation, stale source
use, concurrency and broker failure handling still require validation.

Cross-process native regression `tmp/cross-delegation-native.Cn31cD` passes:
private procmgr launch code uses the runtime wrapper to delegate its endpoint
to a child; the child's real call returns the kernel-stamped tag rather than
its spoofed input field. Even a grantable endpoint does not let that child
delegate without CSPACE. Explicit event publication to procmgr is permitted,
while publication to ungranted devmgr is denied. The first run `VXl2C8` exposed
a test-fixture mismatch (it still expected procmgr to be ungranted); those logs
are retained. Full capability-security exits 0 after that fixture correction.
This is a synthetic native broker exchange, not production GPU admission or
an exit/reuse race test. Private launch hooks must not ship in a NUC image.

After narrow promotion, the current shared kernel/runtime compile and the
cleaned `tests/recipient-mint` hosted wrapper tests pass. Shared QEMU baseline
`/tmp/cubit-shared-delegation.8hc0LZ` exits 0 with `capability-security` passing.
That baseline checks regression compatibility, not the additional private
cross-process delegation assertions described above.

The private devmgr bootstrap retains its Intel endpoint in slot 31 with
read/write/grant rights, rather than the former read/write-only source.
This source is held by the existing policy broker, not by the GPU process.
It is necessary for `Delegate_Endpoint` to derive read/write-only application
endpoints while retaining the original GPU generation. This bootstrap edit
has not yet been rebuilt or hardware-tested; public admission is still absent.

### Admission must not block the supervisor service loop

Source review identifies a circular-wait hazard, not a reproduced deadlock:
`Intel_GPU_Buffer_Memory.Acquire` and the GPU's `Request_Authorization` submit
requests to devmgr and wait for completions. Devmgr's main loop currently
blocks in `receive` and handles one request at a time. Adding a synchronous
GPU reserve/activate call inside that handler could leave devmgr waiting for
the GPU while the GPU waits for a devmgr backing or authorization reply.
An asynchronous submission followed by a completion-only wait has the same
problem if it stops servicing inbound device requests.

The user-approved startup-supervisor direction moves spawning/grants out of
devmgr. The production broker should therefore use a bounded pending-admission
table in startup's dispatcher, servicing inbound requests and outbound completions
between steps. Reuse `CuBit.Async_Requests.Tracker` for each pending operation;
its tokens correlate kernel receipts, not authority. Capture the recipient
before policy evaluation and retain the stable GPU source slot for the entire
transaction. Reserve a GPU session, delegate its endpoint, then confirm
activation; installation alone is not success. A timeout detaches the local
request and still requires draining any eventual completion. It does not
cancel remote work, reclaim memory or authorize token/session reuse.

`CuBit.Capability_Grants.Incarnation(Target)` exposes the already-captured
generation32/PID32 for the reserve/activate payload. It performs no inspection
and confers no authority; pass the same retained `Target` to endpoint
delegation. `Process_ID` alone is insufficient for the control protocol.
The hosted recipient fixture verifies that later PID-generation changes do
not replace the identity supplied to installation/delegation, and that invalid
captures reject locally. Mock syscall rejection is not proof of kernel race
handling or an implemented asynchronous supervisor admission path.

The admission transaction now lives in
`userspace/lib/display/intel_render_admission.*`, with its native adapter in
`intel_render_admission_native.*`. The adapter advances one `capSubmit` or
endpoint-delegation operation at a time and accepts completion-queue receipts
from the owning dispatcher; it does not wait or drain a private queue.
Cancellation does not cancel remote work: late successful reserve/activation
requests an abort, and malformed or uncertain cleanup remains quarantined.
An abort acknowledgement closes admission, not backing/grant retirement.
The adapter is not yet installed into a startup supervisor.

`Intel_Render_Admission_Dispatch` is the bounded single-owner container for
that future dispatcher. It advances at most one native action per step in
round-robin order, reserves an exclusive token range, and routes unrelated
completions back to the caller. Admission deadlines are checked both before
steps and before accepting completions: a late activation cannot become Active
merely because the timer callback has not run. Clock rollback cancels pending
and active admissions. Late replies remain drainable after cancellation.
Tickets/destination reservations are retained rather than reused on Abort:
confirmed backing retirement is a separate protocol. Exhausted capacity or
tokens reject further work; three tokens are reserved at admission for reserve,
activate, and abort so exhaustion cannot consume another transaction's cleanup
token. This is not yet a reclaiming, indefinite-lifetime
supervisor. Native strict compilation passed; dispatcher behavior has hosted
mock-IPC regression coverage, not SPARK proof or startup execution.

Run its hosted fixture under Nix with
`gprbuild -p -P tests/mesa-anv/memory-fixture/admission_dispatch.gpr` and
`tests/mesa-anv/memory-fixture/build-admission-dispatch/admission_dispatch_tests`.

Hosted regressions (inside Nix) are reproducible with
`gprbuild -p -P tests/intel-gpu/render_admission.gpr` and
`gprbuild -p -P tests/mesa-anv/memory-fixture/admission_native.gpr`, then their
respective executables in `build-render-admission/` and
`memory-fixture/build-admission-native/`. The first uses the real GPU control
state machine; the second mocks native receipts and delegation syscalls.

Before enabling this route, test a GPU reserve request that deliberately
requests backing through the resource service before replying: allocation and admission
must both finish. Also test timeout followed by late completion and a service
restart between reserve and delegation. Existing synthetic delegation tests
exercise neither circular waiting nor the admission dispatcher.

The internal `Intel_GPU_Buffer_Views` helper now resolves an application BO
through the session-scoped registry and shares only a whole-page subrange using
`Create_Via_Capability`. Existing kernel DMA allocation tags frames to the
driver; `createGrant` requires that ownership when pinning source frames.
Capability-based creation passes the recipient generation to `createGrant`,
which checks it under the grant lock. This is distinct from the delayed
endpoint-mint problem below: the helper requires an already-held stable
recipient endpoint that the caller has associated with the authenticated
session. It does not establish that association itself.

Each limited view is one grant lifetime: Empty -> Shared -> Retiring -> Retired.
Malformed requests or failed creation/revocation permanently fail the view;
there is no retry or reference reuse. Shared exposes the wire reference;
Retiring hides it and polls owner-side retirement confirmation. Backing remains
retained and the BO remains registered; retirement of one grant is not GPU
completion, revocation of other views, or permission to free backing. The
helper must be serialized by the service owner and is not yet connected to a
public render endpoint. Hosted mock-grant tests cover bounds, foreign handles,
deferred retirement, failure quarantine and no retry. Native compilation uses
real runtime APIs; live kernel grant behavior has not been exercised here.

The broker path needs an additional check before it can activate a public
render session. `Syscall.Admin.handleMintCap` accepts a numeric target PID,
checks that target is admitted/not closing under its mailbox lock, and calls
`hasCspaceGrantFor`. A scoped `CAP_CSPACE` requires the target's current
generation to match; the explicit bootstrap root (`object.ref = 0`) does not.
The new endpoint's generation then binds the *GPU service*, not its recipient.
There is no expected recipient-generation argument in this mint operation.

Consequently a delayed grant through a wildcard broker cannot establish that
the recipient is the client incarnation for which the GPU reserved the tag.
If that client exits and its PID is reused before minting, admission of the
replacement PID is insufficient evidence. A fresh render tag by itself does
not close this gap: it could be freshly installed into the wrong incarnation.
This is a code-path finding, not a demonstrated kernel exploit or a claim
that the currently private graphics endpoint is exposed to applications.

Required shared primitive: bind installation to an already-authenticated
recipient incarnation (a selected generation-bound authority or equivalent
request/reply authority), revalidate it atomically with the target slot update,
and fail without changing a slot on mismatch/closing. A separate generation
query followed by PID-only mint is still a check/use race. Preserve the
capability-space authorization check; possession of reply authority alone
must not authorize minting. If scoped authority is selected, a coexisting
wildcard root must not silently replace that selection on mismatch.

Before activation, tests must cover exit/reuse between reservation and grant,
stale/mismatched recipient authority, closing recipients, occupied destination
slots, broker failure after installation, replayed acknowledgements, and GPU
service replacement. Public rendering remains unadvertised pending this
integration. The existing private supervisor-to-driver allocation path and NUC
drawing image do not add this delayed application-grant operation.
