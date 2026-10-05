# Full Desktop optional-render startup fixture

The private Mesa-linked Desktop fixture can now carry a canonical optional
render request while preserving all its existing capability requests. Build
with `tools/build_desktop_vulkan_link.py --device-startup-probe
--optional-render-probe BUNDLE MESA_SOURCE NEW_OUTPUT` in the pinned Vulkan Nix
shell under the shared build lock.

This is a wire-format integration fixture, not production manifest syntax.
`tests/render-startup/desktop_optional_manifest.py` adds one v1 request with
type 11, rights 3, fixture slot 62, param0 1 and param1 0. It rejects malformed
metadata, duplicate render requests and slot collisions. The builder verifies
the modified object and final ELF section. The private main inspects its own
slot and requires it to be empty before exercising the proved software-startup
branch. Slot 62 is specific to this fixture, never a production binding.

Run `test-desktop-vulkan-boot.py LINKED SEED NEW_OUTPUT` inside Nix. It requires
procmgr's software-only admission marker, the Desktop empty-slot marker and the
startup-policy marker, then runs three keyboard/menu restoration cycles. Add
`--approve-render` with a seed containing no rendering provider to require a
failed suspended GPU attempt followed by a distinct software child incarnation.
The latter test requires exactly two attempts, FALSE then TRUE, a denial and
fresh-child retry marker, and exactly one successful Desktop empty-slot probe.

The seed kernel/initrd/services are copied and hash-checked; the fixture builds
only a private disk/ISO and does not replace the normal Desktop image. The
runtime manifest authority rules are exercised in CuBit, but Mesa device
creation and physical GPU rendering are not exercised by these fallback cases.

Production manifests are moving toward typed CCL schemas. Their representation
of optional rendering should be coordinated with that owner; this test does
not introduce or require a new magic-keyword syntax. The required behavior is
optional admission with fresh-process software recovery, independent of its
eventual source spelling. Old frozen initrd/config fixtures remain identified
as seeds rather than evidence that current production CCL syntax compiles.

## Native evidence

- `build/desktop-optional-render-r2/result.json`: linked ELF SHA256
  `d93708bafbe31dbd97ff9b4643503ccb45f89fb4ba6261ea51d0048ef2aba079`,
  verified optional request and required startup symbols. The previous bundle
  failed its libc hash check; the existing bundle builder refreshed dependencies
  before this link instead of bypassing verification.
- `build/desktop-optional-render-boot-r1/result.json`: unapproved optional
  startup, empty slot, owner software branch and all three menu cycles PASS.
- `build/desktop-optional-render-fallback-r1/result.json`: approved but
  unavailable rendering, failed suspended attempt `4294967328`, fresh software
  attempt `8589934624`, empty slot, owner software branch and all three menu
  cycles PASS. The fixture uses four TCG vCPUs and 1 GiB RAM. These identities
  differ in incarnation even though the numeric process slot was reused.

The recovery evidence does not prove retirement of driver resources from a
successful GPU admission. That separate case requires the Mesa/context/target
retirement path and native hardware fault tests.

## Admitted device startup candidate

`--admitted-startup --optional-render-probe` replaces the no-authority-only
probe with a real device/context startup path in the private Desktop. It
inspects the fixture endpoint, initializes the singleton owner once, and checks
initial session health before entering the desktop loop. Software drawing
remains active even when the context is ready; GPU scene submission and display
presentation are not wired by this option. Normal exit attempts bounded
retirement; fatal exit paths do not establish clean retirement.

`Desktop_Vulkan_Startup` owns the device, context and submission state without
exporting copies. The strengthened device close contract proves shutdown never
resets a used owner to fresh. Hosted singleton startup/health/retirement tests
pass. Fresh `startup-final` proof has 456 checks across the source closure,
zero unproved or justified (`build/desktop-vulkan-startup-proof-r2.log`).
FFI behavior, endpoint admission, mapping validity and Mesa remain trusted
boundaries; these proofs do not verify the foreign libraries or the whole
legacy Desktop. Typed production manifest integration remains separate.

The first native candidate linked and reached the software desktop, but its
boot test failed because the minimal runtime emitted numeric enum images.
The builder now emits explicit phase labels. That failed run is preserved in
`build/desktop-admitted-startup-fallback-r1`; it is not an acceptance result.

The corrected candidate `build/desktop-admitted-startup-r2` linked with SHA256
`f56a53344cd9144208b01b0cf7d77a58766f37255c00d9b887c3c7c822e202e6`.
`build/desktop-admitted-startup-fallback-r2/result.json` passes the native
approved-but-unavailable recovery test, singleton SOFTWARE marker, and three
keyboard menu open/restoration cycles. Input/seed hashes are verified by the
runner. This executes the software branch of the actual startup integration;
the admitted hardware branch remains unexecuted. Default staging is unchanged.

## Desktop-owned target lifetime

The singleton now owns a first-output target set, its buffer pool and the
shared image accounting ledger. `Prepare_Targets` takes private native request
metadata and refuses calls before device readiness or after any target attempt.
It initializes three images through the existing context-aware target owner.
`Stop` retires children first; failed device health prevents further child FFI
calls and retains accounting. These entry points do not yet run from the native
Desktop loop: admitted-device metadata construction and output dimension wiring
are the next integration steps. Multiple outputs and resize replacement are not
implemented by this first-output API.

Six independent hosted process tests pass: normal retirement, budget exhaustion,
uncertain target binding, uncertain backing release, software startup and sticky
health loss. Each checks repeated calls cannot reset accounting or retry release.
`build/desktop-vulkan-targets-r1.log` records them; selected singleton proof has
26 checks, zero unproved/justified in `target-owner-proof`. Ledger validity and preservation of the configured byte limit are now proved
through the image, target and Desktop owner contracts (see below). Native linking with actual target,
image and binding boundaries passes in `build/desktop-target-owner-r1/result.json`.
This is not evidence of native target allocation, rendering or GPU presentation.

## Configured budget and device metadata

The image owner and every target allocation/rollback/retirement operation now
promise that the ledger limit is unchanged. The Desktop contract publishes
`Configured_Limit`, proves a first admitted preparation uses its caller's
`Byte_Limit` and never charges beyond it, and proves later rejected requests and
shutdown preserve that limit. Fresh selected proof of image owner, target owner
and Desktop singleton: 144 checks, zero unproved/justified, recorded in
`build/desktop-target-budget-proof-r2.log`. All six runtime fault cases pass with
contracts enabled (`build/desktop-target-budget-tests-r1.log`). This budget is
for owned image allocation requirements; it is not a bound on all Mesa/driver,
process or system memory.

`vulkan_device_storage.h` adds a narrow private metadata constructor using the
same copied admitted device view and live context. It creates no Vulkan objects,
submissions or grants. Three distinct static image records and one target request
are prepared once; protected/lazily allocated memory types are excluded. Invalid
state, dimensions, entry points and memory masks return cleared output. Request
storage remains immutable after publication except for the existing owned-target
binding step. It supports dimensions up to 16384 per axis; allocation limits and
actual device image support are checked by the existing owners/foreign boundary.

The exact adapter was tested in `build/device-target-metadata-dj69nmww` with
address/undefined-behavior sanitizers: invalid inputs, missing entry points,
invalid memory counts/masks, distinct unallocated records, and repeat-call
preservation pass (`build/device-target-metadata-r1.log`). This is a hosted mock
of Vulkan metadata queries, not an admitted hardware test. The native Ada call
boundary and output-dimension hook still need integration.

## Native output-dimension hook

Add `--prepare-targets` to the builder's `--admitted-startup
--optional-render-probe` invocation to prepare the first output's private target
set after display dimensions are validated. This diagnostic candidate uses a
64 MiB image-allocation budget and logs readiness, actual charged bytes and the
limit. The budget can reject large outputs (including three full 4K targets);
it is an explicit fixture limit, not the eventual production output policy.
No pixels are submitted or presented by this option. Default builds and normal
staging remain unchanged, and existing startup-only candidates retain their
scope. A target failure retains software drawing without resetting owners.

`Vulkan_Device_Targets_FFI` is the narrow Ada/C metadata boundary. Its C-layout
record mirrors the native description and clears all outputs on failure,
including a deliberately dirty foreign failure in the hosted test. The SPARK
`Configure_Targets` wrapper guards readiness before the foreign call and passes
requests to the existing context-aware owner. Rejected metadata consumes the
single target attempt; repeats cannot query the device or reset the budget.

Hosted startup plus eight target scenarios pass in
`build/desktop-configure-targets-r1.log`. Fresh selected wrapper proof has
37 checks with zero unproved/justified in `configure-proof` and
`build/desktop-configure-targets-proof-r1.log`. Native target-enabled link
passes in `build/desktop-configure-targets-r1/result.json`. Hardware target
allocation and GPU scene submission remain separate unverified gates.

`build/desktop-configure-targets-fallback-r1/result.json` passes native CuBit
approved-but-unavailable recovery, target-allocation skip and three exact menu
restoration cycles. This verifies the new hook's fallback branch only. The
kernel/initrd seed remains explicit and hash-checked; it is not a fresh complete
system build or a hardware result.

## Desktop-owned bounded frame controller

The singleton now provides `Render`, `Poll_Frame` and `Damage_Output`. A render
requires configured live targets and a sealed captured scene. It records and
submits at most one frame; calls while pending defer without beginning another
command or queue submission. A poll performs at most one fence observation.
Damage arriving during a frame is accumulated through the existing target damage
policy. This is bounded policy work, not a hard wall-clock bound on Mesa or
native IPC calls.

The singleton owns the pool and damage history without exporting mutable state.
`Stop` blocks future render/target admission, retains pending GPU work, and drops
only a completed unpublished ready candidate before trying target/context/device
retirement. Polling can complete pending work after stop; a later stop can then
retire it. Unknown submission/completion or failed health retains resources.
Completion means a GPU-ready candidate, not physical display presentation.

`build/desktop-frame-regression-r1.log` passes the prior startup and eight target
cases plus six new frame cases: normal completion/reuse, 100 busy resubmissions,
100 pending polls and shutdown, fence failure, submission failure, health loss,
and clean cancellation after layout preparation rejection. The native link is
`build/desktop-frame-owner-r1/result.json`. The native drawing loop does not yet
call the new Render/Poll API; captured scene integration and display handoff
remain outstanding.

Final selected frame/scene/Desktop proof passes 168 checks with zero unproved
or justified (`build/desktop-frame-owner-proof-r3.log`). Two initially missing
contract facts were made explicit: successful admission preserves non-faulted
damage, and successful scene recording leaves damage unchanged. Native CuBit
fallback and three exact menu restoration cycles also pass in
`build/desktop-frame-owner-fallback-r1/result.json`, using explicit prebuilt seeds.
No actual native GPU submission is exercised by that fallback test.

## Textured pipeline ownership

`Prepare_Pipeline` registers a context child before creating the existing affine
shader engine and fixed 136-entry source descriptor provider. Creation is tried
once. Known-clean failure retires that registration; uncertain creation or
release retains it. Shutdown requires submission/source quiescence before
releasing the provider and pipeline, then retires its child. Pending GPU work or
failed device health prevents premature destruction. Pixel uploads, imported
image authority and sampled-image backing lifetime are separate work; creating
this provider does not make CPU client buffers valid Vulkan sources.

`--prepare-pipeline` requires `--prepare-targets` and adds pipeline startup after
successful target allocation in the private Desktop. It logs pipeline readiness
without switching drawing or presentation to GPU. Pipeline/provider Vulkan
allocations are outside the image byte ledger; their object/descriptor counts
are fixed, but no total-RAM bound is claimed.

Evidence: `build/desktop-pipeline-tests-r1.log` passes five lifecycle scenarios
and six frame scenarios including pipeline retention during pending work and
health loss. `build/desktop-pipeline-proof-r1.log` proves 68 selected Desktop
owner checks, zero unproved/justified. The real hosted target oracle now creates
and destroys the pipeline/provider through the actual singleton C bridge,
rejects repeat creation/destruction, and passes its existing pixel/retirement
checks with zero Vulkan validation errors in
`build/device-pipeline-rendering-r1.log`. Those fill scenes do not sample images
through this new provider; textured-source integration is still required.
The native candidate is `build/desktop-pipeline-r1/result.json`.

Integration audit: the existing Desktop facade accepts CPU image addresses and
synchronous software drawing. Its completion contract also needs to permit
mutation for a deferred GPU backend. CPU-image capture cannot be substituted for
retained Vulkan source tickets or scanout authority. The proposed shared-target
Display contract remains separate from the current private GPU target set;
production capture and display transport must be connected together before
claiming an accelerated desktop.

`build/desktop-pipeline-fallback-r1/result.json` passes native software recovery,
no target/pipeline creation without readiness, and three exact menu restoration
cycles. This uses explicit prebuilt kernel/service seeds and does not exercise
hardware pipeline creation.

## Desktop source admission and retirement

`Import_Source` and `Release_Source` now keep managed source tickets inside the
Desktop-owned submission controller. Import requires the live pipeline, ready
device, idle command state and open admission. Pending frames return busy without
foreign import calls. Release remains allowed after shutdown begins, but only
while the device is healthy and the command controller can release it. A null
release key grants no permission to free backing. Uncertain import or release
quarantines the submission controller and retains the pipeline/context.

Callers supply trusted, process-private source request metadata for an already
authorized, nonaliasing GPU image and this pipeline's provider. The wrapper does
not turn a CPU pointer into a texture, create an image lease, or validate external
capabilities. CPU-image upload/import, source request construction, and backing
ownership still need integration. `Source_Held` reports registration retention,
including uncertain states; it does not mean rendering or device health is good.

Six hosted scenarios pass in `build/desktop-sources-tests-r1.log`: pending-frame
retention and shutdown release, clean import rejection, uncertain import,
uncertain release, health-loss retention, and stale-ticket/source-slot reuse.
A captured scene retaining the old ticket rejects without queue submission.
The selected Desktop proof passes 77 checks with zero unproved/justified in
`build/desktop-sources-proof-r1.log`. These source scenarios use foreign mocks;
they are not evidence of sampled textures in the live native Desktop.

The actual native source-provider functions link with the new wrappers in
`build/desktop-sources-r1/result.json`. No additional boot/performance claim is
made for these uncalled native APIs.

## Owned-image source request constructor

`Import_Owned_Source` constructs source metadata for this device's existing
provider before invoking managed admission. The SPARK wrapper rejects closed
admission, missing pipeline, pending work and occupied source slots before the
constructor. The C boundary accepts only a private live owned-image record from
the same instance/physical device/device and dispatch, with BGRA8 or R8 sampled
usage. It rejects target image/memory aliases, invalid extents, an occupied
provider slot, and absent/retired pipeline or targets. Fixed request storage is
not mutated on rejection of an occupied slot.

The pointer is trusted process-private metadata, never an arbitrary IPC/client
address. Validation does not establish authority over external memory or prove
actual image layout. Backing ownership and shader-readable initialization remain
caller obligations. Constructing a request does not allocate a source image,
upload CPU pixels or copy any image data.

`build/device-source-metadata-r1.log` passes ASan/UBSan metadata tests with
mocked Vulkan creation: device/role/extent/alias/slot rejection, BGRA and R8,
occupied-request preservation, and retired provider. The six lifetime scenarios
now enter through this wrapper and pass in `build/desktop-owned-sources-r1.log`.
Selected Desktop proof passes 83 checks with zero unproved/justified in
`build/desktop-owned-sources-proof-r1.log`. Actual native boundaries compile and
link in `build/desktop-owned-sources-r1/result.json`. No native source allocation,
upload or textured desktop frame has been exercised by these tests.

The source constructor is additionally exercised with real Vulkan sampled
pixels in `build/owned-source-rendering-r1.log`; see
[target-context-lifetime.md](target-context-lifetime.md). An owned image is charged
to the shared target ledger, initialized with GPU clears, imported through the
actual new provider, sampled, and retained until observed completion. This
closes hosted constructor/provider sampling coverage, not CPU upload or native
Desktop texture/display integration.

## Bounded Desktop source backing

Desktop's GPU singleton now owns five `Vulkan_Owned_Source` entries. They share
one eight-entry allocation ledger with the three output targets and preserve
its configured byte limit and monotonically issued allocation identities.
`Allocate_Backing` requires healthy admitted-device state, ready targets and
pipeline, an idle submission, open admission, a free descriptor slot and a
fresh/confirmed-closed backing owner. It returns a generation-bearing allocation
lease; it does not upload pixels, establish shader-readable layout or import
a descriptor. Source images are BGRA color or R8 masks.

`Release_Backing` rejects stale leases, unhealthy devices, pending work and any
occupied descriptor slot. Shutdown stops admission and attempts to release
unimported, idle backing before target/pipeline/device retirement. Quarantined
backing retains its context dependency, including uncertain preparation with
no confirmed allocation charge. Future uploads must use the same submission
lifetime before this release gate can be used safely.

The narrow `Vulkan_Device_Source_FFI` constructs private metadata on the admitted
device and clears both outputs on failure, even if the foreign call wrote dirty
outputs. Its C adapter owns five static records, uses the admitted device's
filtered memory mask, rejects live/quarantined records and occupied descriptors,
and permits metadata replacement only when no native image/memory remains.
Stage-zero metadata can represent a clean pre-creation rejection, so callers
must serialize and prove the matching SPARK owner fresh/closed before invoking
it; the raw C constructor is not an external capability/import interface.

Evidence:
- `build/source-backing-metadata-r1.log`: ASan/UBSan constructor rejection,
  bounded slot/extent/format checks, BGRA/R8, clean reuse, preservation of live
  or uncertain records and normalized failure outputs, PASS.
- `build/desktop-backing-r2.log`: nine new singleton scenarios plus six retained
  descriptor/source scenarios, PASS. Covers full five-source capacity, 32 reuse
  cycles while the remaining allocations stay live, stale release, aggregate
  budget exhaustion, dirty FFI failure, uncertain preparation/bind/release,
  pending work, shutdown, occupied descriptors and health loss. The first build
  stopped on an invalid Loop_Entry annotation; r2 uses the actual Budget object.
- `build/desktop-vulkan-startup/obj/backing-proof-r2/gnatprove/gnatprove.out`:
  selected singleton proof, 105 checks (69 flow, 36 prover), zero
  unproved/justified.
- `build/source-backing-vulkan-r1.log`: actual device metadata constructor and
  Ada FFI feed the context-aware source owner in the real llvmpipe oracle:
  32 resized sampled-image lifetimes, 24,576 exact textured pixels, 82,944 total
  scene pixels, pending-release rejection, final ledger refund, zero validation
  errors. Pixel initialization still uses GPU clear commands, not CPU upload.
- `build/desktop-backing-r1/result.json`: native Mesa-linked Desktop SHA256
  `fe083b96e252d716f6d323e4a049de8518c61fa51c1a2b7c63afeb8b992a64c7`.
  Native symbols include allocation/release, metadata construction and the
  context-aware source owner. This is link evidence; no native source allocation
  or GPU drawing is activated by the fixture's main loop.

At this checkpoint, the five-source/three-target split left no staging slot
when full. The upload-buffer checkpoint below adds one accounted slot while
preserving the same aggregate byte limit. Production upload admission must
reserve its bytes before CPU-source admission can consume the remaining budget. Pipeline/descriptor allocations and global multi-output admission also
remain outside this image-only ledger. Complete source upload/import, live scene
capture and direct display handoff remain required before hardware activation.

## Accounted mapped upload buffer

`Vulkan_Upload_Owner` now owns a reusable staging allocation and context child.
It reserves the child before creating a transfer-source buffer, charges the
actual Vulkan memory requirement before allocation/binding/mapping, and exposes
only a successful private mapping. Capacity is bounded to 16 MiB per buffer;
this is a maximum allocation size, not an upload scheduling quantum. It selects
compatible host-visible coherent memory and uses a dedicated allocation, so no
noncoherent flush assumptions are hidden in the uploader. Incompatible devices
reject this path rather than expose a mapping with unsuitable memory properties.

The GPU instance of `Compositor_Storage` now enables one extra slot: three
targets, five source backings and one upload buffer. The byte limit is unchanged.
The default CPU instance retains eight slots. The first private regression found
that Total still summed eight entries; the ninth-slot total was corrected before
publication. No failed candidate was published.

Clean rejection releases the context dependency and any charged allocation;
uncertain preparation, binding, mapping or release retains its dependency and
charge where known. A successful mapping with a null pointer is quarantined.
Close requires reader-retirement evidence. Mapping presence does not authorize
CPU writes during GPU use: production upload scheduling still needs to enforce
that exclusion and share the submission state before Desktop can use this API.
The native record, matching device, healthy dispatch, coherent-memory properties
and truthful completion/destruction observations remain audited FFI assumptions.
No queue submit, fence wait or pixel copy occurs in the allocation boundary.

Evidence:
- `build/upload-owner-r1.log`: 13 failure/retirement cases, actual requirement
  accounting, ninth-slot admission beside eight held allocations, 32 safe reuse
  cycles and image-owner regressions pass. The unchanged CPU-ledger regression
  passes 4096 reuse cycles, retained failures, isolation and exhaustion.
- `build/upload-proof-r1.out`: selected upload/image owner SPARK proof, 93 checks
  (39 flow, 54 prover), zero unproved/justified. `build/upload-cpu-proof-r1.out`:
  default CPU ledger, 37 checks, zero unproved/justified.
- `build/upload-boundary-r1.log`: ASan/UBSan native C boundary tests cover 19
  paths, coherent-type filtering, actual-size dedicated binding, mapping,
  incompatible/invalid requests, uncertain retention and confirmed unmap/free.
- `build/upload-buffer-vulkan-r1.log`: actual llvmpipe now replaces GPU clear
  initialization with CPU stores directly into the accounted mapped upload
  buffer, followed by a buffer-to-image transfer and sampled rendering. Across
  32 resized source lifetimes, 24,576 textured pixels and 82,944 total scene
  pixels match; pending work prevents upload-buffer release; final unmap/free
  and aggregate refund pass, with zero Vulkan validation errors. The test's
  transfer commands and completion waits remain fixture code, not a production
  upload recorder or latency measurement.
- `build/upload-native-cpu-r1.log`: new owner compiles against CuBit's native
  runtime, and C boundary compiles through the native Mesa compiler. No native
  execution is claimed. `build/upload-desktop-ledger-r1.log` passes all nine
  Desktop backing scenarios against the revised ledger.
- `build/upload-published.json`: 18 implementation/test files published under
  the shared lock and hash checked against tested private inputs. The device
  storage header's lifetime/slot comments were subsequently clarified without
  changing declarations or executable code.

Next integration: private admitted-device staging metadata, Desktop ownership
and byte admission, bounded upload geometry/recording, completion-controlled
write access and shader-readable source publication. The live Desktop remains
on software drawing, and direct GPU display handoff is still outstanding.

## Checked transfer recording and submission

`Compositor_Upload` validates BGRA/R8 transfer rectangles, image extents,
four-byte buffer-offset alignment, optional padded row length and the complete
source byte span using bounded wide arithmetic. It also constructs full-width
row chunks that fit the available staging capacity, allowing images larger
than one staging allocation. A valid chunk does not imply that the whole image
is initialized: completed row coverage must still be tracked before import.

`Vulkan_Upload_Recording` charges one bounded command attempt through the existing
submission controller, requires recording outside a render pass, matching held
context dependencies, live upload/source owners, a fitting plan and no descriptor
in the source's assigned slot. The private source must belong to that slot and
have no descriptor aliases elsewhere. Invalid geometry or foreign rejection
invalidates the batch so it must be cancelled. `Seal_Transfer` seals a nonempty
admitted transfer batch without pretending it is a rendered output frame.
Existing Submit/Poll/Cancel then govern completion and uncertain retention.

The narrow native recorder checks matching device/dispatch, nonaliasing backing,
live objects, sampled-image role and format, exact image dimensions, bounds,
stride and staging capacity before issuing any command. A successful call emits
producer/image barriers, one buffer-to-image copy and the shader-readable
transition. It performs no CPU pixel copy, allocation, submit or wait. The caller
must supply truthful previous-layout information, finished producer writes,
exclusive source ownership and a command buffer recording outside a render
pass. Shader-readable layout alone does not make partially initialized pixels
publishable. This remains a private interface, not an IPC pointer importer.

Evidence:
- `build/upload-record-policy-r1.log`: 380,160 enumerated pixel-address geometry
  cases, 4K row tiling, extreme extents and undersized buffers pass. Recording
  tests cover invalid plans, wrong parents, occupied descriptors, admission
  exhaustion, cancellation, pending polls and seal/submit/poll uncertainty.
- `build/upload-record-proof-r1.out`: selected geometry, recording and submission
  SPARK proof, 111 checks (64 flow, 47 prover), zero unproved/justified.
- `build/upload-record-vulkan-r1.log`: ASan/UBSan native boundary checks cold and
  retained BGRA plus R8 barrier/copy sequences and 40 rejected calls with no
  command emission, followed by real Vulkan execution.
- `build/upload-record-vulkan-r2.log`: llvmpipe executes the production geometry,
  Ada/C recorder and transfer submission controller for 32 CPU-written resized
  sources, alternating BGRA color and R8 masks. All 24,576 textured pixels and
  82,944 total scene pixels match. Pending upload and draw work retain staging;
  final unmap/free/refund passes, with zero Vulkan validation errors. The host
  still supplies completion waits; production Poll itself never waits.
- `build/upload-record-native-r1.log`: native CuBit Ada/C compilation passes,
  followed by the existing submission/source/frame/damage regression suite,
  including 1000 lifecycles, 10000 pending polls and bounded-command exhaustion.
  This is not native upload execution or a timing result.
- `build/upload-record-published.json`: 20 implementation/test files published
  under the shared lock with byte/hash checks against the tested private snapshot.

Desktop still needs admitted-device staging construction/admission, a distinct
upload-pending purpose alongside frame-pending work, completion-controlled write
access, and confirmed initial row coverage before source publication. The normal
Desktop drawing loop does not yet invoke this recorder. Hardware presentation
and physical latency measurements remain outstanding.


## Completion-gated chunked source uploads

`Compositor_Upload_Progress` tracks a source image's confirmed full-width row
prefix. Each write receives a nonwrapping ticket tied to its backing identity.
Only a matching pending transfer completion advances coverage. Stale/duplicate
observations leave state unchanged; uncertain producer cancellation or GPU
completion quarantines the source. Publication requires the entire image.
A retired descriptor's same allocation may receive new content without native
reallocation; its chunk sequence persists across content and backing changes.

This is a per-image policy. The owner must separately exclude writers to the
shared staging buffer, retire the CPU producer and any recorded commands before
confirming cancellation, retire descriptors before restarting content, and
observe the matching submission fence. Backing identities and request provenance
remain trusted caller obligations. The policy performs no waits or pixel copies.

Evidence:
- `build/upload-progress-r1.log`: 2160 completed chunks for four 3840x2160
  content versions with a 64 KiB staging capacity, 21600 pending observations,
  cancellation/retry, stale/duplicate completion, identity validation, uncertainty
  and sequence exhaustion pass. Its original proof invocation analyzed zero
  checks and is explicitly superseded by the corrected proof below.
- `build/upload-progress-proof-r2.out`: the SPARK-enabled model instance analyzes
  all 17 entities, with 23 checks (16 flow, 7 prover), zero unproved/justified.
  Run `gnatprove -P tests/compositor/upload_progress.gpr --mode=prove --level=2
  -j2 --checks-as-errors=on -u upload_progress_model.ads` inside Nix.
- `build/upload-progress-native-r1.log`: the policy and its instance compile
  against the CuBit Ada runtime; this is compilation, not native execution.
- `build/upload-progress-vulkan-r1.log`: actual llvmpipe transfers through the
  production recorder/submission path use only 384 staging bytes. All 32 BGRA/R8
  contents require multiple chunks: 16 resized allocations and 16 same-allocation
  rewrites. Patterned data matches 24576 textured pixels, with 82944 total scene
  pixels checked and zero Vulkan validation errors. Writer/pending staging reuse,
  premature descriptor import (including after fence wait but before policy
  observation), and partial-content publication are rejected. Final retirement
  refunds all allocation charges. Host waits remain test-fixture operations.
- `build/upload-progress-published.json`: nine production/test files match the
  tested private snapshot. The private path-adjusted runner is not published.

The guarded write/import integration currently lives in the hosted Vulkan bridge.
Desktop's normal loop still needs staging admission, shared-writer ownership,
separate upload/frame pending purposes, and completion-gated source publication.
No hardware rendering, scanout, throughput or physical latency claim follows.


## Desktop staging allocation admission

The singleton now owns one `Vulkan_Upload_Owner` alongside its output targets and
source backings. `Configure_Upload` requires a ready admitted device, configured
healthy targets, live pipeline and idle submission. It constructs private native
metadata through `Vulkan_Device_Upload_FFI`, then charges actual buffer allocation
requirements to the existing shared budget before binding/mapping. Call it before
allocating source images to preserve staging headroom. Repeated live admission,
pending-frame admission, shutdown and uncertain owners cannot reset the buffer.

`Release_Upload` requires a healthy device and idle submission; Stop tries it
before retiring targets/context. No mapping is exported by Desktop yet, so there
is currently no CPU writer to retire. Adding the writer API MUST extend this gate
to cover producer retirement as well as GPU completion. The memory limit covers
these owned Vulkan allocations, not Mesa's total process memory.

Evidence in `tests/compositor/build/desktop-upload-*`:
- `policy-r1.log`: eleven Desktop scenarios pass, including pre-start rejection,
  insufficient budget, clean retry, dirty/uncertain outcomes, null mapping,
  pending shutdown and health loss. All three targets, five source backings and
  staging coexist under one exact budget; staging is retired/reused 32 times.
- `proof-r1.out`: actual Desktop singleton and upload owner analyzed, 152 checks
  (93 flow, 59 prover), zero unproved/justified. Both Configure_Upload and
  Release_Upload contracts are covered. Native memory/device/FFI facts remain
  trusted boundaries, not SPARK proofs of Vulkan implementation behavior.
- `native-r1.log`: changed Desktop and Ada bindings compile against CuBit runtime.
- `regressions-r1.log`: existing startup, nine backing, eight target, six frame,
  six source and five pipeline scenarios pass with the new owner present.
- `vulkan-r1.log`: ASan/UBSan constructor tests pass: admitted-device identity,
  nine rejection/preservation cases, confirmed closed reuse and provider teardown.
  Its later Vulkan harness compile failed on a pointer-edit typo; fixed in r2.
- `vulkan-r2.log`: real llvmpipe runs the admitted-device metadata constructor,
  production buffer owner and chunked upload path. 32 patterned BGRA/R8 contents,
  24576 textured/82944 scene pixels and zero Vulkan validation errors; retention
  and final unmap/free/refund pass. This uses the hosted bridge, not live Desktop.
- `published.json`: twelve changed production/test/builder files hash-verified.

The native link helper includes the upload-buffer C boundary and requires its
symbols when linking admitted Desktop. Normal drawing remains software. Next is
Desktop's global writer lease, upload-vs-frame pending purpose, per-source row
coverage and completion-gated importer, followed by actual scene/presentation
integration. Hardware timing remains unmeasured.

Native integration follow-up: the old service bundle correctly failed hash
verification after the runtime changed (`desktop-upload-link-r1.log`). A new
`desktop-upload-service-bundle-r1` was built against the current runtime; all
input guards remained enabled. `desktop-upload-link-r2/result.json` records the
successful native Desktop link and required upload-owner/C boundary symbols.
The binary SHA256 is
`60a396fae31efaacbadfda2c7cf60bfd213818de181e6319a5821429f837d3b9`.
`desktop-upload-fallback-r1/result.json` records native QEMU software recovery
with explicit frozen kernel/service seeds: unavailable GPU startup retries a
fresh software child, and three keyboard/menu cycles restore exact pixels.
This is software fallback execution, not native upload or hardware performance.


## Desktop protected writer and transfer completion

Desktop now integrates its source row-coverage policy with one shared staging
writer. `Begin_Write` returns a private ticket, checked full-width row chunk and
mapping. A second writer, frame submission, descriptor retirement, backing or
staging release cannot overlap that producer. Submit/Cancel invalidate its write
permission; producer retirement remains an audited same-process caller fact.
Pointers must never be retained or used after permission ends. No IPC or foreign
client pointer import is added by this API.

Each accepted transfer shares the existing bounded submission lifetime. Only
`Poll_Upload` observes its fence; `Poll_Frame` leaves that transfer alone. The
reverse also holds for frames. Completed chunks release the shared uploader so
a frame can run before the next chunk. Each polling call makes one observation,
never waits. Uncertain producer/recording/submission/completion retains ownership
and accounting instead of guessing that memory is reusable.

Owned backing slots 0..4 now import only through `Import_Backing`, with matching
allocation generation and complete confirmed row coverage. The public trusted
external-source imports use slots 5 and above. Restarting a source requires its
descriptor retired and clears coverage without reallocating or resetting chunk
sequences. Allocation replacement also preserves the per-slot chunk sequence.
Stop cannot retire an active writer or transfer; completion/cancellation remains
available during shutdown so a subsequent Stop can finish cleanly.

Evidence (`tests/compositor/build/desktop-writer-*`):
- `proof-r3.out`: 210 selected SPARK checks (129 flow, 81 prover), zero unproved
  or justified. The actual Desktop singleton invariant permits writing/pending
  coverage only on the active source, pairs CPU writing with idle submission,
  and pairs upload completion with the matching pending transfer. The nested
  coverage policy and write-permission contracts are included.
- `verify-r2.log`: earlier strengthened proof exposed descriptor retirement
  overlapping CPU writing and insufficient phase/frame guarantees. The overlap
  is now rejected; fresh-device state and completion phase contracts were
  strengthened. No assumptions or waived checks were added.
- `native-tests-r2.log`: fourteen lifecycle/fault scenarios pass, including
  partial import rejection, stale generation/ticket rejection, cancellation,
  interleaved frame work and distinct polling, pending shutdown, health loss,
  recording/sealing/submission/completion uncertainty, and descriptor-release
  exclusion. Native CuBit Ada compilation then passes.
- `regressions-r2.log`: startup, backing, target, frame, external source, pipeline
  and upload-admission regressions pass against the final policy. The backing
  descriptor test now completes an actual mocked upload before importing.
- `published.json`: ten production/test/builder files match the private snapshot;
  the Release_Backing comment was clarified after proof without code changes.

Foreign mapping validity, producer retirement, native handle identity, memory
visibility and Vulkan completion observations remain trusted boundaries. These
Desktop scheduler tests mock Vulkan; preceding real llvmpipe tests cover the
underlying upload/rendering components, not this complete singleton integration.
The native builder now links the transfer-recording C boundary and checks writer,
submit, poll and importer symbols. The normal Desktop loop still uses software.
Next: execute this singleton against real Vulkan, then feed complete desktop
scenes and client content through it and connect output to display presentation.


## Actual Desktop scheduler on real hosted Vulkan

Run `nix-shell tests/compositor/vulkan-affine-shell.nix --run
'bash tests/compositor/test-desktop-real.sh'` under the shared build lock.
The new oracle uses actual Desktop_Vulkan_Startup, Device_FFI, Mesa_Service,
context/target/source/upload owners, transfer recorder, shader pipeline and
submission controllers. Only the service transport is replaced by a host adapter
that lends the harness's real llvmpipe device; it does not validate CuBit launch
or GPU authority. Link wrappers observe private context/target metadata, and
Vulkan dispatch wrappers count source creation/destruction and identify the
recorded framebuffer. Every Vulkan operation is forwarded to real Mesa.

`build/desktop-real-upload-r6.log` passes 40 chunks using 384 staging bytes,
eight BGRA/R8 contents, four actual sampled-image allocations and four rewrites
that preserve the same native image. All 6144 read-back pixels match patterned
color or mask expectations. The test rejects simultaneous writers, premature
retirement and import before full coverage, including after a host fence wait
but before Desktop observes completion. Frame and transfer polls remain distinct.
Final closure destroys all four source allocations, target images and context,
refunds Desktop's accounting, and releases the simulated service owner once.
The existing affine oracle also passes 178176 pixels; Vulkan validation reports
zero errors. This is correctness evidence, not a quiet-host performance run.

The initial r1 test adapter used mismatched C prototype types and did not compile;
that fixture was corrected to the production header. r2/r3 deliberately retained
pixel diagnostics for a missed caller obligation: version2 reused an output and
showed version0 because the test had not marked changed scene damage. Source
upload completion alone cannot identify its output placement. The final bridge
calls Damage_Output for the changed textured quad before Render; r4 passes BGRA,
and r5/r6 add R8 and backing replacement. Production scene/client integration
must supply the corresponding damage, rather than relying on first-use clears.
No Desktop implementation was changed to make this oracle pass.

`build/desktop-real-upload-published.json` hashes seven test/runner files.
`CUBIT_COMPOSITOR_ALIRE_ROOT` optionally selects an existing compiler project for
an isolated source snapshot; the default is this checkout's kernel directory.
Normal Desktop still draws in software. This test does not establish native GPU
execution, zero-copy scanout, tear-free physical presentation, 240Hz throughput,
or input-to-photon latency.


## Full source capacity under one byte budget

Desktop now supports all 136 existing descriptor slots as owned backings: room
for 128 glyph masks and eight other images. Native metadata and the Ada FFI
range match. A slot may instead hold a trusted external source, but external
import requires a Fresh or confirmed-Closed Desktop backing; allocation rejects
an occupied descriptor. Live, partially uploaded and quarantined backings cannot
bypass Import_Backing's completion gate. This changes private slot policy only,
not cross-process import authority or the normal software Desktop path.

Compositor_Storage now takes Slot_Count, replacing the special-case ninth slot.
Its exact allocation-sum invariant remains. CPU capacity defaults to eight;
GPU accounting selects 140 entries (136 sources, three targets and one upload).
The context registry holds 139 children (the targets share one child ticket).
These are metadata limits, not automatic image allocations or increased byte
limits. Every real Vulkan allocation requirement still charges the same budget
before allocation; uncertain release cannot refund it. Physical memory facts,
foreign completion and release remain audited adapter assumptions.

Private Nix verification /tmp/cubit-source-capacity-r1:
- Full Desktop test: 128 R8 plus eight BGRA images, each written, completed,
  imported and retired, with exact 573440-byte mocked allocation budget. It
  rejects external/owned slot overlap in both directions and stale backing
  import after a confirmed slot changes role.
- Selected Desktop/context/image-owner proof: 326 checks, 169 flow and 157
  prover, zero unproved/justified. Independent eight/140-slot accounting proof:
  146 checks, including exact refunds and nonwrapping identities. Existing
  4096-cycle and 140-allocation/1120-replacement ledger tests pass.
- Image/context owner tests, 14 writer fault/lifecycle scenarios, nine backing
  scenarios and six external-source scenarios pass.
- C metadata bounds and preservation checks pass with address/undefined
  sanitizers over all 136 slots.
- Hosted actual Desktop on real llvmpipe still passes 40 chunks, eight contents,
  four allocations/four rewrites and 6144 exact pixels. The affine oracle passes
  178176 pixels, with zero validation errors. This real oracle exercises four
  allocation lifetimes, not all 136 simultaneous native allocations.

Evidence: build/source-capacity-* and build/storage-capacity-proof.out.
Reproduce the full policy test with desktop_source_capacity.gpr; existing
writer/startup/owner projects supply regressions. Proof selects
 desktop_vulkan_startup.adb, vulkan_context_owner.adb, vulkan_image_owner.adb
through the capacity project. Source hashes cover the published files.
No native GPU execution, performance, or full desktop scene integration is
claimed by this capacity milestone.


## Padded-row staging and direct glyph producer

Begin_Write now accepts optional Row_Pixels (zero preserves packed behavior).
The checked row planner rejects a stride narrower than the image and accounts
for every padded row, including the final one, before returning its mapping.
The confirmed-prefix and writer/transfer lifetime policies remain unchanged.
This allows the existing font rasterizer's aligned pitch to feed Vulkan's
bufferRowLength directly, without retaining or repacking another CPU image.

Desktop_Glyph_Upload.Start checks a complete R8 image against the density plan,
including extent, origin, full height, zero offset, pitch and capacity. It calls
the existing font boundary on the exclusive staging mapping, then submits once
without waiting. Too-small staging, wrong geometry/format or failed/invalid font
metrics cancel the producer without publishing. Caller must observe completion
and import the backing before associating a glyph source or drawing it. Staging
must fit the whole glyph; this helper does not accumulate partial font rasters.

The existing Compositor_Glyph_FFI implementation is unchanged and outside proof.
Its now-SPARK-visible specification documents the audited synchronous call,
non-retention of the mapping, and checked raster/advance/capacity guarantees.
Global null describes named Ada state, not writes through the foreign pointer.
Font parsing, actual pixel writes, addresses and returned metrics are trusted
boundary facts; new upload geometry and orchestration are proved SPARK.

Private Nix evidence in /tmp/cubit-strided-upload-r1:
- Stride/Desktop proof: 243 checks, 146 flow and 97 prover, zero unproved.
- Direct glyph orchestration proof: selected desktop_glyph_upload.adb; report
  in build/direct-glyph-upload-proof.out. No proof claim for the font body.
- Full 136-source test uses 41-pixel images in 48-pixel rows and rejects a
  narrower stride. Existing 2160-chunk/21600-pending coverage tests and all
  14 default-stride writer lifecycle/fault scenarios pass.
- Eight glyph scenarios cover accepted/pending/completed upload, font failure,
  zero/oversized advance, wrong height, undersized staging, wrong format and
  wrong image extent. This fixture mocks raster pixels and GPU completion.
- Real hosted Desktop llvmpipe uses 48-pixel rows for 32-pixel BGRA/R8 images,
  poisons all padding with 0xa5 and compares all 6144 rendered pixels exactly:
  60 chunks, eight contents, four allocations/four rewrites. The accompanying
  affine oracle passes 178176 pixels, zero validation errors. This verifies
  strided transfer pixels, not real font-to-GPU text or native hardware latency.

Reproduce with desktop_strided.gpr, desktop_glyph_upload.gpr (scenarios 0..7),
upload_progress.gpr, desktop_writer_tests.gpr (0..13), and test-desktop-real.sh
in the documented Nix Vulkan shell. Source hashes: strided-upload-published.json.
The normal Desktop still uses software; glyph residency, scene capture and
presentation integration remain required.


## Real fonts and typed glyph association

Desktop now owns Vulkan_Glyph_Sources alongside its private submission.
Bind_Glyph requires a completed owned R8 backing, matching allocation identity,
matching image extent for the requested density, the same slot's live source
ticket, and idle producer/submission. Slots 0..127 map to the bounded glyph
association table; all 136 backing slots remain available for general images.
Face/code identify the producer-supplied raster: this metadata does not inspect
or prove the font's pixels. Capture_Glyph uses the existing typed scene method;
missing, retired, wrong-key or wrong-output-density sources reject the snapshot.
No private submission state escapes the Desktop singleton.

The new hosted oracle links the existing cubit-fonts Rust rasterizer, not a mock.
It exercises both bundled faces at 100%,125%,150%,200%, four image allocations
and four same-image rewrites. A link wrapper verifies every production font
call receives exactly the owned Vulkan upload mapping; the existing synchronous
rasterizer writes there directly. Separately rasterized CPU references use the
same font engine, so this checks integration/placement/transfer rather than
independently proving TrueType rasterization. All 49152 target pixels match
exactly, including fractional-density pitch padding and snapped placement.
Premature imports fail both before fence completion and after a host fence wait
until Desktop observes completion. Typed binding/capture then records the scene.
Final cleanup refunds all accounting and closes the service adapter once.
The accompanying affine oracle passes 178176 pixels; validation errors are zero.

build/glyph-bindings-proof.out records the selected Desktop and glyph-source
policy proof. The extended eight-scenario glyph fixture checks premature binding,
wrong slot/lease, wrong raster density, duplicate live binding, wrong key/output
scale and stale association after descriptor retirement. All eight cases pass.
Sources/hashes/logs are under build/glyph-real-* and build/glyph-bindings-*.
Private reproducible source/font capture: /tmp/cubit-glyph-real-r1; font archive
and IBM Plex font hashes are recorded in glyph-real-font-inputs.log.

To run in Nix, build the existing fonts-host target under the shared build lock,
set CUBIT_FONT_HOST_ARCHIVE to its absolute libcubit_fonts.a path, then run
 test-desktop-glyph-real.sh in vulkan-affine-shell.nix. The optional
CUBIT_GLYPH_CAPTURE_DIR must name an existing directory; eight PPM readbacks are
saved after pixel verification. Default compiler project is kernel, with the
existing CUBIT_COMPOSITOR_ALIRE_ROOT override for isolated source snapshots.

This is Linux-hosted llvmpipe with borrowed-device service admission. It is not
native CuBit GPU execution, display handoff, a latency measurement or full
Desktop UI scene capture. The normal desktop still renders through its existing
software path. Cache eviction/residency and mainloop integration remain required.


## Bounded GPU glyph residency

Desktop_Glyph_Residency now owns up to 128 glyph backings and 128 independent
reader leases, exclusively using backing slots 0..127. Its limited state cannot
be copied/reset to discard live ownership. Acquire performs at most one eviction
and one upload admission, and Poll makes one completion observation. Ready cache
hits skip rasterization and upload. Saturation defers; captured/pinned glyphs
cannot be eviction victims. The default CPU cache retains its 32-reader limit.
Actual GPU allocations remain charged to Desktop's aggregate storage ledger;
logical glyph bytes do not represent total Mesa or process memory.

Readers must survive both CPU scene capture and GPU execution. Capture_Retired
is a caller attestation that the CPU snapshot no longer references the glyph.
Release additionally requires Desktop.Can_Retire_Readers: a healthy, idle live
renderer, available writer, and no pool/damage fault. Frame_Pending=False alone
is insufficient after uncertain completion. Unknown outcomes quarantine resources
and retain charges. Close refuses live readers; successful close refunds glyph
ownership before the caller stops Desktop. No waiting is introduced.

Selected concrete SPARK analysis of residency and Desktop completed 387 checks
(196 flow, 191 prover), none unproved or justified. The strengthened default cache
contract passed 101 checks (29 flow, 72 prover) and its 4096-cycle regression.
These are selected-unit results, not proof of Mesa, Rust fonts, Vulkan, the whole
Desktop, or the caller's Capture_Retired assertion.

Mock tests fill all 128 resident/pinned slots, check no duplicate uploads,
bounded saturation, frame-safe eviction, stale reader rejection, accounted
shutdown, five allocation/font/transfer/import failure cases, and uncertain
frame completion retaining both reader and allocation. The final uncertain-frame
fixture was trimmed after proof; its assertions were rerun successfully.

The real hosted oracle retains eight native glyph images from both bundled
faces at 100%,125%,150%,200% density, verifies cache hits skip font calls, matches
49152 glyph pixels and 178176 affine pixels, reports zero validation errors,
and confirms all eight images are destroyed and allocation charges refunded.
Run test-desktop-glyph-residency-real.sh with CUBIT_FONT_HOST_ARCHIVE in the
existing vulkan-affine-shell.nix environment. Evidence and hashes are recorded
under build/glyph-residency-* and build/glyph-uncertain-final.log.

This remains hosted llvmpipe with mocked service admission. It does not establish
native GPU execution, display presentation, hardware latency, or full Desktop
scene capture. The normal Desktop path remains software; GPU scene integration
must retain and retire these reader leases according to actual snapshot lifetime.


## Complete scene ownership and glyph readers

Desktop_GPU_Scene owns a limited, non-copyable Vulkan_Scene snapshot plus its
glyph residency/cache. Begin_Frame starts only from idle healthy state. Append
uses existing solid/clip/texture/backdrop layer representation; raw Glyph_Mask
is rejected so callers cannot bypass Add_Glyph's reader acquisition. Add_Glyph
retains one reader per distinct key for the entire captured scene: repeated
text does not consume one reader per occurrence. The bounds remain 128 distinct
glyph readers and Vulkan_Scene.Maximum_Layers (512) ordered commands.

A cold glyph starts one asynchronous upload and invalidates that capture.
Finish discards the CPU snapshot rather than submitting any prefix; Poll makes
one completion observation and returns Retry once a fresh scene can be built.
This lets the event loop process input before recapturing current state. It does
not queue old snapshots, wait, or silently truncate text. Overflow and invalid
capture reject the complete scene. The caller must choose the software fallback
or a suitable complete rendering strategy for scenes beyond these limits.

After submission, Discard/Close cannot retire readers. A confirmed completion
first discards the owned CPU snapshot, then releases its glyph readers through
the healthy-idle gate. Unknown completion quarantines the owner. Closing the
cache unsuccessfully also quarantines the owner, preventing a new capture from
using a stopped cache. Completion means renderer completion, NOT display latch
or scanout retirement. Output damage remains supplied to Desktop before Finish.

Non-glyph layers currently refer to caller-owned immutable source assets; their
tickets are revalidated by production Desktop submission. This owner does not
implement authenticated client-buffer import, source-producer immutability,
per-output device ownership, or actual mainloop/display integration. Normal
Desktop rendering remains on the software path. No new FFI is introduced.

Mock tests cover a cold cache, 100 pending upload polls, mixed fill/clip/text
layers, 200 repeated glyph occurrences with one reader/one rasterization,
100 pending frame polls, forbidden pending close/discard, uncertain completion
retaining the reader and allocation, overflow, raw glyph bypass rejection,
explicit capture discard and accounted shutdown. The selected scene-owner
SPARK proof passes 55 checks (29 flow, 26 prover), none unproved or justified.
This establishes its stated contracts/initialization/bounds over the existing
policy contracts, not foreign rendering or caller-owned source lifetime.

The real hosted scene-owner oracle drives capture/retry/finish/poll using existing
Mesa llvmpipe and the Rust font rasterizer. Eight retained images, two faces and
four DPI scales match 49152 glyph pixels; the affine companion matches178176
pixels, with zero validation errors. Confirmed frame completion releases scene
readers automatically; close destroys all eight images and refunds the ledger.
Run test-desktop-gpu-scene-real.sh in vulkan-affine-shell.nix with the existing
CUBIT_FONT_HOST_ARCHIVE and optional CUBIT_COMPOSITOR_ALIRE_ROOT variables.
Evidence: build/gpu-scene-{r1,r2,real-r1}.log, gpu-scene-proof.out and source
hash manifests. These are hosted correctness tests, not hardware performance.


## Straight-alpha embedded icons

Straight-alpha icon rendering integration

The desktop Mesa drawing path submits embedded icon and window-icon atlases
without allocating a converted premultiplied atlas. Source RGB is multiplied
by source alpha in the fixed-function blend stage; source alpha itself uses
ONE, and the destination factor is ONE_MINUS_SRC_ALPHA. Existing premultiplied
and replacement modes remain distinct. The native software fallback remains.

The affine descriptor retains its layout. Its Over field now admits 0 (replace),
1 (premultiplied source-over), and 2 (straight source-over). The Vulkan engine's
private C structure now holds three pipelines: every consumer must rebuild
against the matching header. Partial initialization destroys all three slots.
Both foreign adapters reject invalid blend indices before indexing state;
masked/glyph operations require premultiplied source-over. The raw legacy blit
continues to admit only its existing two modes.

SPARK coverage: affine/binding/frame/scene 280 checks, Desktop facade 82 checks,
submission 74 checks, zero unproved/justified in those selected runs. This proves
the stated policy contracts, initialization and bounds under imported contracts.
It does not prove Mesa, Vulkan, C pointer validity or display behavior. Desktop
main's new atlas call remains integration-tested code, not a new proof claim.

Evidence: hosted Vulkan 304 direct cases, 233472 pixels, 72 straight-alpha
requests; full scene 712 submissions, 546816 pixels, 115 straight requests;
zero Vulkan validation errors. Partial-pipeline creation and recording guards
are exercised. Native CuBit softpipe suite passes 384 output requests plus
existing affine/mask/glyph tests. Actual private Desktop linked with source and
dependency hashes; native dual-output interaction passes. Expanded 125/150%
DPI, primary-output changes and arrangement validation also pass.

No physical GPU, scanout, 240 Hz or input-to-photon claim follows. Ordinary
Desktop remains software-rendered. Full GPU scene routing and authenticated
client/display buffer ownership remain work toward the overarching goal.
