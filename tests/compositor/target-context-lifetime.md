# Output targets retain their Vulkan context

`Vulkan_Owned_Targets.Initialize` now reserves a context child before preparing
any output image. It allocates the existing three backing images and attaches
their views using the same submission context. A missing/foreign context or
full child registry rejects creation before allocating. Clean creation rollback
retires the child; uncertain creation or teardown keeps it registered.

The context-aware `Close` requires the matching parent. Existing submission,
output epoch and writer/ready/submitted/front-reader gates still control target
retirement. It retires the parent token only after the images and views are
known closed. Existing low-level operations preserve that token; callers using
the new initializer must use context-aware close to release the registration.
Forgetting that final call retains the context rather than destroying it early.

## Verification

- `vulkan_target_parent_tests.adb` exercises clean budget/creation rollback,
  uncertain image/view operations, registry capacity, foreign contexts and the
  ready/submitted/latched display sequence. Parent close is refused while the
  output child remains held. Existing target/submission tests also pass.
- `build/target-parent-proof-final.log`: fresh `parent-final` proof directory,
  51 checks for the changed target-owner unit, zero unproved/justified. Parent
  preservation contracts were added to the lower-level operations, including
  backing cleanup. Earlier incremental reports retained a stale failed check;
  they are not the acceptance evidence.
- `build/target-parent-vulkan-r3.log`: real hosted Mesa/llvmpipe rendering now
  uses an actual context created through `Vulkan_Context_Owner`, the new target
  initializer and context-aware close. Three independent images pass 2,304 RGB
  pixel checks; 72 scenes pass 55,296 fill-pixel checks. The surrounding affine
  regression passes 178,176 pixel checks; zero Vulkan validation errors.
  Attempts to close the context before target teardown and during a simulated
  display hold are rejected; closing after target retirement succeeds.
- `build/native-scene-8fpgc8hh/result.json`: final native Ada/C scene archive
  built from copied inputs, root and snapshot hashes verified after build.
  `build/target-parent-native-final.log` contains the compile/archive output.

The converted Vulkan fixture initially left images in transfer-source layout
after readback, conflicting with the context's retained LOAD render pass.
It now explicitly restores color-attachment layout after each test readback.
It also clears the first frame explicitly. This preserves the production
context's retained-rendering semantics instead of weakening the render pass.

The native scene archive builder now includes the context creation boundary
object, keeping its existing external-adapter link requirements intact. Native
compilation is separate evidence from the hosted Vulkan tests. No native GPU
execution, real display latch, physical latency or Desktop activation is
established by this change.

## Trust boundary

Private native requests must identify the same device/context and outlive the
owners. Caller states and native storage must not be copied/reset while live.
The display-retirement observation is supplied by the presentation transport;
the tests simulate that observation. The new policy connects that existing
reader gate to context teardown, but cannot prove the external hardware has
stopped reading. Vulkan calls, pointer validity and native retirement reports
remain audited foreign boundaries.

## Admitted-view metadata path

The hosted target oracle now obtains the context request from
`cubit_vulkan_device_context_request`, opens it through the SPARK context owner,
and creates target metadata through the actual `Vulkan_Device_Targets_FFI` Ada/C
boundary. It no longer assembles target/image requests by hand. The same target
owner then allocates and binds all three images using the adapter's filtered
memory mask. Test-only request access permits independent Vulkan readback and
checks of native image/view destruction; production does not export those
addresses.

`build/device-target-rendering-r1.log` passes on Linux llvmpipe: three independent
allocations with 2304 exact RGB pixels, 72 fill scenes across six scales/four
rotations with 55296 exact pixels, blocked context close while targets or a
simulated display front remain held, and clean final retirement/refund. The
surrounding affine oracle checks 178176 pixels with zero Vulkan validation
errors. This is real hosted Vulkan with a locally borrowed device view, not
CuBit render admission, Intel hardware execution or timing evidence.

At that checkpoint, first-frame integration was still missing: the oracle recorded the initial
UNDEFINED-to-COLOR_ATTACHMENT transition before using the retained LOAD render
pass and restores that layout after test readback. The following checkpoint replaces that caller-supplied initial transition.
The target-allocation hook alone still does not submit GPU frames. A cancelled recording must not be mistaken for a
completed layout transition, and retained partial paints must preserve old
pixels. These are the next frame-path acceptance cases.

## Production first-use preparation

`Compositor_Target_Damage.Initialized` now starts false for each target. Only
`Finish(Completed)` sets the active target's bit; changes, beginning a paint,
cancellation and unknown completion preserve those bits. Scene recording asks
the owned target to prepare its layout before beginning the render pass. The
owner checks matching context/output epoch, recording state, acquired writer,
damage extent and a full paint for cold targets. Rejected preparation follows
the existing command-cancellation path.

The narrow C boundary records one image barrier after validation, with UNDEFINED
for cold targets and COLOR_ATTACHMENT_OPTIMAL for retained targets. It performs
no CPU pixel copy, queue submission or wait. Layout correctness relies on the
existing foreign-command/fence observations and on other consumers returning
the image to the documented retained layout; future display/import transitions
must be integrated with that contract. The initialization bit is not a display
retirement observation.

Evidence:
- `build/initial-layout-proof-r1.log`: selected damage, owned-target and scene
  recording SPARK proof, 116 checks, zero unproved/justified.
- `build/initial-layout-faults-r1.log`: recording and ownership fault matrix,
  including preparation failure followed by clean cancellation, PASS.
- `build/initial-layout-boundary-r1.log`: ASan/UBSan six cold/retained barrier
  records, 66 rejected calls without recording, and existing binding checks PASS.
- `build/initial-layout-rendering-r2.log`: actual llvmpipe path cancels a cold
  initialization, retries, verifies 2304 RGB pixels and 58368 fill pixels across
  72 full scenes, three settling frames and one genuine retained partial paint;
  surrounding 178176 affine pixels and zero Vulkan validation errors. The first
  partial fixture failed because alternating buffers still had full damage;
  settling frames consume that history without adding new changes. No assertions
  were removed to obtain the passing result.
- `build/desktop-initial-layout-r1/result.json`: native target-enabled Desktop
  links with the new production preparation path. It does not yet submit GPU
  frames in the Desktop loop, and no hardware frame or timing result is implied.

## Owned sampled source through the admitted-view provider

`build/owned-source-rendering-r1.log` now exercises an actual sampled source
through the new provider. The hosted bridge allocates it with `Vulkan_Image_Owner`
against the same ledger as the three targets, initializes it using GPU transfer
clear commands, and establishes SHADER_READ_ONLY_OPTIMAL. The actual Ada
`Vulkan_Device_Pipeline_FFI.Source_Request` constructor and managed submission
source import then bind it to the singleton pipeline/provider.

A captured scene samples the image and overlays a physical fill. All 768 output
pixels match. Release before the controller observes completion is refused and
native image/backing handles remain live. After completion, source view release
precedes image release; final target teardown returns the shared ledger to zero.
The full oracle checks 59136 scene pixels, 2304 RGB target pixels and 178176
surrounding affine pixels with zero Vulkan validation errors on llvmpipe.

This uses real Vulkan allocation, descriptors, shaders and synchronization, but
not native CuBit device admission or CPU image upload. GPU clear initialization
is a test input, not a replacement for the production upload/import path. The
hosted harness explicitly waits for test readback; no wait or pixel copy was
added to the production frame controller. Pipeline/context orchestration here
uses the hosted bridge; Desktop singleton lifetime/fault tests remain separate.

## Reusing a confirmed-closed image owner

`Vulkan_Image_Owner.Rearm` permits reuse only from `Closed` when its old
allocation ticket is no longer current in the matching device ledger. It clears
only the closed owner's metadata, never the ledger or its identity counter.
Fresh, prepared, live and quarantined owners are left unchanged. A later
`Prepare` still requires fresh, exclusively owned native request metadata;
Rearm does not reset a C request, establish image layout or grant import rights.
Reader retirement, truthful foreign closure and matching-ledger provenance
remain trusted obligations of the caller. The production Desktop source
allocator/upload path has not yet been connected.

- `build/image-rearm-r1.log`: hosted regression passes 32 reuse cycles alongside
  another live allocation, monotonically fresh tickets, refusal before reader
  retirement and refusal after uncertain preparation, bind or release.
  Selected SPARK proof in `vulkan-image-owner/obj/rearm-proof-r1/gnatprove`:
  61 checks, zero unproved/justified.
- `build/image-rearm-vulkan-r1.log`: private hosted llvmpipe fixture passes 32
  sampled-image lifetimes with changing dimensions using this exact production
  owner, shared target budget and actual descriptor provider. Checks 24,576
  textured pixels (82,944 scene pixels total), pending-release rejection, final
  descriptor/backing destruction and aggregate budget refund, with zero Vulkan
  validation errors. Sources are GPU-cleared; CPU uploads are not tested.
- The expanded host fixture is presently private at
  `/tmp/cubit-image-rearm-vulkan-r1`; `build/image-rearm-host.patch` and
  `build/image-rearm-sources.json` preserve its reviewed delta and input hashes.
  Shared-lock conflicts initially prevented publication of those two test-file
  edits; the context-aware source checkpoint below supersedes that pending patch.
  The owner API and unit regression are in the main tree. This is not
  native CuBit execution, hardware timing or display presentation evidence.

## Source backing retains its context before import

`Vulkan_Owned_Source` now owns an image allocation and a registered context
child together. Initialization reserves the child before any Vulkan image
creation, and charges the caller's existing device ledger through
`Vulkan_Image_Owner`. A missing or foreign context, null request, exhausted
child registry or already-held owner rejects creation. Confirmed-clean creation
failure retires the child; uncertain preparation or binding retains it, even
when no descriptor has yet been imported. Confirmed release refunds the backing
and retires the child. Closed owners reuse the guarded Rearm operation while
preserving ledger and context generations.

The caller must provide a matching private request and ledger, a usable device,
and retirement evidence covering upload work, drawing and descriptor release.
A completed GPU frame alone does not retire an imported descriptor. The host
bridge only passes retirement after the managed source release returns its
nonnull key. False retirement and foreign-context close leave resources held.
Foreign Vulkan correctness, device health and pointer/provenance remain outside
the SPARK proof. This package does not upload pixels or establish image layout.

Evidence:
- `build/source-parent-final-r1.log`: nine allocation/failure cases, unimported
  source blocking context close, foreign-parent and held-reader rejection,
  uncertainty retention, other-child preservation and 32 reuse cycles pass.
  The actual llvmpipe fixture now uses this owner for its 32 resized sampled
  lifetimes, with 24,576 exact textured pixels, 82,944 total scene pixels,
  deferred release and final refund; zero Vulkan validation errors.
- `build/source-parent-proof-r2.log` and `.out`: selected new-owner proof,
  13 checks (7 flow and 6 prover), zero unproved/justified. The contracts include
  retention of the context dependency after uncertain creation and release.
- `build/source-parent-native-r1.log`: new owner compiles with CuBit's native
  runtime and Alire compiler into a private object directory. This is compilation,
  not a native execution or display test.
- `build/source-parent-published.json`: all seven new/modified production and
  test files published under the shared lock and byte/hash checked against the
  tested private sources. This supersedes the earlier unpublished reuse fixture.

Desktop's singleton still needs bounded sampled-image metadata construction,
source allocation/upload integration and complete scene capture. Source backing
now has the required parent-lifetime policy, but the live Desktop does not yet
use this package. Native GPU presentation and hardware latency remain open.
