# Compositor-owned Vulkan image backing

`Vulkan_Image_Owner` now uses the existing proven `Compositor_Storage` ledger
to admit private GPU image backing. `vulkan_owned_image.c` is the narrow foreign
boundary; it calls the supplied authorized Mesa/Vulkan dispatch table. No Mesa
allocator, driver or Vulkan implementation is duplicated.

The sequence is:

1. Prepare a private BGRA8 output/source or R8 sampled image. Check format,
   extent and usage support, create the image, then query its actual memory
   requirements. No device backing allocation is issued by prepare.
2. SPARK intersects compatible memory types with the caller's allowed mask,
   selects the first match in at most 32 steps, and reserves the full reported
   byte count in a shared device ledger **before** calling allocation/binding.
3. Publish the image only after the owner reaches `Live`. Missing capacity or
   an incompatible memory type closes the unbound image without allocation.
4. Release only after the caller establishes that all GPU work and dependent
   views/descriptors have retired. A denied retirement does not call Vulkan or
   change the ledger. Clean release refunds the reservation; uncertain binding
   or release quarantines it. Released identities cannot become current again.

There are eight backing slots per ledger. The current accounting type supports
at most `Natural'Last` bytes; larger driver requirements are rejected before
allocation. The budget is supplied by the caller, not a hardcoded desktop limit.
The hosted pixel fixture uses one 16 MiB ledger for three images. This is not a
measurement of total Desktop or Mesa memory. Vulkan metadata, command buffers,
driver caches and allocations outside this owner are not charged here.

The first implementation uses dedicated optimal-tiling backing, including the
dedicated-image allocation structure required by implementations that mandate
it. The intended use is output targets and shared source atlases, not one
allocation per character. Upload/layout-transition policy remains separate.
See the Khronos contracts for [memory requirements](https://docs.vulkan.org/refpages/latest/refpages/source/VkMemoryDedicatedRequirements.html)
and [image binding](https://docs.vulkan.org/refpages/latest/refpages/source/vkBindImageMemory.html).

## Proof and trust boundary

The focused SPARK report has 59 results (25 flow, 34 prover), zero unproved or
justified checks. It verifies the owner against the ledger's contracts, including
budget validity, state transitions and unchanged ownership on held readers.
The existing ledger supplies non-wrapping identities and bounded byte accounting.

Trusted obligations remain explicit:

- A matching, authorized Vulkan 1.1 physical/logical device and immutable private
  foreign request storage. Do not copy/reset a live owner, ledger or C record.
- Driver function/handle validity, compliant failure behavior and truthful memory
  requirements. The C adapter and Ada import wrappers are outside SPARK proof.
- The allowed memory-type mask comes from the same physical device and excludes
  types whose requirements the caller cannot meet (for example protected memory).
- One ledger covers these owners on the device; the caller may not evade the
  aggregate budget by constructing a new ledger for every image.
- Actual fence completion and destruction of dependent views/descriptors before
  supplying retirement evidence. The owner does not manufacture that evidence.
- Vulkan destroy/free completion establishes API resource release, not proof of
  kernel physical-page reclamation. Unknown resources stay quarantined.

These are private, non-exported images. CPU grants do not become GPU import
authority. This interface does not implement external-memory import, display
borrowing, scanout retirement or native Desktop activation.

## Validation

Run the policy test and proof in the repository's Nix development environment:

```sh
gprbuild -p -P tests/compositor/vulkan_image_owner.gpr
tests/compositor/build/vulkan-image-owner/vulkan_image_owner_tests
cd kernel
alr exec -- gnatprove -P ../tests/compositor/vulkan_image_owner.gpr \
  -u vulkan_image_owner.adb --mode=all --level=2 --report=all \
  --checks-as-errors=on -j1
```

Policy tests cover all 32 memory-type positions, nine rejection/uncertainty
cases, held readers, eight-slot exhaustion and stale identities after reuse.
The C boundary's independent fault test covers seven missing dispatch functions,
format/extent/usage rejection, create/allocate/bind failures, success with invalid
handles, mandatory dedicated backing, insufficient charge, incompatible memory
types, prepared-image cancellation, destruction order and repeated calls.

Run real hosted Vulkan plus those boundary faults with:

```sh
nix-shell tests/compositor/vulkan-affine-shell.nix \
  --run 'bash tests/compositor/test-vulkan-owned.sh'
```

On 2026-10-02 this passed with three production-owner images, 212 draws and
178,176 pixel comparisons across scaling/rotation/blending cases. All three
images retired and their backing charges returned to zero. Vulkan validation
reported zero errors. This is Linux llvmpipe, not native Intel rendering.

The complete existing submission fixture also passed after the shared host
cleanup helper change: 712 queue submissions, 546,816 output-pixel comparisons,
19,336 covered glyph pixels and zero validation errors. That fixture still uses
its test allocator; only the separate three-image fixture uses the new owner.
No native GPU performance, physical latency or 240 Hz claim follows.

`vulkan_image_native.gpr` supplies the CuBit-runtime compile check. After the
initial lock attempts expired, the subsequent run passed against CuBit's Ada
runtime. `vulkan_owned_image.c` also compiled using the existing musl Mesa
build's exact ABI command. Log: `build/vulkan-image-native-r2.log`. This is a
native compilation result, not native GPU execution.

## Three-target lifetime integration

`Vulkan_Owned_Targets` now owns three backing images together with their
`Vulkan_Target_Owner` views. `Allocate` admits backing without publishing it;
`Attach` binds those same images to the view request and creates the views.
The foreign binding rejects device, role, extent, image-handle or allocation
alias mismatches before writing any image into the target description.

Backing allocation failure rolls back already-created unsubmitted images.
A clean view-creation failure also releases backing. Uncertain view creation,
binding validation or view destruction retains every backing reservation.
Once attached, `Close` uses the existing target owner's gate: correct output
epoch, the bound quiescent submission with no retained sources, a valid non-faulted
pool, and empty writer/ready/pending-display/front roles. Views are destroyed
before any backing image or memory. Failed gates leave both state and budget
unchanged; that behavior is part of the proved contract.

`Attach` captures the submission's private native context identity. The C
binding verifies its device matches the images and that queue, command and
fence handles are present before publishing views. An unrelated idle context
cannot close the bundle, even with the correct output epoch. A null context
is rejected before view creation and its unpublished backing is released.

Callers must not copy/reset the bound controller, recycle its native context
address while attached, submit the private images through another controller,
or recreate an empty display pool to invent retirement. The policy cannot
authenticate a physical fence or display latch by itself.
Unattached backing can be cancelled only because the caller is forbidden to
submit, export or otherwise publish those handles before `Attach` succeeds.

Validation on 2026-10-02:

- Combined SPARK report: 578 results (183 flow, 395 prover), including unchanged
  dependent units; zero unproved/justified checks. An initially missing loop
  invariant was supplied before the successful proof.
- Policy tests: partial-budget rollback, failed/uncertain view creation,
  uncertain binding/release, recording and pending GPU work, wrong epoch,
  writer/ready/pending-display/front retention, unpublished cancellation, and
  null/duplicate request rejection, unrelated submission rejection and null
  context cleanup.
- Production C binding: one successful independent set and 18 invalid sets,
  with no partial request publication.
- Actual hosted Mesa: three private images allocated by the SPARK bundle,
  rendered through its views, and read back as 2,304 exact RGB pixels. After
  actual GPU fence completion, a simulated held front prevents destruction;
  releasing that front closes every image/view and refunds the backing budget.
  The same executable passes 178,176 affine pixel checks with zero Vulkan
  validation errors. GPU rendering is real; display retirement is simulated.

Run the bundle's hosted pixel and C fault suite with:

```sh
nix-shell tests/compositor/vulkan-affine-shell.nix \
  --run 'bash tests/compositor/test-vulkan-target-bundle.sh'
```

Policy tests/proof use `vulkan_owned_targets.gpr` and unit
`vulkan_owned_targets.adb`. Logs are `build/vulkan-owned-targets-policy.log`,
`build/vulkan-owned-targets-final-tests.log`, `build/vulkan-owned-target-binding.log`
and `build/vulkan-target-bundle-run.log`. The real fixture's first two build
attempts exposed a C/Ada object-name collision and a missing Ada test bridge;
both were corrected before the passing run.

The bundle's native-runtime compilation now passes in a private snapshot with
the complete compositor dependencies and both C boundaries. Reproduce with:

```sh
nix develop -c python3 tests/compositor/compile-private-targets.py
```

This copies regular independent files for compositor/display sources, CuBit
runtime, and existing musl/Mesa Vulkan headers; it checks original and copied
hashes before and after compilation. No shared output or runtime is modified.
The final context-bound result is `build/private-targets-pfh2mali/result.json`,
with inputs in its `inputs.json` and commands/toolchain versions in
`build/vulkan-owned-context-native.log`. The earlier pre-context snapshot was
`build/private-targets-japdqimn`. Shared-lock attempts expired; these passing
component builds use the repository's permitted isolated snapshot workflow.

Context-bound policy/proof evidence is in `build/vulkan-owned-context-policy.log`;
the corresponding real pixel and 18-case foreign rejection evidence is in
`build/vulkan-owned-context-pixels.log`. These supersede the earlier logs for
the added association checks. Native compilation is not linking, booting or
executing on the Intel GPU. Desktop adoption, external image import,
GPU-to-display borrowing and hardware presentation remain outstanding.

Logs: `build/vulkan-image-owner-policy.log`, `build/vulkan-owned-run.log`,
`build/vulkan-owned-submission-regression.log`; the proof report is in
`build/vulkan-image-owner/obj/gnatprove/gnatprove.out`.
