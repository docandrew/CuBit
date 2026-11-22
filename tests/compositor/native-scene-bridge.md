# Native Ada scene bridge

`native_scene_bridge.ads/.adb` exports a trusted, externally serialized C
integration fixture over the production SPARK compositor. It is not the
Desktop backend or a client GPU-import ABI. The public C contract is
`native_scene_bridge.h`; every caller must satisfy its Vulkan object, layout,
immutability, lifetime and synchronization requirements.

The bridge owns a private three-target bundle and a 16 MiB backing ledger,
retains one borrowed affine source, and records a full-size source followed by
a green physical rectangle at [4,12) x [4,12). It supports output extents from
12 through 65535, subject to Vulkan support and the actual allocation budget.
The initial graphics gate uses the existing 64x64 Mesa triangle source.

The pool chooses the writer. Begin returns that slot without beginning a pass,
allowing caller-owned preparation barriers. Record invokes the production
`Vulkan_Scene_Recording.Record_Scene`; Submit seals and queues only after the
caller records final layout/readback barriers. Readback must use that same
command buffer/submission. Poll queries the actual fence once and never waits.
A pending result retains all objects; neither timeout nor cancellation releases
submitted work. Ready remains held until explicit Release after the CPU
consumer returns. Close rejects a held ready frame and frees views before
backing. The borrowed source stays immutable throughout the entire open session.
An uncertain result is sticky and prevents reopening or releasing resources.
A clean Close permits a fresh cycle; the accounting ledger is not reset.

This fixture intentionally does not manufacture display-latch evidence. Its
CPU readback release is caller-supplied evidence, not proof of physical scanout
retirement. A production Desktop path still needs authenticated producer/output
leases and actual presentation integration.

## Build and link

From the repository root, use the runtime's matching Alire toolchain inside Nix:

```
nix develop -c bash -c 'cd kernel && alr exec -- python3 ../tests/compositor/build-native-scene-bridge.py'
```

The script creates an independent `tests/compositor/build/native-scene-*`
snapshot, verifies 801 input hashes, builds `libcubit-native-scene.a`, and includes
a binder-generated `compositor_sceneinit` routine. Call it exactly once before
any bridge call, never again to reset a session. The snapshot includes the
matching runtime archive and the precise header used by the build. It writes
no shared runtime, staging, image or Git index outputs.

The archive contains Ada policy and its elaboration. The graphics linker must
also include the matching `userspace/runtime/adalib/libgnat-user.a` and these
production C adapters, compiled against its Mesa/musl headers:

- `vulkan_submission_native.c`
- `vulkan_targets.c`
- `vulkan_owned_image.c`
- `vulkan_owned_target_binding.c`
- `vulkan_affine.c`, with generated affine shader headers
- `vulkan_sources.c` (submission's managed-source entry points reference it,
  although this fixture uses a borrowed source)

Include each adapter once; the previous C-only smoke's inclusion of submission
implementation must not duplicate it. Compile affine shaders with the existing
`build-vulkan-affine-shaders.py`. Use the existing affine engine, source
view/descriptor, compatible BGRA render pass and three fresh image requests.
Native graphics owns the app, barriers, linker hook and CPU presentation.
No Vulkan implementation is replaced.

## Evidence and limits

- Nine mock scenarios pass: normal completion, start/begin-pass/draw/end-pass/
  cancellation/seal/submit/poll failure. Unknown outcomes retain backing.
- Normal execution exercises 1,000 pending polls without a second submission;
  100 clean open/begin/cancel/close cycles retain monotonic accounting.
- Real C-to-Ada calls exercise open/begin/record/submit, pending completion,
  forbidden pending cancellation/close, completion, explicit release and close.
- Final bridge SPARK report: 736 results (230 flow, 506 prover), including
  dependencies; zero unproved or justified checks. This proves checked Ada
  paths and contracts, not Vulkan correctness or physical synchronization.
- Native compilation, binding and archive creation are checked against an
  independent copy of the CuBit runtime. This is not application linking,
  native execution, GPU pixel verification, scanout or latency evidence.

Logs: `build/native-scene-bridge-proof-r4.log` and
`build/native-scene-bridge-final.log`. Proof report:
`build/native-scene-bridge/obj/gnatprove/gnatprove.out`.

The first proof needed explicit damage bounds rather than an unproved relation
between global output and damage dimensions. The bridge now requests the
actual damage state's complete bounds. Initial native binding also exposed
GNAT 15 from plain Nix versus the runtime's GNAT 16: the documented build uses
Alire's matching toolchain and does not suppress binder consistency checks.

## Shared real-Mesa/native command setup

The hosted integration fixture is `native_scene_pixels.h`, called by
`vulkan_affine_host.c`. It links the exact bridge and production C adapters,
uses eight open/close lifetimes with 16 submitted frames each, and checks every
pixel of the source texture plus green rectangle. Source data is uploaded once
and remains immutable through every session. The actual source shader-read
transition and descriptor construction are in `vulkan_affine_host.c`; there is
no GPU import or synthetic fence-completion evidence.

Native graphics can reuse `native_scene_transfer.h` verbatim. Its
`cubit_native_scene_readback_commands` takes the selected output image and a
caller-owned readback buffer plus Vulkan function pointers. It records the
color-write -> transfer-read layout/dependency, image-to-buffer copy and
transfer-write -> host-read dependency. It never submits or waits. Call it
between Ada Record=0 and Submit; all readback commands therefore complete under
the same fence checked by Ada Poll. A rejected adapter call records nothing:
cancel the still-unsubmitted frame before releasing its resources.

The working target pass has one BGRA8_UNORM attachment, one sample, CLEAR/STORE,
initial layout UNDEFINED and final COLOR_ATTACHMENT_OPTIMAL. Its external to
subpass dependency is TRANSFER/TRANSFER_READ -> COLOR_ATTACHMENT_OUTPUT with
COLOR_ATTACHMENT_READ|WRITE. Full repaint makes discarding previous contents
valid; this fixture does not demonstrate retained partial-repaint layouts.
Owned image requests use COLOR_ATTACHMENT|TRANSFER_SRC and match the pass and
output dimensions. Native graphics must retain all request structures, the
source draw context and every referenced Vulkan resource until Close=0.

The immutable source descriptor uses the affine engine's descriptor layout and
sampler, a BGRA8_UNORM view and SHADER_READ_ONLY_OPTIMAL. Source image usage must
include SAMPLED at creation, in addition to its producer's color/transfer usage.
The existing triangle/teapot image cannot gain that usage retrospectively.
Complete the producer before borrowing it; record a correct producer-layout to
shader-read transition and memory dependency before the compositor samples it.
For an image already read back by the producer, its tracked old layout is
TRANSFER_SRC_OPTIMAL. Preserve the source until the compositor session closes.
The first gate may explicitly wait the producer fence; it is not a pipelined
or zero-copy-scanout performance claim.

Run the actual bridge pixel oracle inside the existing Vulkan Nix shell:

```
NATIVE_SCENE_EXTENT=64 bash tests/compositor/test-native-scene-pixels.sh
NATIVE_SCENE_EXTENT=256 bash tests/compositor/test-native-scene-pixels.sh
```

Each includes a second process that deliberately omits the output transition;
it must fail and produce a Vulkan layout/synchronization diagnostic. The normal
path checks that pending polls, completed-but-unreleased frames cannot be treated as a reusable target.
Uncertain-outcome retention is covered by the separate mock fault tests. Simulated consumer rejection
returns its CPU borrow only after checking the pixels; it is not a UI.App test.

Final hosted evidence: `build/native-scene-pixels-shared.log`, with per-size
positive/negative logs in `build/native-scene-pixels-{64,256}`. Both sizes pass
8 lifetimes, 128 submissions, 384 forced pending observations, 16 never-submitted
cancellations and 64 simulated consumer rejections each. Exact bridge pixel
counts are 524,288 and 8,388,608 (8,912,896 combined). The accompanying affine
regressions check 950,272 and 15,204,352 pixels; both report zero Vulkan validation
errors. Missing-barrier controls diagnose the actual attachment-to-transfer
layout mismatch and read-after-write hazard. Source hashes are recorded in
`build/native-scene-pixels-source.sha256`. These are hosted lavapipe checks,
not Intel hardware execution or throughput/latency measurements. No Ada bridge
source changed after the successful native archive/proof handoff.
