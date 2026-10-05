# Borrowed Mesa device to compositor context

This bridge is groundwork for Desktop's Vulkan backend. It is not connected to
Desktop startup or presentation yet, and does not establish GPU acceleration
on CuBit. Desktop's existing Mesa backend still uses softpipe.

`vulkan_context.c` takes the already admitted, process-local device view from
`Mesa_Service`. It resolves device dispatch through the borrowed instance
procedure address and creates one resettable command pool, one primary command
buffer, one fence and one BGRA8 render pass. The existing submission adapter is
initialized against that exact device and queue. No second device, loader,
capability, image backing or presentation authority is acquired. The render
pass loads existing color attachment pixels to preserve undamaged regions;
callers must initialize fresh targets and establish the required layout before
use. Its conservative external dependency is not a latency measurement.

All dispatch entries needed for destruction are checked before creation.
Partial initialization rolls back only successfully created objects, even if a
failed Vulkan call poisons its output parameter. All objects are unsubmitted
at this stage. Success and failure both consume the one creation attempt;
repeated creation cannot overwrite retained ownership. Release contains no
wait and destroys each child at most once. Vulkan calls and native transport
may still block; this is not a wall-clock bound or an asynchronous API.

`Vulkan_Context_Owner` places the lifetime policy in SPARK. A fixed eight-entry
registry retains child-resource obligations before native creation. Tickets
bind a context address, slot and nonwrapping generation; stale or foreign
tickets cannot retire another child. Registration preserves existing children,
and retirement preserves unrelated entries. Failed/uncertain child retirement
keeps the obligation. Closing requires an empty registry plus a matching
`Vulkan_Submission` with no pending work or registered source imports. Foreign
release failure permanently quarantines the context without retry.

Before production use, every dependent target-view and pipeline owner must
register before creation and retire only after confirmed destruction or a
known-clean rollback. `Native_Retired` is a trusted observation, not a proof
that a GPU or display finished. Display consumption must be discharged by the
existing target/pool owners before their child ticket is retired. Caller
serialization, unique uncopied owner storage, private immutable requests,
borrowed device lifetime, Vulkan implementation behavior and pointer validity
remain outside these policy proofs. There is deliberately no device-loss
recovery or context-address reuse path here.

## Evidence

- `build/vulkan-context-VasdeM2P` / `vulkan-context-host.log`: all four creation
  failures, all sixteen missing device dispatch entries, missing instance
  dispatch, repeated operations and exact cleanup tested with mocks. Sixteen
  real hosted Mesa create/destroy cycles passed with zero validation errors.
  No queue submission or CuBit GPU execution occurs in this test.
- `build/vulkan-context-owner-r2.log`: child capacity, stale/foreign tickets,
  retained source imports, recording/pending GPU work, successful completion,
  clean rejection and quarantine tests passed. All 575 SPARK checks including
  dependencies passed, with zero unproved or justified checks. Foreign
  procedures' stated effects and termination remain trusted.
- `build/vulkan-context-native-h1bp46w8/result.json`: native Ada owner/FFI and
  musl C bridge compilation passed against a frozen source/runtime snapshot.
  No final native link, Desktop activation or physical GPU execution claimed.

Run hosted tests in `vulkan-affine-shell.nix` with
`bash tests/compositor/test-vulkan-context.sh`. In the standard Nix shell,
build/run `vulkan_context_owner.gpr` and prove `vulkan_context_owner.adb` with
`--checks-as-errors=on`. Native compilation uses the kernel Alire toolchain:
`alr exec -- python3 ../tests/compositor/test-vulkan-context-native.py MESA_SOURCE`,
where MESA_SOURCE is an absolute path to the existing Mesa tree in this checkout.


## Rendering through the owned context

`test-vulkan-context-rendering.py` runs the existing scene/pixel oracle with
context creation and retirement routed through `Vulkan_Context_Owner` and its
real C boundary. It uses a private source snapshot, retains a child obligation
before creating target views and pipelines, rejects early context close, and
retires that obligation only after the test has destroyed those children.
The test uses the exact same native submission identity held by the owner.
Its host-only setup/upload and final fault-injection commands include explicit
waits; those waits are test scaffolding, not production scheduling policy.

The successful run completed 712 actual Vulkan queue submissions, checked
546,816 pixels, included 96 retained wallpaper scenes, and reported zero
validation errors. Partial repaint touched 15,240 versus 73,728 full-frame
pixels across 96 test scenes. These are correctness/work-count observations,
not physical GPU timings or a 240 Hz claim. All input hashes and any concurrent
root drift are recorded in the private artifact. See
`build/vulkan-context-rendering-r2.log` and the artifact path it names.
The first run failed to compile due to one leftover reference to the old
manually-created submission variable; the successful test uses the owned
context throughout. Desktop startup/device admission and production child-owner
integration remain pending.
