# Capturing Desktop fills in output pixels

Desktop's native-output `fillRect` path already applies output geometry and UI
clipping before calling `Desktop_Compositor.Draw_Fill`. Its `Area` argument is
an output-local physical rectangle. Treating those values as a logical Vulkan
scene layer would apply DPI scaling, rotation and output origin a second time.

`Vulkan_Scene.Append_Physical_Fill` now captures that rectangle as
`Physical_Solid`. It reuses the existing layer storage, source-free opaque fill
semantics and immutable scene ordering. Raw physical layers reject negative or
greater-than-65535 coordinates. Empty or inverted rectangles produce no fill.

`Vulkan_Frame.Replay_Physical_Fill` intersects the supplied pixels with the
target's damage and current clip. It neither rescales nor rotates them. Logical
`Replay_Fill` first performs its existing geometry mapping, then uses the same
bounded implementation. Each layer still issues at most eight fill attempts;
the 512-layer scene and shared 4,096-draw budget are unchanged. Scene metadata
remains 24,608 bytes in the hosted x86-64 test. There are no new pixel buffers,
pixel copies, foreign entry points or shaders.

This prepares the renderer's capture interface; Desktop has not switched to it.
The production Desktop sources were left untouched while the graphics owner
rebuilt consumers for the grant ABI change. A native Vulkan backend must still
connect frame begin/end, source admission and queued submission to presentation.
Capture acceptance is not GPU completion or permission to return a source grant.
Icon capture, source uploads/imports and the driver/display handoff remain work.

## Evidence, 2026-10-02

- Hosted policy tests cover 72 combinations of scale, rotation, visible extent,
  out-of-output clipping and empty rectangles, plus malformed raw coordinates.
- The final combined SPARK report for frame/scene has 558 results (176 flow,
  382 prover), including unchanged dependencies, zero unproved or justified.
- Actual hosted Mesa runs 72 scenes across six scales and four rotations with
  a nonzero output origin. All 55,296 output pixels match a scalar physical-pixel
  oracle. These scenes use the production owned target bundle and actual
  submission/fence controller, rather than only calling the fill FFI directly.
- The same executable still passes three-target allocation/retirement, 2,304
  RGB pixels, 178,176 affine pixel checks and 18 foreign binding rejection cases,
  with zero Vulkan validation errors. Display retirement is simulated.
- The complete preexisting submission suite passes 712 actual queues, 546,816
  output-pixel checks and 19,336 covered glyph pixels after the replay refactor.
- Final CuBit component compilation passes in the isolated snapshot
  `build/private-targets-018sruyz`, using copied runtime and existing Mesa/musl
  headers with source hashes checked before and after compilation.

An initial test compile required explicit Ada type visibility/conversions. The
first proof attempt encountered an unsupported local record declaration inside
a loop. Moving it outward exposed a stale success flag on a defensive malformed
layer return; explicitly clearing that flag completed the proof. Those failed
attempts are not acceptance evidence. The final proof, pixel and native logs
are `build/vulkan-physical-fill-{policy,pixels,native}-r3.log`; the broader
regression log is `build/vulkan-physical-fill-regression.log` (before the
defensive rejection-path fix). The final pixel run covers the final source.

Run in Nix:

```sh
gprbuild -p -P tests/compositor/vulkan_submission_mock.gpr
tests/compositor/build/vulkan-submission-mock/vulkan_submission_mock_tests
cd kernel
alr exec -- gnatprove -P ../tests/compositor/vulkan_submission_mock.gpr \
  -u vulkan_frame.adb vulkan_scene.adb --mode=all --level=2 --report=all \
  --checks-as-errors=on -j1
```

The actual pixel suite is
`nix-shell tests/compositor/vulkan-affine-shell.nix --run 'bash tests/compositor/test-vulkan-target-bundle.sh'`.
These are correctness and component-compilation results, not native GPU scanout,
frame-rate, physical latency or 240 Hz measurements.
