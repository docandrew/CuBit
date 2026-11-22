# Recording a captured scene without publishing it early

The current Desktop `pumpOutput` marks its writer complete immediately after
synchronous CPU/Mesa drawing. A Vulkan backend must not use that transition
after command capture or recording: queued GPU work still owns its writer.

`Vulkan_Scene_Recording.Record_Scene` now provides the production recording
step over the existing owned targets, submission controller, buffer pool and
per-target damage state. The caller first acquires/starts a frame with
`Vulkan_Frame.Begin_Record` and records any audited image/source preparation.
The new step then:

1. Checks target readiness, owning submission context, output epoch, sealed
   scene and source-ticket validity before beginning a render pass.
2. Begins the selected target's pass, replays the scene and ends the pass.
3. Reports `Recorded` while retaining the writer and active repaint plan.
   It never queues or publishes a frame itself. The caller can record final
   barriers, then call `Vulkan_Frame.Submit` and poll the real fence.
4. Cancels unsubmitted commands on clean preflight/replay failure. Successful
   cancellation returns the writer and keeps repaint work pending.
5. Quarantines the writer and damage plan after uncertain begin/end/cancel
   failure. Backing cannot be freed through the owned-target close gate.

The frame contracts now expose that successful begin/end retain the rendering
writer, and that ending a pass preserves the frame's completeness flag. This
lets the recording routine prove its outcomes without conflating recorded,
queued, GPU-complete and display-retired states. No new pixel memory, copying,
foreign interface or shader is introduced.

## Verified evidence, 2026-10-02

- Nine policy cases cover successful recording without implicit submission,
  unsealed scenes, stale sources, wrong context/output epoch, begin/replay/end
  failures and failed cancellation. Recorded work cannot close its targets;
  uncertain failures retain all 12,288 test backing bytes.
- The combined SPARK report contains 702 results (211 flow, 491 prover),
  including unchanged dependencies; zero unproved or justified checks.
- The real Mesa target-bundle fixture now uses this production recording step
  with `Vulkan_Frame` acquisition/submission/polling and repaint state. The
  pool selects the actual target. A real Vulkan fence must finish before the
  writer becomes ready. The 72 physical-fill scenes still pass 55,296 exact
  pixel checks, followed by held-display retirement checks, 2,304 RGB pixels,
  178,176 affine checks and zero Vulkan validation errors.
- Final native component compilation passes in
  `build/private-targets-29k_75b_`, with copied CuBit runtime/Mesa/musl headers
  and source hashes verified before/after compilation.

Logs: `build/vulkan-scene-recording-tests.log`,
`build/vulkan-scene-recording-proof-r2.log`,
`build/vulkan-scene-recording-pixels.log`, and
`build/vulkan-scene-recording-native.log`.

An initial compile needed ticket-operator visibility. The first proof also
exposed missing modular guarantees: successful begin/end retain the writer,
and end preserves frame completeness. The final contracts state precisely
those properties; no stronger claim is made about rendering state after an
uncertain error. Only the final successful proof is acceptance evidence.

Tests/proof use `vulkan_owned_targets.gpr`, units
`vulkan_scene_recording.adb vulkan_frame.adb`. Real hosted Vulkan runs through
`test-vulkan-target-bundle.sh` in `vulkan-affine-shell.nix`. Native compilation
uses `compile-private-targets.py` in Nix.

This is not yet wired into the Desktop service's event/presentation loop.
Image-layout/source-ready preparation and final transitions remain audited
caller obligations. Driver import/output authority, physical display retirement
and end-to-end timing remain outstanding. The hosted display hold is simulated;
these results do not establish native Intel execution, tear-free scanout or
240 Hz/input-to-photon performance.
