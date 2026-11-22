# Vulkan wallpaper operation

The wallpaper recorder reuses the existing Mesa/Vulkan affine engine and its
68-byte push-constant layout. Mode 2 draws physical-output Fill, Fit or Center
wallpaper with the Desktop software painter's endpoint-aligned bilinear
sampling and integer rounding. It does not apply logical DPI scaling to the
wallpaper a second time. Fit/Center fragments outside the placed image preserve
the previously drawn opaque background.

`Compositor_Backdrop` prepares bounded placement and damage clipping in SPARK.
`Vulkan_Backdrop_Binding` maps an empty plan to no commands and otherwise crosses
one audited foreign interface. The 56-byte descriptor holds geometry, never
pixels or memory authority. The clip fields use nonnegative signed 32-bit Ada
subtypes; their permitted values 0..65535 have the same representation as the
C descriptor's uint32 fields. The ABI and actual Ada-to-C rendering path are
regression-tested. The C entry point independently validates its entire input
before recording any command.

`Vulkan_Submission.Backdrops` connects the operation to the existing submission
controller. It charges the shared 4096-attempt cap, validates source generation,
checks output dimensions and command/device correspondence, and invalidates the
whole candidate frame on rejection. It neither creates a second queue nor
releases a source. The existing controller retains source tickets through
recording and pending work; uncertain cancellation quarantines the controller.
Empty operations still consume one bounded attempt.

## Accepted evidence, 2026-10-02

- Geometry, sampling and binding proof: 79 analysis results, no unproved or
  justified checks (`build/backdrop-proof-all-r7.log`). Earlier r5/r6 failures
  remain recorded; they are not accepted proof results.
- Submission adapter proof: 8 analysis results, no unproved or justified checks
  (`build/backdrop-submission-r1.log`). This invocation proves the new adapter
  against the parent controller's contracts, not a fresh proof of all parents.
- Hosted placement tests: 61,440 combinations including singleton and maximum
  dimensions, clipped/empty/reversed damage, all three modes, and 56-byte ABI
  (`build/backdrop-policy-r7.log`).
- Actual hosted Mesa llvmpipe Vulkan: 210 frames and 161,280 exact pixels per
  shader variant. Both normal quotient estimation and forced integer division
  pass, including large/tall source images. Existing affine regression also
  passes 212 draws and 178,176 exact pixels per variant, with zero Vulkan
  validation errors (`build/backdrop-final-r7.log`).
- C boundary: 32 rejected descriptors/dispatch configurations record zero
  commands; the accepted case records six commands with a 68-byte push.
- Submission fault tests: stale/missing source, mismatched output, foreign
  mismatch/failure, empty damage, rejection preventing subsequent recording,
  pending source retention, successful completion/release, full 4096-attempt
  cap, and failed cancellation retaining sources in quarantine all pass.

The shader and C/Vulkan behavior are audited and tested, not SPARK-proven.
Resource mapping validity, actual image dimensions/format, descriptor contents,
render-pass compatibility, synchronization, and true GPU completion remain
foreign/provider obligations. The recorder performs no allocation, upload,
barrier, submission, wait, display publication or retirement.

## Run

From the repository root:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/compositor/backdrop.gpr && ../tests/compositor/build/backdrop/backdrop_tests'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/compositor/backdrop.gpr -u compositor_backdrop.adb compositor_image_sampling.adb vulkan_backdrop_binding.adb --level=2 --timeout=30 --checks-as-errors=on --report=all -j4'
nix-shell tests/compositor/vulkan-affine-shell.nix --run 'bash tests/compositor/test-vulkan-backdrop.sh'
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/compositor/backdrop_submission.gpr && ../tests/compositor/build/backdrop-submission/backdrop_submission_tests && alr exec -- gnatprove -P ../tests/compositor/backdrop_submission.gpr -u vulkan_submission-backdrops.adb --level=2 --timeout=30 --checks-as-errors=on --report=all -j4'
```

A private source snapshot now compiles the final submission/replay adapter and
proposed scene wiring against the copied CuBit native runtime. Evidence:
`build/backdrop-scene-preview-6yugvkj5/result.json` and
`build/backdrop-scene-preview-r2.log`. This is native component compilation,
not native execution or Desktop activation. A native-only project is provided
as `backdrop_submission_native.gpr`.

The same private snapshot passed the new `backdrop_scene_tests`, the existing
submission fault/lifetime suite, and 222 SPARK analysis results with zero
unproved/justified checks across scene, backdrop replay, geometry and sampling.
The scene snapshot remains 24,608 bytes. New kinds reuse source tickets and
existing layer geometry storage, with a typed capture operation and validated
physical source dimensions. Tests check modes, sparse damage intersected with
UI clipping, reset clips, background-before-wallpaper ordering, stale ticket
preflight before any fill, malformed layers, layer capacity and rejection.
The scene integration is now applied to the shared source tree. Both Desktop
backends and the scene adapter compiled against the native runtime under the
shared lock (`build/backdrop-scene-publish-r2.log`). The former demo text before
an application's first published buffer was removed; the existing neutral
window fill remains. This cosmetic change has compilation evidence, not a new
native screenshot/interaction run.

The refreshed native scene archive is
`build/native-scene-xagbu73y/libcubit-native-scene.a`, SHA-256
`45f2d4d8a5688b96a1c4ebaa21c237618826205d1d03d9b5c233985d0c0b11b8`.
It embeds the new audited C wallpaper recorder, preserving existing native
consumer link inputs. Its frozen runtime matches SHA-256
`14b76bb748b4a9ee41a611bac8d1b5c92090f04570945c20b06d121bf142be45`.
This archive has not yet been linked into or executed by a CuBit GPU application.

Additional actual hosted Mesa scene validation passed in private snapshot
`build/backdrop-scene-vulkan-l5wk2k10`: 96 ordered wallpaper scenes, cycling all
three modes at source density, with real source import, queue submission, fence
polling and retirement. The independent scene pixel oracle preserves its
existing bounded alpha-blend tolerance; the separate wallpaper primitive test
above checks exact bilinear pixels at varied source/output sizes. Both baseline
and wallpaper variants passed 712 submissions and 546,816 checked output pixels
with zero Vulkan validation errors. Logs are
`build/backdrop-scene-vulkan-preview-r1.log` (baseline passed; initial variant
GPR source-list error preserved) and `build/backdrop-scene-vulkan-preview-r2.log`
(corrected variant passed). The test-only hooks are now published. The checked-in
`test-backdrop-scene-pixels.sh` passed on the shared sources
(`build/backdrop-scene-pixels-final.log`): 712 actual submissions, including 96
retained wallpaper scenes, 546,816 checked pixels and zero validation errors.
The runner requires the wallpaper-specific PASS marker so the ordinary
submission suite cannot masquerade as this variant. Shared scene/policy source
bytes were also compared with the accepted 222-check proof snapshot.

## Remaining integration

Connect the retained wallpaper operation to Desktop's backend. Scene capture,
damage replay and bounded metadata are implemented and validated at component
scope; Desktop does not yet select this Vulkan path. The
service/device owner, source uploads/imports, scene-wide fallback and physical
presentation still require live integration. These tests do not enable GPU
Desktop rendering or establish NUC throughput/latency. The graphics scene
archive previously published to the peer remains a frozen, separate fixture.
