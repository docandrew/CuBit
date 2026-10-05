# Cursor rendering through the compositor

The native per-output Desktop path now draws its cursor through
`Desktop_Compositor.Draw_Output` with premultiplied source-over blending.
Previously it always wrote cursor pixels directly into the CPU output buffer.
Keeping the cursor in renderer order is necessary before an asynchronous GPU
backend can own that output: CPU blending must not race queued scene writes.

The source is the existing immutable cursor atlas, addressed at the selected
cursor's first pixel with its exact width, height, stride and byte capacity.
It outlives every frame and retained view. This introduces no pixel allocation,
full-screen intermediate or new foreign interface. It reuses the existing
Mesa retained image cache and affine blend operation. Its metadata/view
capacity remains bounded by that cache. Target and source authority are still
validated by the existing compositor boundary.

The optional `Over` argument defaults to False, preserving existing opaque
client draws. The legacy backend rejects this acceleration attempt cleanly and
Desktop retains its existing CPU cursor loop. Mesa's known-quiescent failure
also permits that fallback; an uncertain result exits without ordinary reuse.
A future deferred backend must retain source leases and prohibit per-draw CPU
fallback once work is queued, as documented by `Complete_Output`.

## Evidence and limits

- Native legacy and Mesa Desktop compile/link pass.
- Hosted facade regressions pass all eleven existing text/fill/completion fault
  scenarios. These are fault-policy checks, not a real GPU test.
- SPARK facade analysis passes 912 results (217 flow, 695 prover), including
  dependencies, with no unproved or justified checks. Desktop's pointer/address
  wiring and the foreign renderer's truth remain audited boundaries.
- The native Mesa output oracle now covers 256 copy/over requests across four
  scale/origin configurations, four rotations and eight damage cases. Source
  alpha spans transparent, opaque and intermediate values. Independent integer
  blend and sampling calculations check every target pixel, including row
  padding; target/geometry mismatch must leave all pixels untouched.
- The first native run reached `COMPOSITOR-OUTPUT: PASS` and subsequent mask and
  placement checks, but hit the 90-second budget before the complete probe
  finished. It is evidence for the output oracle, not a full-suite pass.
  The subsequent 300-second-budget run passed the complete compositor probe
  and baseline Mesa pixel/triangle tests. The VM was stopped after the probe
  exited; the headless runner accepted the complete log.
- The hash-matched Mesa Desktop passed all four native interaction groups:
  primary display, scaling, arrangement and Desktop. The scaling oracle checks
  exact wallpaper restoration after cursor motion at 150% and mixed-scale seam
  mapping, alongside 125% primary reflow. This remains QEMU software rendering,
  not hardware timing.

Logs: `build/cursor-facade-proof.log`, `build/cursor-facade-proof-r2.log`,
`build/cursor-facade-native.log`, `build/cursor-facade-probe.serial.log`.
The initial proof invocation used an unsupported option spelling and failed
before analysis; the corrected invocation is the accepted proof result.

This does not enable a GPU Desktop backend or establish a latency improvement.
Wallpaper, icon/pixel paths, scene-wide admission/fallback, source upload and
real device/session ownership still need integration. Physical cursor planes,
scanout retirement and hardware timing remain separate goal requirements.

Final evidence: `build/cursor-facade-native-r3.log` (complete pixel suite),
`build/cursor-facade-desktop-r2.log` and matching `.serial.log` (Desktop).
The first attempt to start Desktop after the pixel suite stopped preboot on an
unrelated runtime formatting error; the shared source had already been fixed
before the accepted retry. No unrelated runtime source was edited here.
Accepted Desktop SHA256:
`4f48cc841d7753afe803df40edf3fbc443dccb488d21134a290b7a11a12c23fa`.
Source hashes: `build/cursor-facade-source.sha256`.
125% screenshot: `build/cursor-facade-desktop-r2.serial.settings-scale-primary-head-0.png`.
