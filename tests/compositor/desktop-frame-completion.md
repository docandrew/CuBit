# Desktop renderer completion and fresh damage

Desktop's output pump no longer unconditionally declares rendering complete
immediately after its drawing calls. It finishes the renderer's output scope,
then treats Complete, Pending and Unsafe separately. Pending returns to the
event loop with the writer held; later passes only poll that output. It neither
redraws nor resubmits the writer. Complete transitions the existing proved pool
into a ready frame and publishes it through the existing authenticated Display
completion path. Unsafe exits without releasing uncertain renderer resources.

The legacy CPU and Mesa softpipe backends still complete synchronously. Mesa
reports Unsafe when its retained cache cannot establish quiescence. The facade
accepts the target address, output selector and whether the call is a poll;
these software implementations do not mutate renderer state. The facade currently declares
read-only Engine access. A GPU backend must extend that contract to describe its
actual mutable submission state, retain all source/target leases and never wait
inside this call.
This change does not enable a Vulkan Desktop backend by itself.

## Damage ownership

Each output has incoming Damage and a separate Frame_Damage snapshot. The new
SPARK `Compositor_Damage.Capture` moves incoming metadata into an empty snapshot
and clears only incoming metadata. It rejects an empty input or an occupied
snapshot without changing either. The frame's message uses the snapshot's
bounds. Successful presentation submission clears only that snapshot, preserving
new input/layout/client damage accumulated while the renderer was pending.

This adds one bounded eight-rectangle metadata state per output, no pixel
allocation or image copy. Existing per-target repaint history still tracks the
pixels needed when a target is reused. The same snapshot path is used by the
software fallback; there is no separate untested capture implementation.

A pending renderer prevents the event loop's indefinite idle wait. Until GPU
completion can wake the kernel activity wait directly, the pump uses a bounded
one-millisecond polling fallback; input may wake it earlier. That is a polling
interval, not a measured response-time or 1 ms keypress-to-photon guarantee.
The current software backends never need that pending wake path.

## Verification and remaining boundaries

- Capture policy test: 1,000 held frames with 32,000 fresh updates, sparse-list
  overload and layout replacement. Captured frames cannot be overwritten;
  submitting one never clears subsequent damage. Existing pixel-coverage damage
  tests also pass.
- Damage SPARK proof: 28 results, 11 flow and 17 prover, zero unproved or
  justified checks, including existing damage operations.
- Both legacy and Mesa Desktop native compilation/link pass.
- Completion facade tests cover clean/known-quiescent fallback and uncertain
  fill/text/cache retirement in 11 scenarios. Both finish and poll observations
  add no drawing calls and release no views.
- Completion facade proof: 912 results (217 flow, 695 prover), including
  dependencies; zero unproved or justified checks. The first attempt correctly
  rejected an In_Out contract for an operation whose current implementations
  only read renderer state; the final contract states Input.
- The first hash-matched native regression failed relocated-primary
  double-click recognition during long softpipe redraws. A deterministic
  processing-time negative control reproduced the timing mechanism. Pointer
  acquisition timestamps now survive driver queues/retries and feed the existing
  click policy. The unchanged native regression subsequently PASSed primary,
  mixed 125/150% scaling, arrangement and Desktop interaction checks.
  Accepted Mesa Desktop SHA256:
  `9381afec96993f61b2dc6d82ca736b123c69447bd23962f2e1afd92f00f67f3c`.
  Evidence: `build/pointer-time-native-r3.log` and matching `.serial.log`.
  This is native CuBit under QEMU TCG, not a hardware performance benchmark.
  The earlier stale-staged-binary run remains excluded from acceptance.
- Native instrumented Desktop completion-delay and unsafe-result fixtures pass
  (details below). Normal software rendering remains synchronous; real GPU
  completion and device-loss integration remain required.

The production `main.adb` event-loop wiring and backend/driver completion facts
remain audited integration boundaries. The damage and buffer-pool contracts are
proved; that does not establish that a foreign renderer's completion report is
true. Native GPU input-to-presentation, device loss, actual output retirement
and physical timing remain required goal work.

Logs: `build/damage-capture-policy.log`, `build/desktop-frame-native.log`,
`build/desktop-frame-backend-r2.log`, and `build/desktop-frame-dual-r2.log`.
Source hashes: `build/desktop-frame-source.sha256`.

## Native completion fault fixture

`build-desktop-completion-fixture.py` creates an extending GPR project and an
instrumented copy of `main.adb` in a unique build directory. It changes exactly
two completion call sites, inserts local test declarations, and adds a
post-publication assertion. Production source and its backend selection remain
unchanged. Anchor counts must match exactly or generation fails. Build records
include input hashes and generated-source/binary hashes.

The delayed fixture withholds the real software renderer's completed result
for at least three event-loop polls and ten milliseconds on the first eight
frames per output. It checks writer ticket/address and captured damage remain
unchanged. Synthetic fresh damage enters the production incoming-damage and
repaint states while the first frame is held. Assertions verify publication
preserves that damage and that the next submitted frame covers it. Normal
Desktop interaction, primary output, arrangement and mixed-DPI observers are
run unchanged. The synthetic damage is not a measurement of real input latency.

The unsafe variant returns Unsafe after holding the first frame. Its oracle
requires Desktop's uncertain-completion exit message and rejects any successful
publication marker. This checks the event-loop branch; it cannot prove that
process teardown safely retains real GPU resources. That requires native driver
ownership and device-loss tests when the GPU backend is connected.

Run builds and boots in Nix under `coordination/build.lock`, for example:

```sh
flock --exclusive coordination/build.lock nix develop -c python3 \
  tests/compositor/build-desktop-completion-fixture.py EXISTING_MESA_BUILD
# Use the unique fixture directory printed by the builder:
flock --exclusive coordination/build.lock nix develop -c python3 \
  tests/compositor/run-desktop-completion-fixture.py FIXTURE_DIRECTORY
```

Pass `--unsafe` to the builder for the second variant. The runner temporarily
stages the fixture, restores the exact previous Desktop in a finally block,
and records restoration hashes. Unsafe intentionally makes the normal Desktop
readiness test fail; acceptance additionally requires the precise fault oracle.
The log oracle has two positive cases and eight rejection controls.

Accepted delayed run: `build/desktop-completion-delayed.log` and `.serial.log`,
fixture `build/desktop-completion-8lcuk0il`, binary SHA256
`395eaf3a6f5453f89a3d46bcbdefa0abb5b987f813b6f61c287d83776d8ead10`.
Both outputs passed retained-writer/fresh-damage checks (three and eight polls),
then next-frame consumption checks. All four native interaction groups passed.
The staged Desktop was restored and compared byte for byte after the VM exited.
These are native CuBit software-rendering tests under QEMU, not hardware GPU or
physical-display performance evidence.

Accepted unsafe run: `build/desktop-completion-_hmrc2f2/boot-result.json`,
`boot.log` and `serial.log`. Binary SHA256
`87aa9d15ed20e2ac8c991b1a39c40bfcc667fd26dea778febfe805507856203b`.
The first held output returned Unsafe and Desktop logged its uncertain-writer
exit. No frame-publication or release marker occurred. Normal Desktop readiness
failed as expected; the separate fault oracle passed. The saved production
binary was restored with SHA256
`9381afec96993f61b2dc6d82ca736b123c69447bd23962f2e1afd92f00f67f3c`.


## Explicit output begin and deferral

Selected renderers now receive Begin_Output before Desktop consumes incoming
damage or repair metadata. Started admits the output drawing scope; Deferred
returns to the event loop without consuming either damage set or changing the
writer. Pending damage on a writable selected-renderer output also activates
the bounded one-millisecond retry, avoiding indefinite idle while begin is
deferred. Start_Unsafe exits with the writer retained. While rendering is pending,
Desktop calls only Complete_Output with Poll=True, never Begin_Output again.
Legacy CPU and Mesa softpipe begin synchronously; Mesa rejects uncertain cache
ownership. This introduces no allocation, copy, queue or new foreign call.

The completion fixture also defers the first three starts per output, asserting
exact preservation of damage, frame snapshot, repaint state, pool and target
address. Every completion must have one open begin scope; starting during held
completion fails. Native integration and selected SPARK evidence are recorded
with the output-begin publication manifest when accepted. The unproved Desktop
event loop, mapping authority and foreign completion truth remain boundaries.
This hook alone does not bind private Vulkan images to Desktop CPU addresses or
enable client GPU imports, Display image sharing or physical scanout.
