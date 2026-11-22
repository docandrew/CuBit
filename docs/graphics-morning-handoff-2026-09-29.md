# Graphics morning handoff — 2026-09-29 UTC

## State

Intel hardware acceleration is not working or verified. Latest NUC feedback
is `native DC transition transition-failed`; obtain the accompanying
`stage=... exit=...` before changing the transition. A/B inherited power
references do not establish a successful DC-Off transition. Keep submission
gated and firmware scanout retained.

The shared build lock was still unavailable at this follow-up. No new image
was built, no USB device was written, and no commit or push was made.
The previous private image in `.build-workspaces/intel-dc-init-xqker_xg`
has not been modified. It does not contain the later batch-start/EU changes.

## Completed source work awaiting packaging

- Fixed-address private-VM batch probe, with ordered initialization barriers
  and separate batch-result/HWSP completion checks. No hardware execution claim.
- Stable EU-fuse sampling in the driver and ADL-N topology translation for
  Mesa. Adapter updates derived counts, pixel-pipe counts and L3 banks itself.
- Exhaustive hosted topology test and CuBit object compilation passed in
  `tests/mesa-anv/target/topology-test.job7CO`. This is not native discovery.
- DC transition failure-stage regression passed (session79587); it exercises
  callback simulations, not the physical power transition.

Mesa must apply runtime workarounds after measured topology, including the
Gfx12.0 <=32-EU geometry-URB limit. Scratch/workaround finalization and native
device discovery remain incomplete. Do not expose an offline PCI-default
device as if it were a working render adapter.

## Software rendering and presentation

Re-read `docs/mesa-software-native-boundary.md` and
`tests/mesa-software/native-mesa-window.c` during this follow-up. Existing
recorded native QEMU evidence covers software OpenGL cube rendering, nine
buffer replacements, repeated context lifetime, and Escape exit. These tests
were not rerun during this follow-up. Linux lavapipe tests are separate and
must not be described as native CuBit or Intel acceleration.

Current CPU contract: finish rendering before attachment; never modify the
currently attached buffer; reuse the previous buffer only after unambiguous
successful replacement. IPC carries control and grant metadata, not pixels.
Desktop still CPU-composes into display buffers. An attach/present reply is
not a hardware scanout fence and cannot authorize reuse while an asynchronous
GPU consumer might still be reading.

Next software-pipeline milestone: carry explicit buffer retirement through
the app/desktop/display boundary before allowing asynchronous GPU composition.
Cover delayed completion, failed/ambiguous replacement, client exit, resize,
and device loss. Preserve the existing synchronous CPU contract until that
protocol exists; do not add a pixel-copy IPC fallback or false completion.

## Resume order

1. Coordinate a brief snapshot window, then create `intel-batch-eu` with
   `tools/build-workspace.py create ... --seed-live` under Nix. The helper
   acquires the shared lock; subsequent builds use the private lock.
2. Build runtime, Intel driver and kernel privately; preserve seeded-artifact
   provenance. Run the UEFI USB/hub QEMU desktop regression before handoff.
   QEMU cannot establish Intel hardware execution.
3. Obtain the NUC DC stage/exit detail, resolve that failure, then test the
   private batch result on hardware before any rendering claim.
4. Resolve shared devmgr/procmgr/runtime ownership before adding a distinct
   render-query endpoint. Existing GPU role17 is the virtio display role,
   not authority for Mesa to access raw Intel MMIO or DMA.

The overnight follow-up stops after this handoff. The larger graphics goal
remains incomplete and blocked on integration/hardware feedback.
