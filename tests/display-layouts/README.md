# Connected desktop layout admission

The portable `CuBit.Display_Layouts` core validates one input seat's proposed
extended-desktop viewports. The native compositor calls it at startup and when
Settings applies a changed arrangement. Persistence and hotplug reconciliation
remain separate work.

```
nix develop -c make -C kernel test-display-layouts prove-display-layouts
nix develop -c make -C kernel test-display-geometry prove-display-geometry
nix develop -c make -C kernel user_runtime
```

## Model

- Up to 16 viewports in this initial bounded model. This budget is not a virtio
  protocol limit or a claim that hardware supports all admitted dimensions.
- A distinct `Named_Display_ID` type carries a configuration identity, not a
  capability, physical connector index or PID. Name matching, catalog membership
  and authorization belong to the future registry/admission boundary.
- Every used entry has the shared physical mode, rational scale, rotation and
  signed origin. Logical bounds come from `CuBit.Display_Geometry`.
- Duplicate IDs and intersecting interiors are rejected. Positive-length shared
  edges connect viewports; corner contact and gaps do not. Connected rings with
  empty centers are allowed. Explicit mirrors must become one logical viewport
  before this validator; it does not infer mirroring from overlap.
- Empty layouts require explicit `Permit_Headless`. That parameter is a policy
  input, not a grant of authority. Nonempty headless-permitted layouts still get
  all the ordinary validation.

Results distinguish accepted, missing interactive output, duplicate display,
overlap, and disconnected layout. `First`/`Second` identify offending entries;
zero means not applicable. A disconnected result retains the discovered root
component, so Settings can identify the isolated entries. No input is modified,
no windows are moved, and no saved preferences are overwritten.

Connectivity is constructed in bounded breadth passes. Each reached entry gets
a parent and depth. The first input entry is only the connection-tree root:
it is **not** automatically the preferred display or the target for app windows.
There is no heap allocation, device I/O, Config access or polling. This operation
runs when validating configuration, not during input delivery or repaint.

## Evidence and scope

GNATprove discharges 50 proof diagnostics, including the admission postcondition,
with no skipped analysis, `Assume`, or SPARK-Off sections. The postcondition
establishes that an accepted nonempty layout has distinct IDs, disjoint viewport
interiors, and a complete connecting tree: every non-root has a touching parent
with strictly smaller positive depth. Finite descent therefore leads to the
single root rather than a cycle or separate component. Bounds safety,
initialization and termination are also checked. Empty acceptance requires the
explicit headless policy. `check-proof.py` rejects vacuous proof success.

The proof is soundness of **acceptance**, not a proved converse that every valid
connected input is accepted. Rejection completeness and diagnostics are checked
by hosted tests. The test suite covers 6572 arrangements, including every ordered
four-cell arrangement on a 3x3 grid against an independent Manhattan-adjacency
oracle, a reversed 16-monitor chain requiring all breadth passes, removal of a
bridging display, a connected ring with a hole, and mixed-DPI/rotated layouts.
Accepted trees are independently walked to their root in the tests.

The Desktop-owned `Select_Primary` helper chooses only from the supplied ready
layout, preserves a usable current primary by identity rather than array index,
and uses the saved preferred ID or a deterministic fallback when necessary.
Explicit Apply can select the preference; normal late discovery does not move
the taskbar back. Its proved postcondition establishes a valid ready entry for
every nonempty input, and no primary for an empty input. The policy ordering is
regression-tested in 23,040 cases covering all four-display permutations and
readiness subsets, missing preferences, stale previous indices and both update
policies. This does not prove hardware readiness, atomic application or native
taskbar relocation. The display service does not call or own this policy.

The native freestanding runtime builds successfully. Inspection of its
`cubit-display_layouts.o` confirms the Ghost predicates (`Pairwise_Valid`,
`Valid_Tree`, `Complete_Tree`) and assertion handlers are not emitted. Hosted
assertions check the tests; native build settings are unchanged.

These pure-core proofs do not establish correctness of physical monitors or
native presentation. The two-output native regression below exercises actual
presentation separately; the three-output fixture tests discovery only.
Remaining work includes stable named-monitor matching, hotplug integration of
the [placement/recovery core](../window-placement/README.md), and CCL Config
publication. In particular,
rejecting a disconnected desired layout is not a reason to immediately rewrite
user placement when a monitor is temporarily unavailable behind a KVM.

## Native arrangement editor

Settings -> Displays now draws the actual active outputs as draggable tiles.
The primary has a white bar. Dragging changes a pending preview; Apply commits
it, Revert restores the last applied preview, and Escape cancels a tile drag.
Selecting a tile and choosing **Make primary** moves the white bar in the pending
preview. Apply commits that choice along with the arrangement; Revert restores
both. The taskbar and subsequent app launches use the new primary. Existing
windows stay on their monitors, and maximized windows resize to the changed work
areas without losing their restore positions. Resolutions, rotation and driver
state do not change. Categories are selected in a left-hand list, including
Up/Down navigation when the category list has keyboard focus.
The shared `CuBit.Display_Arrangement` core considers touching edges and keeps
the nearest candidate admitted by `Display_Layouts.Validate`. Nearby parallel
edges align within 32 logical units. A preview pixel represents several logical
pixels, so arbitrary offsets are quantized to its current diagram scale.

The native renderer normalizes the scene's minimum X/Y to zero: a display can
still be left of or above the primary; this simply gives the primary a positive
origin in private pixel storage. Damage splitting, pointer confinement and
output-local transfer pitches use the existing geometry. Settings Apply
preserves each window's monitor using its title-bar anchor and keeps titles
reachable; minimized/maximized restore coordinates move too.

No scanout buffer or grant is replaced. Existing in-flight buffers stay immutable
until completion, then receive a full repaint using the new scene coordinates.
The private scene/drag buffers reserve capacity once at setup (up to the existing
16 MiB budget each, bounded further by the two displays' summed dimensions).
If that reservation fails, startup can use the original scene capacity; larger
arrangements are rejected without changing the live layout. Drag/Apply do not
allocate, modeset or introduce another pixel-copy stage. The rectangular scene
budget still limits extreme offsets/high resolutions; native composition still
admits at most two outputs despite the portable model's 16-entry budget.

The snapping helper has 3,812 hosted cases including signed extreme coordinates,
all four relative sides, mixed scale/orientation, invalid indices and duplicate
IDs. Tests check accepted connectivity, normalization and preservation of
identity/mode/scale/orientation. GNATprove checks arithmetic, indexing, variant
access and rejection-preserves-input behavior, with no assumptions or skipped
analysis. Functional acceptance soundness comes from the existing layout
validator; this is not a proof of the entire native compositor or SMP rendering.

```sh
nix develop -c make -C kernel test-display-layouts prove-display-arrangement desktop
CUBIT_TEST_ARRANGEMENT=1 nix develop -c tests/headless/run.sh \
  --test desktop-dual-output --accel kvm --cpus 4 --timeout 120 \
  --serial /tmp/cubit-arrangement.log --keep-logs
nix develop -c make -C kernel run-desktop-dual
```

The arrangement fixture uses real guest mouse input and both QEMU scanouts; it
does not mutate guest memory or substitute a hosted UI. Saved CCL Config layouts,
stable monitor identities, rotation and runtime mode
switching are deliberately not enabled by this editor yet.

Native QEMU/KVM regression passed with two 1024x768 outputs and with a 1024x768
primary plus 1280x720 secondary (`CUBIT_TEST_MIXED_OUTPUTS=1`). It exercises
above/left/below/offset/restore arrangements, unchanged scanout sizes, window and
taskbar relocation, launching/maximizing a client on a primary with nonzero Y,
vertical pointer crossing and old-cursor restoration. This is functional evidence,
not a latency benchmark or physical-hardware validation. The mixed case also
exposed and now regression-tests the heap tracking-headroom fix described in
[`tests/heap-admission`](../heap-admission/README.md).

Add `CUBIT_TEST_PRIMARY=1` alongside `CUBIT_TEST_ARRANGEMENT=1` and allow a
180-second timeout to exercise primary preview/Revert/Apply, taskbar migration,
new-client placement, maximizing on the new primary, and reclaiming the taskbar
area when changing back. The primary chooser's existing SPARK postcondition
proves selection of a valid ready display; native window resizing, presentation
and Settings interactions are regression-tested, not covered by that proof.
The primary-switching fixture passed with the mixed-resolution QEMU/KVM pair.

## Per-output logical scaling

The selected display has **Scale −/+** controls for 100/125/150/175/200 percent.
Apply commits scale and arrangement together; Revert discards both. Presets that
leave less than 800x480 logical units are rejected without modifying the pending
layout, keeping the initial native Settings and applications reachable. Scaling
can change edge adjacency, so the shared arrangement helper reattaches the changed
viewport through the normal connectivity validator. Physical modes are unchanged.

The scene, work areas, wallpaper placement and pointer coordinates are logical;
damage and transfer buffers remain physical/output-local. Unit scale keeps its
bulk row-copy path. Scaled outputs use center-sampled nearest-neighbor fallback
for existing logical-pixel client surfaces, with column mappings computed once
per dirty rectangle and direct writes into the existing transfer buffer. Row
addressing respects the byte pitch, including padding. No new intermediate image
or grant is introduced; buffers already submitted remain immutable.

This is **not native-density application rendering**. Text in a scaled legacy
surface is enlarged after rasterization. The existing client information response
still describes logical dimensions with a unit-density buffer contract. A future
per-surface scale/configure revision must distinguish logical size, pixel size and
raster density, trigger toolkit/TrueType rerasterization, and reject stale buffer
generations before advertising native mixed-DPI text. Rotation is not enabled.

There are 40 hosted scale/side/selection combinations plus empty/single/invalid
selection cases. The scale helper's run-time checks and rejection-preserves-input
postcondition are SPARK-proved; geometry mapping uses the existing proved core.
The raw-address compositor sampler and native UI are **not** SPARK-proved.
`CUBIT_TEST_SCALING=1` with the arrangement/primary/mixed-output flags exercises
125/150%, rejection of an unusably small workspace, scaled cursor placement and
pixel-exact cursor cleanup, restoring 100%, primary reflow, and a mixed-scale seam.

Persistence remains gated on stable monitor identification. Discovery explicitly
publishes endpoint-lifetime catalog IDs, not durable monitor names. Next: carry
monitor/connector matching metadata through authorized discovery, resolve named
CCL Config profiles against it, validate the whole revision, and keep missing
monitor preferences intact. Do not persist current array indices as identities.
