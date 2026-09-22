# Connected desktop layout admission

The portable `CuBit.Display_Layouts` core validates one input seat's proposed
extended-desktop viewports. It is not yet called by the native compositor or
Settings; this is a tested/proved foundation for those clients, not an enabled
multi-monitor desktop or persistence implementation.

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

No new QEMU or physical-monitor claim follows from this pure-core work. The
previous three-output QEMU fixture tests discovery only. Remaining work includes
named-monitor matching, output/session generations, integration of the now-tested
[placement/recovery core](../window-placement/README.md), actual presentation,
and CCL Config publication. In particular,
rejecting a disconnected desired layout is not a reason to immediately rewrite
user placement when a monitor is temporarily unavailable behind a KVM.
