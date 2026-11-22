# Keyboard focus after client window retirement

The current native Penny no-click four-window fixture failed after closing the
fourth window, despite proven native input-batch delivery (76 fetched/delivered,
zero fallback/disable/rejection/resynchronization). The same browser and Desktop
passed with a click on a surviving titlebar before Ctrl+N (105 events delivered
after reopening). These are native CuBit QEMU functional tests, not hardware
latency measurements. Artifacts are under `tests/servo/build/perf-tmp`:

- Failure: `nix-shell.FODNZW/penny-interaction-2y5fskyn`.
- Explicit-focus comparison: `nix-shell.yyyKls/penny-interaction-oebl226i`.

Source diagnosis: `OP_SURFACE_DESTROY` cleared `focusSurface` when destroying
the focused surface, but did not select a surviving window. `queueKey` drops
input when focus is zero. The process-wide `OP_DESKTOP_BYE` had the same gap.
Internal close, minimize and dead-client reaping already restored focus.

The applied repair selects the topmost used, non-minimized window after removal
and before processing further input. Unfocused window destruction preserves
current focus. Both handlers already request a full redraw, covering active
window decoration changes. Authorization and buffer retirement are unchanged.
`Compositor_Focus` is a bounded pure SPARK selector; eligibility is collected
from the actual surface table and its ordering remains back to front.

Evidence:

- `build/focus-policy.log`: all 256 eight-slot eligibility masks pass. The first
  proof invocation selected no concrete generic instance and failed; it is not
  proof evidence.
- `build/desktop-focus-routing-proof.log`: the corrected concrete instantiation
  proves selection, absence, topmost ordering and termination. Actual extracted
  destroy/goodbye handlers pass with mocked resource operations. Removing the
  restoration or bypassing ownership checks makes those tests fail.
- `build/desktop-focus-1gucxhj2/focus-result.json`: native legacy Desktop linked
  with zero undefined symbols and Mesa-backend compilation passed. It uses a
  frozen prior Desktop dependency/runtime snapshot; `focus-inputs.json` records
  inputs. Native execution evidence follows below.
- `build/desktop-focus-ready.json` identifies the candidate and source hashes.
  `build/desktop-focus-preview.patch` records the applied shared-main edit.

The shared-main patch is applied and byte-identical to the tested candidate.
The original no-click native browser gate now passes with only Desktop changed:
four windows, original 0.3-second character pacing, close and context retirement,
Ctrl+N without a focus click, a second navigation and clean exit. All four
windows retain batching with no fallback, rejection, disabling or resync.
`tests/servo/build/focus-repair-gate-fismyob5/comparison.json` binds the identical
browser/kernel/service hashes and changed Desktop binary. Native evidence:
`tests/servo/build/perf-tmp/nix-shell.9HmKgn/penny-interaction-o4o1wrb3/focus-analysis.json`.
This verifies the repaired workload, not every focus gesture or hardware latency.
Shared staged binaries were not replaced. GPU integration remains unfinished.
