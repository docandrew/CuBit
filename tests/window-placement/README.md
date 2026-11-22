# Window placement and bounded recovery

`CuBit.Window_Placement` is a pure, shared SPARK planner. It is compiled into
the native runtime, but **the desktop does not call it yet**. These are hosted
tests and proofs, not a claim of working native multi-monitor recovery.

```
nix develop -c make -C kernel test-window-placement prove-window-placement
nix develop -c make -C kernel user_runtime
```

## Rules implemented

- A preference holds a named home and work-area-local logical bounds, including
  decorations. Automatic planning has no write access to preferences or Config.
- The input list contains only currently ready, authorized work areas, already
  resolved into the effective layout. A reserved desired rectangle is not a
  ready work area. Work areas exclude taskbars and other reserved desktop UI.
- A suitable home wins. If absent, the caller's explicit grace policy determines
  whether to wait or find a provisional fallback. If home is ready but cannot
  fit the window, further discovery delay cannot repair that geometry: try a
  fallback immediately, even under the wait policy.
- Prefer the previous effective display for fallback when it still fits;
  otherwise choose the smallest stable named ID among suitable areas. Array
  enumeration order is not preference. Duplicate IDs produce an ambiguity error,
  including duplicates unrelated to the requested home.
- Clamp the requested local position, retaining width and height. The entire
  decorated rectangle fits within one work area, stronger than only recovering
  the title bar. Oversized windows get a typed `No_Suitable_Work_Area` result:
  silently resizing below app minimums or cropping controls is not a solution.
- Pointer capture and modal interaction return `Deferred_Interaction` with no
  proposed move. The owner must replan when interaction ends.
- A recovery episode has one monotonic deadline. Repeated `Begin_Recovery`
  calls leave an active episode unchanged, even after expiry. Explicit completion
  ends it. Deadline arithmetic saturates rather than wrapping at clock exhaustion.
  No polling, sleeping, heap allocation, hardware I/O, or focus changes occur.

`Target.Available` means a valid proposal exists against the supplied snapshot;
it does **not** mean a window has been shown, a physical panel is displaying
pixels, or that a previous placement remains usable. Failure/deferred results
contain no stale rectangle disguised as a live destination.

## Integration boundary

The desktop owner must obtain a coherent authorized ready-output/work-area
snapshot, call the planner, and apply its proposal within the same serialized
state transition. The array index is snapshot-local, never durable identity.
If this becomes asynchronous, the apply boundary must check **window incarnation,
intent revision, topology/session generation, and current interaction state**;
discard and replan stale proposals. This module does not provide that registry
or claim to prove its concurrency behavior.

A completed explicit user move or "Bring here" replaces the home/local preference
in the owner, even while a monitor is missing. Automatic fallback never does so.
Replanning then uses the new intent, so a reconnect cannot restore an old home.
There is no delayed restoration queue inside the planner. Tests replace the
preference to exercise this behavior; live user-move/revision handling is future
integration work, not an already implemented desktop feature.

Recovery episodes belong to an output/topology recovery owner, not each window
or repaint. Preserve the episode through unsuccessful probes and flapping; only
complete it on settled recovery or explicit cancellation/reconfiguration. The
caller supplies a monotonic clock and chosen finite grace duration. Grace expiry
selects `Allow_Temporary_Fallback`; it never authorizes device access, renews an
output session, or releases DMA resources. Explicit user removal may bypass grace
without implying that hardware is quiescent. No product timeout is chosen here.

Geometry is already logical: derive rotated/scaled output bounds through
`Display_Geometry`, then subtract reserved desktop UI. This core does not resolve
EDIDs, choose modes, repair a disconnected surviving layout, enforce authority,
group modal windows, implement workspaces, or persist configuration. Those are
separate responsibilities, not silently assumed features of a valid rectangle.
An empty ready list or insufficient work area requires trusted recovery UI (or
authorized remote recovery), not pretending the old window remains reachable.

## Evidence

The hosted suite passes **231,563** placement/timer cases, plus boundary assertions:
exhaustive small work areas/window sizes/local offsets, signed/numeric extremes,
late preferred display, manual reassignment before reconnect, capture/modal
deferral, portrait-sized/scaled work areas, too-small home, no ready output,
duplicate identities, enumeration reversal, fallback stability, repeated probe
events, deadline expiry and overflow-safe saturation. The clamp oracle uses
independent conditional arithmetic rather than calling `Fit` or `Contained`.

GNATprove discharges **47 proof diagnostics**, without skips, `Assume`, or
SPARK-Off sections. It proves numeric/index/discriminant safety, initialization,
termination, and the functional postconditions:

- Successful plans reference an entry in the supplied ready snapshot, preserve
  requested dimensions and contain the full rectangle in that work area.
- Home/fallback reasons correctly distinguish the destination's named identity.
- Non-idle interaction returns a deferred result without a destination.
- Starting an already-active episode preserves it exactly; a new deadline is
  not before its start. Completion deactivates the episode.

Optimal fallback ordering, classification of every rejected input, and the
integrated reconnect UX are **not** claimed as proved. Selection behavior has
regression tests; integrated reconnect UX still needs registry/compositor tests.
`check-proof.py` requires nonempty full-SPARK results and successful postcondition
proofs. Runtime builds retain their normal optimized, no-assertion settings;
hosted test assertions are not enabled in the kernel or native runtime.
The native build passes, and object inspection confirms no emitted `Safe_Plan`
Ghost predicate or assertion-handler references. Existing geometry (191,488
pixel round trips) and layout-admission (6572 arrangements) regressions also pass.
