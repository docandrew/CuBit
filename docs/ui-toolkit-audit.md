# CuBit UI Toolkit Reliability Audit

## Goal

The shared CuBit UI toolkit should let an application author declare a control
once and inherit correct rendering, hit testing, focus, capture, invalidation,
and input behavior. Applications must not need private repairs for ordinary UI
behavior. Win32 is the behavioral benchmark: controls should feel immediate,
predictable, keyboard-complete, and difficult to leave in a confused state.

The Alloy visual language is intentionally CuBit's own. The benchmark concerns
control quality and interaction semantics, not copying Windows artwork or APIs.

## Non-negotiable invariants

1. A pointer transaction begins only on a physical press edge over an enabled
   control. Moving a held pointer into a control never invents a press.
2. Capture remains owned by the same stable control until release, cancellation,
   or input resynchronization, even outside its original bounds.
3. A push control activates only when press and release belong to it and the
   release is inside. Drag controls may commit continuously while captured.
4. Conditional rendering, reordering, clipping, or insertion of sibling
   controls cannot transfer capture or keyboard focus.
5. Rendering and interaction use the same geometry calculation. A visible
   thumb, divider, caret, or resize handle is exactly the object hit testing and
   dragging operate on.
6. A clipped or disabled control cannot be hit, focused, captured, or activated.
7. Ordered input edges are observed in order. Any event that can change layout
   or control membership is followed by a control-map rebuild before another
   event is dispatched.
8. Loss or discontinuity cancels capture and transient gestures, then installs
   an authoritative input snapshot. It never fabricates continuity.
9. Pointer motion with no visual or semantic transition causes no application
   repaint. Dragging invalidates the smallest declared action region.
10. Bounds, offsets, capacities, values, and arithmetic are checked and bounded.
    A small surface or exhausted table fails inertly, not by underflowing,
    aliasing another control, or accepting an invisible interaction.
11. Every pointer operation has a documented keyboard equivalent where one is
    meaningful. Focus is visible, deterministic, and cannot remain on hidden
    content.
12. Widgets own their behavior. An application may handle domain actions, but
    it must not reimplement button, scrollbar, selection, focus, or capture
    state machines.

## Common control contract

Every interactive widget is identified by a stable application-assigned
`Control_ID`. That identity is used consistently by:

* immediate-mode hot, active, capture, and focus state;
* pointer cursor selection;
* visual and action damage lookup; and
* future accessibility and automation metadata.

IDs must be unique among simultaneously rendered controls in a scope. Reusing
an ID for a different live control is a programming error and must eventually
be diagnosed deterministically rather than producing ambiguous input.

Control registration distinguishes two regions:

* **visual damage** is the control face affected by hover or depressed state;
* **action damage** is the containing view whose content can change when the
  control operates, such as a table resized by a divider.

Rendering may be clipped to damage, but registration and logical evaluation
still describe the complete current control tree.

The migration target is a hybrid retained model: stable control identity,
geometry, capture, focus, and values survive independently of paint, while the
pixel renderer remains immediate and damage-clipped. Input changes retained
state first and paint only displays the resulting state. Existing render-driven
controls remain available during the transition, but new behavior should use
retained dispatch.

## Scrollbar contract

Both axes use the same model. `minValue` and `maxValue` describe the inclusive
logical content range, `pageSize` describes the visible portion, and the
largest valid starting position is derived as `maxValue - pageSize + 1` when
content exceeds the page.

Arrow presses move one unit, track presses move one page, and a thumb drag
retains its initial grab offset. The thumb never leaves the inset track. Arrow
buttons depress and repeat only when movement in that direction is possible.
Wheel helpers clamp at both ends and never perform signed/unsigned wraparound.

## Audit status

### Tree connector polish (follow-up)

Config Inspector now uses the shared tree widget with Bluecurve node icons.
Revisit ancestor continuation lines and elbow/disclosure alignment, especially
through expanded nested branches and scrolling. Fix this in the shared tree
geometry/renderer and test clipped rows; do not add a private inspector theme
or per-application pixel corrections. This is deferred visual polish, not a
blocker for Config service work.

### Native control expansion

After the display buffer/submission lifetime foundation, add or mature shared
controls for menus (menu bars, popups, nested menus and context menus), combo
boxes, editable dropdown combos, and radio-button groups. Radio/menu drawing
and interaction primitives already exist; complete their retained behavior
rather than create parallel app-specific implementations.

Each control must inherit the common focus, capture, damage, clipping and
resynchronization contract. Include keyboard traversal/accelerators, Escape
cancellation, popup dismissal/focus restoration, disabled-item behavior,
selection/change events and narrow repaint regions. Test those behaviors in
the shared toolkit before exposing them to CCL; Workbench must simply consume
the same widgets as other native applications. Do not change geometry/theme as
part of the display-protocol migration.

### Caption double-clicks (2026-09-10)

The native compositor uses the reusable pure-SPARK `CuBit.Click_Sequences` ADT
for caption double-click to maximize/restore. A pair requires the same stable
surface identity, a complete first press/release, at most 500 ms from first
press to second press, and at most four pixels per axis of movement. Moving
outside that box cancels the candidate even if the pointer returns. Other
buttons, scrolling, a non-caption press and input resynchronization cancel it.
Completed pairs are consumed, so three presses produce one double-click, not
two. Invalid or backwards clock readings cannot satisfy a pending pair.

The caption excludes window buttons and resize grips. Maximized captions still
consume their own input; the restoration click must not leak into the client.
Maximize remains subject to the window's maximizable/fixed-size flags. Ordinary
application clicks/movement do not add clock syscalls for this recognition.

The recognizer currently uses compositor receipt time because the source report
does not carry a trusted event timestamp. Adding end-to-end timestamps remains
input-protocol work. This does not change the application's existing text-field
double-click handling: migrate that frame-count-based implementation separately
when the common toolkit receives reliable event time. No new application IPC
double-click event type is introduced by the caption feature.

Validation: hosted boundary tests and the focused state proof pass; QEMU's
`desktop-display` regression verifies caption drag, exactly one maximize from
three stationary presses, and exactly one restore from a later pair. The
`input-stream` and `desktop-protocol` gates also pass on 4-CPU KVM. Input stress
retains 6 client presents and 138 input requests. The headless harness uses
QEMU's accepted `meta_l` key name; `super_l` was rejected by the monitor.

### Corrected in the current pass

* Button and drag capture now begins only on the press edge. This fixes controls
  capturing a button that was already held when the pointer entered them.
* Shared widgets now bind interaction identity to their explicit `Control_ID`
  instead of draw order. A sibling inserted during a gesture cannot steal it.
* Focused text-field caret and selection state is no longer clamped by an
  unrelated shorter field rendered earlier in the frame.
* Vertical and horizontal scrollbars share bounded layout functions between
  renderer and state machine, including arrows, inset track, thumb, page size,
  and grab offset.
* Files and CCL Workbench use the shared vertical scrollbar. Workbench also uses
  the shared horizontal scrollbar rather than private behavior.
* Generic scroll areas now derive thumb extent and maximum start from content
  height and viewport height.
* Split panes become inert when their minimum sizes cannot fit. A drag on a
  tiny surface cannot underflow its position calculation.
* Non-motion events that dirty a view now form an event-dispatch barrier so the
  control map is rebuilt before a later queued event is hit-tested.
* The bounded control map now holds 128 controls, detects duplicate live IDs or
  exhaustion, and makes hit/damage lookup fail inertly while the app harness
  disables toolkit pointer dispatch and reports the fault.
* Horizontal sliders now share one inset-track/thumb layout between rendering
  and interaction and retain their thumb grab offset while dragging.
* Pointer resynchronization cancels capture and multi-click/selection gestures.
* Ordinary pointer motion uses control-local damage; drag controls use declared
  action damage. Whole-window repaint remains an explicit canvas policy.
* Controls now declare whether held motion can continuously change their
  value. Ordinary buttons and rows no longer repaint their containing view for
  every packet merely because they own capture; sliders, scrollbar thumbs,
  splitters, and column dividers retain continuous action damage.
* Scrollbar thumbs no longer move on the initial press while their grab offset
  is being established, avoiding an integer-rounding snap before any drag.
* Files now uses the first retained-dispatch control. Its vertical scrollbar
  changes value during input dispatch, preserves capture and grab state across
  declarative frame rebuilds, and repaints table content only when the logical
  scroll position changes. The compatibility path remains for clients not yet
  routed through the shared application harness.
* Shared retained buttons now own press, capture, cancellation, and one-shot
  activation before paint. Files refresh and row selection no longer mutate
  their models while drawing.
* Table dividers and vertical split panes now retain their exact grab offset.
  Files column resizing and the Devices pane splitter update their model during
  dispatch without snapping to the center of a wider hit target.
* Devices now uses retained buttons for refresh and tree rows, retained split
  dragging, and the retained shared scrollbar. A tree selection is committed
  before repaint, so the previously selected row cannot survive merely because
  it was drawn earlier in the frame.
* Control-map rebuild and lookup costs are proportional to the live control
  count. Repainting a small view no longer copies, clears, or scans all 128
  bounded control slots.
* Grouped selection widgets request one stable follow-up render before present.
  This prevents an earlier row, radio button, table row, or tab from retaining
  stale selected pixels when a later-rendered item changes the shared value.

### High-priority open findings

1. **Keyboard control model.** Define Tab/Shift+Tab traversal, Enter/Space
   activation, Escape cancellation, arrow navigation, accelerators, default
   buttons, and focus restoration for every shared control.
2. **Control-map render atomicity.** Invalid registration is now observable and
   fail-inert for subsequent dispatch, but registration and widget evaluation
   still happen together. Add a preflight or equivalent transaction so a
   duplicate discovered late in a faulty frame cannot let an earlier control
   commit an action in that same frame. Surface the precise offending ID in a
   developer diagnostic without exposing it to untrusted input.
3. **Time-based multi-click recognition.** The basic state package currently
   measures double-click separation in rendered frames. Idle time therefore is
   not represented. Carry a trusted monotonic event timestamp or compositor
   click sequence and use time plus spatial slop.
4. **Unified repeat timers.** Scrollbar arrow repeat still depends on an
   application-owned timer in Workbench. Timer/repeat policy belongs in the
   toolkit event harness so every app gets identical initial delay, repeat
   rate, cancellation, and disabled-at-bound behavior.
5. **Clipping audit for specialized packages.** `Lists`, `Trees`, and `Tables`
   register and evaluate some raw bounds directly. Verify nested clipping and
   ensure invisible portions cannot hit, while preserving full action damage.
6. **Value-change damage.** Separate semantic value changes from mere pointer
   motion so sliders, scrollbars, dividers, and selections present exactly once
   per changed value and do not repaint at rest.
7. **Surface arithmetic.** Consolidate checked/saturating rectangle edge,
   intersection, inflation, and allocation-size arithmetic. Audit all direct
   `x + w`, `y + h`, pixel-count, and minimum-size expressions.
8. **Text input completeness.** Add caret visibility, horizontal field scroll,
   clipboard commands, platform text composition, selection autoscroll, and
   deterministic behavior at bounded storage capacity.
9. **Composite ownership.** Trees, tables, tab panels, menus, and editors need
    explicit focus/capture behavior when rows disappear, models refresh, a menu
    closes, or a selected item becomes disabled.

## Widget acceptance matrix

Each reusable interactive widget must have automated cases for:

| Area | Required cases |
| --- | --- |
| Press/click | inside, release outside, press outside then enter, rapid edges |
| Capture/drag | cross bounds, window edge, cancellation, resynchronization |
| Disabled | pointer, wheel, focus, keyboard, programmatic state change |
| Focus | click focus, forward/reverse traversal, hide/remove, visible indicator |
| Geometry | minimum size, one-pixel boundaries, resize, clipping, integer scale |
| Values | minimum, maximum, empty range, one item, full page, overflow boundary |
| Damage | hover enter/leave, held motion, semantic change, no-change motion |
| Input loss | sequence gap, state snapshot, held button/key cancellation |

The first concrete test set covers button capture, stable identity across
reordering, independent focused text state, grouped tree-selection
stabilization, vertical/horizontal scrollbar arrows and thumb dragging, wheel
clamping, Files table resizing, Files scrollbar arrow and thumb operation
through QEMU's real input path, Files wheel scrolling, and application
liveness after those operations.

## Release gate

A widget is not considered customer-ready merely because it draws correctly.
It is ready when its common state machine is used by at least two applications,
the acceptance matrix is covered for its meaningful behaviors, small and
resized surfaces remain safe, pointer and keyboard paths agree, and the CuBit
headless test confirms that malformed or rapid input cannot terminate the app.

The Workbench, Files, and Devices applications are the initial integration
suite. App-specific interaction code discovered in those clients should be
treated as a missing toolkit abstraction unless the behavior is genuinely
domain-specific.
