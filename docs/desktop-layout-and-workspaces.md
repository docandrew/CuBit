# Named displays, desktop layouts and workspaces

Design proposal, 2026-09-21. This captures the next desktop state model; it does
not claim implemented persistence or virtual desktops. Native composition now
supports two mixed-resolution outputs with logical scaling and no rotation,
including session-only origin, scale and primary-display changes from Settings.
Current application pixels are sampled for scaled output, not rerasterized at
native density; the scale-aware surface protocol remains follow-up work.
The existing [display architecture](display-outputs-and-scaling.md) owns the
hardware/presentation boundaries. The shared geometry core already implements
all four rotations, rational scale, signed origins and clipped damage/input
mapping; see [geometry evidence](../tests/display-geometry/README.md).

The first connected-layout admission core is now implemented in
`CuBit.Display_Layouts`. Its accepted-layout postcondition proves unique named
IDs, non-overlapping logical viewports and a complete edge-connected tree.
Hosted tests cover 6572 arrangements; the native Desktop validates both initial
and Settings-applied arrangements through this core. See [layout admission evidence](../tests/display-layouts/README.md).
The initial pure window-placement/recovery core is also implemented in
`CuBit.Window_Placement`: unchanged desired homes, safe full-window fitting,
deterministic temporary fallback, interaction deferral and non-extending recovery
deadlines. See [placement evidence](../tests/window-placement/README.md) for the
proved properties and tested behavior. The placement/recovery core is not yet
wired into hotplug; current arrangement Apply translates windows with their
title-bar monitor and keeps titles reachable. The owner-local output registry and placement-ticket
freshness gate are now implemented too; the display service uses the registry
for boot-output presentation bindings, not window-placement application. See
[registry evidence](../tests/output-registry/README.md). Identity matching,
authenticated live adapters and serialized application of checked plans,
workspaces and Config persistence remain design work below.

## User-facing rules

- Applications use the shared toolkit/surface contract, not monitor-specific
  window-manager logic. Custom renderers obey that same contract.
- Settings shows named displays that can be dragged into position, identified
  on the physical screen, rotated, scaled and assigned a supported mode.
- A missing or slow monitor must not erase the saved arrangement or preferred
  home of an application's windows. Connection order is not identity.
- Optimize ordinary boot, wake and KVM switching for monitors returning to their
  known roles. A transient lack of readiness is not an instruction to rearrange
  the desktop. Genuine removal still has a bounded, usable recovery path.
- Every visible, decorated window has a usable move/recovery affordance on an
  available output. A few visible pixels are not sufficient.
- Virtual desktops (called desktop workspaces below) are independent of
  monitor hardware, process lifetime, filesystem workspaces and security domains.
- Config, described using CCL, is the configuration mechanism. No parallel
  monitor dotfiles, toolkit-specific preferences or ambient user-account policy.

## Objects and lifetime

Use bounded typed collections and enums, not anonymous global arrays whose
indices double as identities. Table capacity is an implementation budget; it
is not a wire identifier or a physical limit of the model.

| Object | Durable meaning / runtime state |
| --- | --- |
| Named display | Stable configuration ID, human label, matching preferences; survives disconnects |
| Live output | Adapter/connector association, capabilities, native mode and generation; not durable authority |
| Layout profile | Named desired arrangement of displays with mode preference, scale, rotation and signed position |
| Desktop workspace | Stable ID/name/order; groups windows independently of attached displays |
| Window placement preference | App-scoped window role, workspace, named home display, logical bounds/anchor and window state |
| Live window | Authorized surface/session, incarnation, current placement, current user-move revision and focus state |
| Effective layout | Currently realizable outputs/positions, topology generation and deviations from the desired profile |

Persistent state contains names and preferences, never live endpoint handles,
surface IDs, frame tokens or generation values that would revive old authority.
The desktop owns live placements and validates application requests. Config owns
desired settings; drivers own actual hardware state. A preference is not proof
that an output exists or that hardware accepted a mode.

### Naming and matching monitors

A named display is a first-class configuration object, for example `desk-main`
with a UI label of "Main monitor". Its stable ID survives label changes and
usually cable/port changes. CCL references resolve to this object rather than
to enumeration index zero or the first adapter to initialize.

Match using available manufacturer/product/serial identity evidence, with an
explicit connector/dock association when required. Raw EDID hashes can be useful
evidence but should not be the sole identity: firmware changes can change bytes.
Serials can be absent, duplicated or false. Two identical monitors without
distinct identities require a saved connector association or user pairing.
Do not guess and silently swap their saved window placements. Settings should
show unresolved/ambiguous matches and offer "Identify" and "Remember as ...".

Unknown monitors receive provisional identities until paired. A fingerprint is
a preference hint, not authenticated hardware identity and not authorization to
capture pixels, inject input or configure another output. Named IDs themselves
are not capabilities. Resolution of names remains authority-filtered.

## Resolution, scale, rotation and position are distinct

### Primary display belongs to the Desktop

The **Desktop**, not display.svc or a GPU driver, selects one effective primary
display per interactive seat. The display service exposes authorized output
inventory, modes, readiness and presentation sessions. It neither knows where
the taskbar lives nor designates a system-wide primary output.

The primary hosts the taskbar, Apps menu and unparented desktop/recovery dialogs.
App-owned dialogs normally follow their parent window. A window's maximize work
area belongs to its own output; only the primary reserves taskbar space. Primary
does not mean coordinate origin, boot framebuffer, first enumerated output,
preferred render adapter, extra authority or a presentation bottleneck.

The desired CCL Config profile names a preferred primary. The Desktop separately
maintains the effective primary among its enabled, ready viewports:

- With no current usable primary, choose the preferred display if ready;
  otherwise choose a deterministic fallback by stable named-display ID.
- Keep a usable effective primary through discovery-order changes and late
  arrivals. Returning preferred hardware does not automatically relocate the
  taskbar, steal focus or move windows; an explicit layout Apply can select it.
- If the primary is lost or disabled, choose a surviving usable output promptly
  so recovery controls do not wait behind the window-placement grace period.
  This never overwrites the saved preference.
- Interactive configuration must retain a usable primary. Headless operation
  is explicitly allowed and has no effective primary; total hardware loss is a
  degraded state, not evidence that a physical monitor remains visible.

"Always shown" means keeping desktop controls on a usable output whenever one
exists. It cannot guarantee panel power, cable connectivity or photons. Applying
this policy belongs to the Desktop's serialized topology/layout transition;
selection from an old snapshot alone does not authorize presentation or prove
that the output remains ready. Taskbar capture/modal interactions must be safely
completed or canceled as part of relocation.

`CuBit.Display_Layouts.Select_Primary` is a pure shared helper for that Desktop
policy. It does not run in display.svc. The native Desktop now calls it during
initial layout admission and confines the taskbar and default application
placement to that output. Runtime primary changes are not implemented yet. Its
input contains ready viewports, not merely detected or desired monitors; identity
matching and authenticated readiness remain the caller's responsibility.

### Geometry

Each placement records these independently:

- Native mode: pixel dimensions and supported refresh preference.
- UI scale: a checked rational factor, not host-window zoom or mouse gain.
- Content rotation enum: unrotated, clockwise 90, 180 or 270 degrees.
- Desktop position: signed logical origin, allowing displays left of or above
  the chosen anchor display. The anchor is a coordinate convention, not first
  detection order or an authority-bearing "primary" monitor.

At 90/270 degrees the upright pixel extents swap before conversion to logical
units. The native scanout mode does not necessarily swap its width and height:
the compositor rotates content into its storage layout. For example, a native
1920x1080 output at scale 1 and rotation 90 occupies 1080x1920 logical units.
At 180 degrees extents are unchanged but content and input orientation change.

Rotation applies to content, damage, cursor placement/restoration and absolute
input mapping together. Relative pointer motion stays in desktop coordinates;
do not rotate it independently in a device driver or feed rounded hit-test
positions back into the motion accumulator. Font scale remains a separate
choice. Rotation must not add glyph color fringes: grayscale coverage is retained.

## Layout admission: a connected arrangement

For one interactive input seat, enabled extended-desktop viewports form a
connected adjacency graph. This means every viewport is reachable through a
chain of neighbors, **not** that every pair must touch.

- Rectangles use logical, half-open edges with centralized rounding.
- Adjacency requires a shared edge segment of positive length. Corner-only
  contact is not traversable adjacency.
- Extended viewports do not overlap. An explicit mirror group is one logical
  viewport presented by multiple outputs, not an overlap exception guessed from
  coincident coordinates. Mirror members must negotiate a compatible logical
  viewport; mode/scale reconciliation is explicit.
- Holes in an otherwise connected arrangement are allowed; monitors need not
  tile one giant rectangle. Pointer motion cannot end in a hole. Define swept
  motion across valid shared edges, rather than teleporting to a nearest monitor.
- Settings snaps touching edges and rejects a disconnected candidate before
  Apply, explaining which group is isolated.
- Intentional separate seats and authorized virtual/headless destinations do
  not have to touch a local seat's monitors. An empty/headless layout is a
  separate allowed policy, not an accidental failed interactive arrangement.

On loss of a bridging display, the remaining effective layout may need temporary
repositioning to remain connected. That is a recovery layout, **not** a rewrite
of the desired profile. Use a deterministic surviving anchor and placement order,
show the deviation, and preserve the original positions for reconnection.

## Boot, hotplug and the three kinds of placement

Keep three distinct records:

1. **Desired:** saved display/workspace/window preferences.
2. **Observed:** outputs actually discovered and ready, with live generations.
3. **Effective:** the safe arrangement/windows currently being presented.

At boot, prepare the selected profile before restoring windows. Track expected
displays as pending rather than declaring the first ready monitor to be the
whole desktop. Use a bounded topology-settle policy with a hard deadline; each
new device must not restart the deadline indefinitely. The timeout is a product
choice still to measure, not a hard-coded promise in this design.

The first usable screen can show recovery/status UI immediately. App startup need
not block indefinitely: placements may wait for their preferred output until the
deadline, then use a clearly provisional fallback. Startup or dock-transition
fallbacks never overwrite saved preferences merely because a frame was shown.

When an expected monitor arrives late, restore an automatically displaced window
only if it has not subsequently been moved/reassigned by the user. Track an
explicit placement revision/cause, not a time heuristic. Never move a window
during active pointer capture, drag, or a modal interaction; defer or offer a
restore action. Device arrival should not unexpectedly steal focus.

On loss of an output, immediately apply the appropriate session/generation and
buffer-lifetime rules; do not continue presenting through an invalid session.
Window relocation is a separate policy decision and may use the bounded grace
period below. Once removal is explicit or that deadline expires, move inaccessible
windows to a surviving reachable viewport, keeping their original home metadata.
Reconnect can restore those homes under the same user-interaction rule. Repeated
dock flapping must not save a sequence of accidental fallback layouts.

### Readiness first: boot, wake and KVM recovery

Product priority: variable discovery speed and temporary loss while switching a
KVM are expected routine cases; optimize for restoring a known setup without
window churn. This is a UX policy choice, not a measured claim that physical
disconnects are rare in every deployment. Mobile/docking environments may need
different bounded timing profiles, using the same state machine.

Do not collapse these independent facts into one `connected` Boolean:

- Expected named-display assignment and the strength/ambiguity of its match.
- Observed connector presence, including unknown or temporarily inconclusive.
- Requested power policy: enabled, intentionally blanked or explicitly disabled.
- Driver readiness stage: discovering, reading identity/modes, preparing the
  link/mode, ready for presentation, retrying, or failed with a reason.

These should become typed state/diagnostic fields. A readable identity is not
proof of a usable link; an inconclusive probe is not an instruction to delete a
named display. Driver presentation readiness is also not proof that a physical
panel is visibly showing pixels. Keep the evidence and its limits visible.

For a known output that becomes unavailable during wake/switching:

1. Preserve its desired rectangle and window assignments. Mark affected windows
   as awaiting their display, rather than silently saving a new home elsewhere.
2. Start a bounded recovery episode. Prefer supported event notifications;
   schedule bounded retries/backoff when needed, outside input/presentation hot
   paths. Retry budgets and the final deadline must not reset forever on flapping.
3. The authorized driver performs the necessary discovery/link/mode recovery
   for an output requested to be on. One unsuccessful attempt must not leave a
   known enabled output abandoned until some other OS or a reboot initializes it.
   Never override explicit blank/disable, lid, or power policy to satisfy a retry.
4. Keep unaffected monitors and apps responsive. On a surviving screen, trusted
   recovery UI can show "Waiting for desk-main" and offer "Bring windows here"
   immediately. An explicit user action bypasses the grace period and records
   deliberate placement; it is not later undone by automatic restoration.
5. If readiness returns in time, restore presentation without moving the windows.
   Otherwise activate the deterministic temporary fallback and report why. Late
   recovery still obeys the no-surprise/manual-move rules above.

An explicit Settings removal/disable is user intent, not an ambiguous observation,
so do not force the user to wait for this grace period. The timings should be
measured and bounded; no multi-second sleep in a compositor loop. Multiple
outputs recover independently and must not serialize behind the slowest one.

Reserved desired rectangles are not phantom available monitors: they cannot
receive pointer input or satisfy a live-window reachability check. During
recovery, the surviving output graph may differ from the admitted desired graph;
report that transitional/degraded state honestly. Recovery controls must remain
usable without pointer travel through a missing bridging display. At the fallback
deadline, produce a connected effective arrangement for the surviving seat.
If no output survives, retain desired state and continue bounded driver recovery
without pretending that any window is visible; available authorized serial/remote
recovery remains separate from local input. Resource reclamation must still wait
for genuine device quiescence, not the expiry of a UX grace timer.

Record each episode's trigger, named/live output IDs, stage transitions, attempts,
last driver error, policy decision and effective fallback. Present a useful
diagnosis in Settings/Inspector; collect it away from latency-sensitive painting.
The reported Linux/macOS KVM behavior motivates this test case, but does not by
itself identify a specific EDID, power, link-training or driver fault.

## Remembering where applications belong

Key preferences by authenticated application identity and an app-local stable
window role, not PID, surface slot, launch order or mutable title text. Multiple
document windows can use optional opaque, app-scoped restore keys; do not make
document names or paths public metadata. Duplicate roles without restore keys
must not overwrite one another's placement; use a deterministic cascade until
the user/app establishes distinct identities.

Store a named home display, desktop workspace, logical restore bounds/anchor,
and normal/maximized/fullscreen state. Preserve the ordinary restore rectangle
separately from a maximized/fullscreen rectangle. On changed DPI or mode, resolve
the monitor first, then adapt its local logical placement to the current usable
work area. Do not reuse absolute physical pixel coordinates blindly.

Validate placement at initial show, explicit move/resize, scale/mode/layout change
and restoration. For a decorated window, keep its title-bar controls and a usable
drag region inside one available work area. Oversized minimum sizes require a
defined constrained-layout/recovery path, not unchecked cropping. Borderless and
fullscreen surfaces must remain recoverable through trusted desktop keyboard/UI
controls. Modal children follow their parent group, cannot strand focus on a
missing display, and must not be restored to a different hidden workspace.

Prefer durable placement updates at completed user moves/resizes or explicit
assignment—not every frame and not automatic recovery moves. Crash recovery may
persist a separate effective-session snapshot later; it must not overwrite the
desired home preference. Remembering placement does not authorize automatically
relaunching applications or reopening documents; those require their own policy.

## Desktop workspaces / virtual desktops

Initial recommendation: named workspaces spanning the whole monitor layout,
with all outputs in a seat switching together. A workspace is a window grouping,
not a separate monitor arrangement; changing workspace does not modeset hardware.
Examples are "Development", "Monitoring" and "Games".

Window membership, physical presentation destination and visibility selection
are separate fields. Do not equate a workspace ID to an output index. This keeps
independent per-monitor workspace switching possible later without redefining
surface ownership; that behavior and cross-boundary windows remain a deliberate
future UX decision, not enabled implicitly now.

- Every ordinary window belongs to one live workspace; explicit "all workspaces"
  pinning is a separate desktop-controlled presentation policy.
- Switching changes visibility/focus atomically for the switching group. A hidden
  workspace cannot keep receiving keyboard/pointer input through stale focus.
  Input and frame generations must prevent delivery into the wrong live surface.
- Hidden apps keep running unless separate execution policy says otherwise;
  being hidden neither revokes authority nor automatically suspends work/audio.
  Offscreen repaint demand can be throttled without violating buffer ownership.
- Remember focus per workspace, restoring only a still-live eligible window.
  Trusted prompts/recovery UI must remain reachable across switches; apps cannot
  self-declare trusted overlays or force themselves into every workspace.
- Removing a monitor does not delete workspaces. Removing a workspace is an
  explicit operation that rehomes windows to a selected surviving workspace;
  it is not process termination or permission revocation.
- Window lists/capture/remote inspection remain authority-scoped. Virtual
  desktops are organizational, **not security isolation boundaries**. Different
  security sessions/seats require explicit authority separation, not new names.

## CCL Config representation and persistence

Use a versioned, bounded CCL **data profile** compiled into owned typed records
before effects. Settings edits the same model; it is not a second policy engine.
Names, display matching, profile placements and workspace membership must resolve
and validate as a coherent revision before applying any part of the arrangement.
Expressions, if later allowed, remain pure and bounded: no device writes or
network discovery while parsing the profile.

Illustrative syntax only; **this grammar is not implemented**:

```lisp
(desktop-layout v1
  (profile "desk"
    (primary "desk-main")
    (display "desk-main"
      (mode 3840 2160) (scale 2 1) (rotation unrotated) (position 0 0))
    (display "desk-side"
      (mode 1920 1080) (scale 1 1) (rotation clockwise-90)
      (position -1080 0)))
  (workspace "development")
  (workspace "monitoring")
  (switching together))
```

The display references above resolve through the named-display catalog; their
labels are not raw EDID identities. Exact schema/field spelling and whether the
catalog is bundled with the profile remain to be chosen before implementation.
No new generic CCL builtin or global authority is implied by this example.

Use the existing Config route, likely a desktop-scoped versioned layout value
plus separately revisioned placement preferences. Current Config boot values
are limited to 1024 printable bytes, the system source to 8192 bytes, and the
service does not provide the desired multi-object transaction semantics. A larger
bounded value/blob and revision publication contract must be designed if needed;
do not scatter one layout across independently updated keys and hope readers
see a consistent arrangement. Hardware preparation and durable Config publication
are also different transactions.

Interactive Apply prepares and validates the candidate, provisionally applies it,
and requires confirmation for disruptive changes. Cancel/timeout restores a safe
previous arrangement when possible. Only confirmed intent becomes the desired
revision. Config write/persist failure is shown as "active, not saved"; do not
report successful durable saving just because modesetting succeeded.

Live CD operation remains explicitly volatile unless an authorized persistent
store is configured. No silent writes to an internal disk. Desktop holds narrowly
scoped configuration/placement authority; apps can request preferences for their
own windows but cannot rewrite other apps' records, switch global workspaces or
change physical layout merely by supplying CCL source.

## Implementation and verification order

1. Define named-display matching outcomes, output generations and bounded layout
   records; keep runtime grants distinct from preference identities.
2. Pure adjacency/overlap/layout validator: initial core implemented and proved
   for acceptance soundness. Add registry/catalog resolution and mode/budget
   admission before using it to change live hardware.
3. Initial pure placement recovery is implemented, with typed reasons and a
   proved full-window containment guarantee for successful proposals; oversized
   windows return a typed degraded result instead of silently changing size.
   The bounded episode timer cannot be extended by repeat start notifications.
   Wire this into the owner's serialized intent/topology transitions next.
   Asynchronous proposals, if introduced, must carry checked intent revisions,
   window incarnations and topology/session generations; pure snapshot planning
   does not provide that apply-time lifetime guarantee by itself.
4. Integrate the generation-bound output registry and independent presentation,
   then toolkit geometry/scale notifications. The registry and opaque placement
   tickets now exist as tested/proved owner-local cores. Native display leases,
   attachments and sessions bind to the boot-selected output's local reference.
   Read-only startup discovery now runs over the existing authorized endpoints;
   it distinguishes detected/ready/selected outputs and invalidates stale walks.
   Add topology notifications with authenticated service/driver incarnations,
   finer discovery-only delegation, then window-intent versions plus serialized
   check/apply into the desktop. Keep current single-output tests.
5. Add Settings layout editing and in-memory workspace switching; then the CCL
   schema/Config transaction and optional durable storage, with clear save status.

Regression scenarios include reversed discovery order, late preferred display,
boot/wake readiness skew, KVM switching with a brief presence loss or delayed
readiness, failed first initialization followed by successful retry, flapping
that cannot extend deadlines forever, explicit power-off that must stay off,
missing bridge monitor, duplicate/missing serials, cable/dock changes, mode/scale
changes, 90/180/270 rotation, small work areas, negative origins, reconnect after
a manual move, modal dialogs during unplug, workspace deletion/switching, stale
input and frame completions, failed Apply, and Config failure after confirmation.
QEMU covers multi-output discovery/presentation; hosted models inject identity
ambiguity and arbitrary event orders. Real monitor timing and connector behavior
still require hardware testing. Only the linked geometry, layout admission,
pure placement/timer, registry and ticket-admission properties have proofs;
integrated recovery, persistence and device-lifetime invariants remain proposed.
