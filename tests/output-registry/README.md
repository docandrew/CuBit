# Output registry and placement-ticket freshness

The pure SPARK `CuBit.Output_Registry` model and `CuBit.Placement_Tickets` adapter
implement bounded output metadata and stale-placement rejection. The registry is
now used by native `display.svc` for its current boot-selected output and lease,
attachment and presentation-session bindings. Placement tickets are **not yet
wired into desktop window movement**. Read-only startup enumeration now uses the
separate [typed discovery codec](../output-discovery/README.md), with revisions
bound to the native broker's registry lifetime. There is no configuration wire
operation, hardware handoff or native hotplug claim.

```
nix develop -c make -C kernel test-output-registry prove-output-registry
nix develop -c make -C kernel user_runtime
```

## Identity and lifetime

These objects are distinct:

| Value | Meaning |
| --- | --- |
| Named display ID | Configuration identity used for preferred window homes |
| Driver incarnation + output number | One observed destination in one authenticated backend lifetime |
| Registry incarnation | One owner-managed registry lifetime; new after restart/reinitialization |
| Output reference | Registry incarnation, private slot and nonzero publication revision |
| Topology snapshot | One immutable ready-work-area list and its registry-wide revision |
| Presentation session | Existing display-protocol binding of an authenticated owner to acquired buffers |
| Window version | Window incarnation plus revision of its desired placement |

Names, numbers, snapshots and tickets are **not capabilities**. The eventual
service adapter must authenticate senders and enforce observation/configuration
authority before any registry access. Backend identity must come from the
authenticated session, not a driver's unchecked claim or EDID bytes. The registry
is an owner-local state machine, not a replacement policy engine or IPC service.

The first model has 16 output slots. Registration rejects duplicate named IDs
and duplicate backend destinations, including entries that are temporarily not
ready. Updating an entry cannot change either identity. Rebinding requires an
explicit retirement and registration. Every accepted mutation strictly advances
the registry revision; updates also replace that output's reference. Reusing a
retired slot does not reuse its old publication revision.

Any mutation invalidates previous topology snapshots. Unrelated output references
remain live, though a pending placement plan must be reconsidered against the
new whole-layout snapshot. Even an identical accepted update rotates its stamp:
publish actual state transitions, not high-frequency paint/probe heartbeats.
This is a control-path structure, not an operation to perform per frame or input.

Counters never wrap. If an otherwise admissible mutation needs a revision beyond
the budget, the registry closes, making every reference/snapshot unusable and
returning no ready areas. Further mutations return `Closed`. Closure/retirement
is **not a DMA fence**: existing frame obligations and acquired grants remain
subject to `Presentation_State` and the real backend's quiescence evidence.

The owner must never reset/copy back an old state under the same incarnation,
reuse driver incarnations after restart, or recycle window/intent versions while
old tickets can exist. New registry lifetimes need a genuinely fresh identity
from the trusted session owner. These are integration obligations, not something
the pure model can establish about a restarted service or kernel endpoint.

## Availability and placement

Presence (`Unknown`, `Absent`, `Present`), requested power (`Enabled`, `Blanked`,
`Disabled`) and readiness (`Discovering`, `Preparing`, `Ready`, `Retrying`, `Failed`)
are independent enums. Only present, enabled, ready entries enter a placement
snapshot. Thus a reserved home, readable EDID, or unfinished modeset cannot be
mistaken for an available screen. Physical visibility still requires hardware
evidence; the model trusts the owner's readiness classification.

Work areas are already in the effective logical coordinate system and exclude
reserved desktop UI. This layer does not resolve EDID identities, validate mode
budgets, admit connected layouts, derive areas from physical modes, collapse
mirror groups or repair a missing bridge display. Those transformations belong
before publication. Named preferences and the bounded recovery episode remain
separate, so transient unavailability does not rewrite desired homes.

`Placement_Tickets.Prepare` captures a snapshot and runs the existing placement
planner against it. The opaque ticket records the result and current window
version. Immediately before applying, `Check` requires a real destination, an
open matching topology revision/incarnation, the same window incarnation and
user-intent revision, and no active pointer capture/modal interaction. It returns
a typed reason when any condition fails. Default/deferred tickets cannot apply.

Check **and** application must occur in one serialized owner transition. After
an await/yield or any intervening topology/intent/interaction event, recheck or
replan. This is not a lock, reservation or proof of the compositor's threading.
The owner must update intent revisions on explicit moves/resizes/reassignment
and give reused window slots new incarnations. These hooks are not yet wired
into desktop `Surface` records. Output-generation checks must also bind future
presentation sessions; rejecting placement tickets does not retrofit the current
frame-submission protocol by itself.

## Evidence and limits

Validation on 2026-09-21: **279 hosted checks**, **103 proof diagnostics** across
the concrete proof units, and the native runtime build pass. Existing window
placement (231,563 cases), layout admission (6572 arrangements), and geometry
(191,488 pixel round trips) regressions pass too. No new assumptions, proof skips
or SPARK-Off sections were introduced.

Native object inspection finds no Ghost `Valid`, `Sound` or `Fresh` predicates
and no assertion-handler references. GNAT emits an invariant stub containing only
`ret`, not a registry scan. Native compiler/check settings were not changed.

Hosted checks cover the 45 presence/power/readiness combinations, coherent ready
snapshots, duplicate names/routes, identity-changing update rejection, stale
notifications, independent output references, retired-slot reuse, full tables,
cross-registry confusion, all three mutation exhaustion paths, closed-registry
behavior, and stale window/intent/topology or busy-interaction tickets.

SPARK analyzes the **concrete production instance** in `CuBit.Display_Outputs`,
a two-revision exhaustion instance, and the placement-ticket adapter. Merely
analyzing a generic template would not prove its instantiations. The proof audit
requires full SPARK coverage, no skips/assumptions, and nonempty successful
postcondition checks for each selected unit.

Proved properties include successful mutation's increasing revision and exact
publication, update/retirement invalidation, unchanged state for ordinary
rejections, fail-closed exhaustion, ready-snapshot soundness (each returned area
matches a live, presentable registry entry), numeric/index/invariant safety and
termination. Ticket admission is proved equivalent to its freshness predicate.
The registry's private Ghost invariant keeps live entry stamps positive and no
newer than the registry revision; it is not a runtime table scan in native builds.

Identity-uniqueness preservation and snapshot completeness are regression-tested,
not separately proved as global invariants. Lifetime uniqueness across service
restarts, authenticated update adapters, atomic check/apply, real presentation,
hardware recovery and physical hotplug are **not** established by these proofs.
The existing three-output QEMU fixture remains a discovery-only test; it does
not exercise registry-backed multi-output presentation.

That native QEMU 11.1/KVM discovery regression was rerun successfully:

```
nix develop -c tests/headless/run.sh --test virtio-gpu-multi-output \
  --accel kvm --cpus 1 --timeout 25 --keep-logs
```

It detects 1024x768, 1920x1080 and 1080x1920 outputs and presents the existing
scanout-zero test frame. It does not show independent desktops on all three.

## Native boot-output binding

`display.svc` registers its selected boot backend before publishing readiness.
The provisional display name, registry and backend incarnation are local to
this single service lifetime and never appear on the wire or in Config. The
registry is constructed once and never restored/reset. These local constants
are **not** globally unique restart identities; discovery across processes will
need an authenticated service-lifetime envelope before exporting any references.
The entry currently covers the full framebuffer, not a desktop work area with
taskbar reservations removed. Native placement planning does not consume it yet.

The display lease, acquired source attachment and active presentation session
each remember their output reference. Owner admission checks the current ready
output and lease binding. Opening a session additionally requires a current
attachment; frame admission requires that session's output to match the current
output. Thus advancing an output generation invalidates old work before acquiring
or reading a frame buffer. Client session IDs and the frame wire format remain
unchanged: the service retains the output association, not the client's message.

Reacquiring a lease cannot retarget an old attachment. The client must reattach
and open a fresh session. Source-based older presentation operations also check
the attachment/output association. Release remains possible for an old lease
owner after generation loss, subject to the existing presentation-fault rules.
Invalidating metadata does not return any acquisition, unmap memory or claim
GPU quiescence. Normal synchronous frame completion and grant-return paths still
own those obligations. The current single-threaded service event loop serializes
admission, rendering and metadata changes; this is not a general SMP lock proof.

A dedicated **compile-time** `output-rebind` fixture rotates the current entry
after one successfully completed test frame. It adds no production control
opcode and does not simulate physical disconnection or modesetting. The native
client verifies stale frames return `Rejected/Not_Acquired`, the existing grant
pin stays intact, stale lease/attachment state cannot reopen, and reacquisition,
reattachment and a fresh session restore successful presentation. It tries the
old session again after reacquiring the lease while the source grant is still
valid, so grant revocation cannot mask a missing session/output check. Then it
revokes the old grant: only an outstanding acquisition can keep that revoked
generation alive. Reattachment with a fresh grant must retire the old generation.

```
nix develop -c bash tests/headless/run-output-rebind.sh \
  --accel kvm --cpus 1 --timeout 25 --keep-logs
nix develop -c bash tests/headless/run-output-rebind.sh \
  --test display-grants-virtio-vga --accel kvm --cpus 1 --timeout 25 --keep-logs
```

The wrapper restores production display and test-client binaries on exit.
Both rebind fixtures pass, including the revoked-pin lifetime checks; the staged
display and client were compared byte-for-byte with the production builds after
restoration. Fixture builds have separate ignored output directories. The native integration
is regression-tested, **not SPARK-proved as a whole**; the 103 registry/ticket
proof diagnostics above still describe only those shared model units.

Ordinary desktop smoke tests also cover the unchanged production paths:

```
nix develop -c tests/headless/run.sh --test desktop-display \
  --accel kvm --cpus 1 --timeout 25 --keep-logs
nix develop -c tests/headless/run.sh --test desktop-virtio-vga \
  --accel kvm --cpus 1 --timeout 25 --keep-logs
```

Both pass, including the desktop's asynchronous-frame markers. Production binary
inspection confirms the generation-rotation fixture branch is removed. There is
no per-frame discovery IPC or registry-table scan; admission checks the retained
reference against its one entry. No latency measurement is claimed by this run.
