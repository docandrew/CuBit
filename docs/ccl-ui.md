# CCL UI

Status: design proposal

## Implemented: scoped Workbench label hooks (September 2026)

The native and Linux Workbench REPLs now use `CCL.Sessions.Submit_With_Values`:
the entire expression is analyzed and its service imports admitted against
explicit granted bindings before execution. The source editor, REPL, Watch,
and scalar bytecode debugger share the host dispatcher. REPL execution was
interpreted at the time; since 2026-10-05 the interpreter is gone and every entry
is compiled to CCLB (with text crossing host imports, format version 9),
verified and run on the VM.

The first UI hooks target exactly one host-owned numeric/text label:

```lisp
(clock.monotonic-ms)
(ui.label-value 42)
(ui.label-value (/ (clock.monotonic-ms) 1000))
(ui.label-visible false)
(ui.label-text "Hello, Cubie")
(ui.label-text (concat "Uptime seconds: " (to-string (/ (clock.monotonic-ms) 1000))))
```

`ui.label-value : Integer -> Boolean` updates and shows the label;
`ui.label-visible : Boolean -> Boolean` changes its visibility. Boolean success
means the model update was accepted, not that pixels reached the display.
`ui.label-text : String[0..1024 bytes] -> Boolean` copies text into the label and
shows it. Empty text is valid; hiding remains a separate operation. Run/F5,
the REPL, and Watch/F7 accept text imports. The runnable sample is
`userspace/ccl/samples/clock-label.ccl`. Watch owns a source snapshot; stop and
restart it to pick up edits.

`CCL.Host_Values` separates source/host contracts from scalar CCLB imports.
Arguments and results carry explicit integer, boolean, or bounded-text tags.
Text has owned storage, never an evaluator-region pointer. Both argument and
result bounds participate in exact grant matching. The evaluator checks an
argument's size before calling the host, and checks returned type/size before
copying a text result into its own temporary region. Already completed effects
are not rolled back if a later argument exceeds a bound. Scalar-only adapters
reject text contracts during whole-program admission, before any effects.
CCLB never encodes strings as integer addresses. (Originally CCLB rejected text
imports; format version 9 carries `Argument_Text_Limit`/`Result_Text_Limit`, so
text now crosses host imports in bytecode.)

The descriptor is `userspace/ccl/interfaces/workbench-ui.ccl-interface`; the
compiler has no special UI syntax or operation names. Completion/type hints use
that same advertised contract. Each operation has an explicit local binding with
Control authority metadata. Neither a bare integer nor the category itself is
a widget handle or a grant.

`CCL.UI_Labels` is a small SPARK model with no GUI/IPC dependency. The Workbench
adapter owns it and draws it with the shared toolkit into the toolbar label area.
Changed values repaint that clipped region; identical values do not mark it
dirty. Hiding it restores the Watch/default label. Existing Watch sampling can
continue underneath; this prototype is not a second window or a general widget
tree. Clearing REPL history neither replays effects nor destroys the host label;
hide it explicitly or close Workbench. No filesystem, global-input, other-window,
or policy-administration authority is passed into evaluated CCL.

Linux emulates the Clock binding locally. Native CuBit uses the Workbench's
authorized Clock IPC endpoint and its native desktop surface. UI hooks are
currently local to the hosting application, **not a new public desktop IPC
widget service**. Ownership-checked handles, surface
creation, button events and independent widget lifetimes remain next steps.
Runtime failures after a successful host call do not roll back earlier effects.
Calls remain synchronous/noncancelable; fuel is not a bound on IPC wait time.

Hosted admission tests cover missing grants before any effects, wrong argument
types, fuel exhaustion, one execution per submission, host failure/type mismatch,
isolated contexts, history clearing, label dirty-state behavior, and compiled /
verified scalar UI imports. SDL tests exercise actual REPL show/hide, text and
Clock calls. `tests/usb-optical/run-live.py --cpus 4 --ccl-ui-hooks` passes on the
rebuilt USB Live CD and drives the native keyboard path through four submissions.
The captured native screenshot was visually checked for the `hello cubit` label
and successful Boolean result. Session proof currently has three
unproved diagnostic-string concatenation bounds in the existing `Result_Image`;
do not claim end-to-end proof of the host adapter or transcript formatter.

The focused host-value/label proof passes 45 obligations (22 flow/termination,
23 prover, zero unproved), using an isolated `--subdirs=text-proof` report.
This covers `CCL.Host_Values` and `CCL.UI_Labels`, not the complete evaluator,
host dispatch, or asynchronous lifetime model. A discriminant failure in the
original text-copy API was fixed by returning a concrete bounded `Text` record
and constructing tagged values separately, rather than adding an assumption
or requiring callers to supply an unconstrained discriminated record.
`tests/ccl-sessions/text_tests.adb` covers bounds, exact grants, copied label
lifetime, text-returning hosts, scalar-adapter rejection, and Watch delivery.

## Earlier Watch slice and full UI service roadmap

The first shared native schema and its SPARK proof boundary are documented in
[Typed desktop protocol](desktop-protocol.md). Create, present and attachment use
checked typed payloads; batching, asynchronous release/presentation and CCL
widget construction remain subsequent work, not current API guarantees.

2026-09-09 native slice: Workbench now has a **Watch / Unwatch (F7)** live
label, using the same toolkit renderer on CuBit and in the Linux preview.
It snapshots the editor source and evaluates it once per second through
`CCL.Periodic_Programs.Evaluate_Due`. The default source formats a live
`clock.monotonic-ms` sample as HH:MM:SS (uptime, not wall-clock time).
CuBit calls the manifested clock endpoint; Linux emulates that binding.
Ordinary Run/F5 now uses that same host adapter for one-shot evaluation.

In the original Watch mode the host owns the label and source returns its value.
The new hooks above can also update that display area. This is **not yet
CCL widget-construction functions**, an independently running desktop widget,
or a new UI service. Closing Workbench stops it. Edits do not alter a running
snapshot; stop and Watch again to apply them. Open/Save persists source only,
never handles, grants, or running state. A loaded file must be explicitly run.

The interpreter (removed 2026-10-05) accepted at most 1,024 source bytes (less than the
editor's 4,096-byte storage capacity); Watch reports an oversized source rather
than evaluating a truncated prefix. Each sample gets 4,096 fuel, missed
deadlines coalesce, and an evaluation error stops subsequent samples. Clock
host calls currently remain synchronous and
not cancellable; fuel bounds language execution, not service response time.
SEC-018 still applies. The shared toolkit's `Wait_Input_Until` parks on a
desktop-owned reply capability until input or an absolute monotonic deadline;
the compositor includes this deadline in its existing event-driven wait.
The live label updates only its own clipped damage rectangle while idle.
Workbench scrollbar repeat uses the same timed wait, selecting the earlier
deadline while a monitor is active; holding a scrollbar thumb must not park
the monitor indefinitely.

The host-call boundary now carries bounded strings. Before adding general
widget construction, extend it with ownership-checked surface handles; do not
bypass that boundary with raw integer "handles" or widget pointers.
Then publish a pinned UI schema with surface creation, label creation, batched
text updates and explicit close, using the authority/ownership rules below.
The names and example syntax below remain proposals, not accepted CCL syntax.

Validation for this slice: Nix-hosted remote/host/periodic tests and the native
`ccl-workspace` and `input-stream` headless KVM tests pass. GNATprove discharges
21 lifecycle checks and 1,176 checks in the SPARK host/evaluator instantiations,
including `Evaluate_Due`, with no unproved checks. These are not proofs of the
native host adapter, IPC transport, renderer, compositor, or service latency.
`make -C kernel prove-ccl-periodic` now includes the SPARK host instantiation
as well as the lifecycle package; run it inside `nix develop`.

2026-09-08 prototype: the browser Observatory can start/inspect/stop a native
periodic label evaluated by `ccl-control`. `CCL.Periodic_Programs` supplies its
reusable, SPARK-proved lifecycle. The browser renders the typed scalar/string
result; the source cannot manipulate DOM or native widget pointers. This is a
single lab-owned output surface, not implementation of the general UI IPC
protocol below, authenticated NEEDS discovery, or native desktop widgets.
See `userspace/ccl/tools/ccl-observatory/README.md` for operation and limits,
and SEC-018 for asynchronous/deadline-aware host work still required.

Current native Workbench source storage and the proposed REPL-to-widget sequence
are tracked in [CCL workspace milestones](ccl-workspace.md). The Workbench uses
native toolkit controls today; the CCL-facing widget IPC interface described
below is not yet implemented.

The shared `CuBit.UI.File_Dialogs` component now hosts Workbench Open/Save
selection. `CCL_Workspace` implements the file-access boundary with native
filesystem IPC and a Linux memory mock. The widget only returns a selected
name; it cannot grant authority or give evaluated CCL the host's file access.

CCL UI is the typed interface through which CuBit Control Language programs
construct interactive desktop surfaces. It is intended for dashboards, status
widgets, application-control panels, management tools, and small interactive
applications. It complements the language design in
[`control-language.md`](control-language.md).

CCL does not link the native widget toolkit into its VM and does not receive
native widget pointers. The desktop service owns all UI objects and exposes a
bounded, typed IPC protocol. Native applications and CCL should ultimately use
the same protocol. The Linux CCL Workbench implements an emulator for that
protocol so the same program can run against mock services or a real CuBit
desktop. Its debugger and authority observatory follow the shared explainable
authority model in [`security-model.md`](security-model.md).

## Goals

CCL UI should:

* make small interactive system tools pleasant to write;
* preserve the desktop service as the rendering and input-isolation boundary;
* make UI authority and resource ownership visible in CCL types;
* support deterministic Workbench execution without changing program logic;
* batch operations to keep IPC and context-switch costs low;
* bound widget trees, text, event queues, drawing commands, and update rates;
* support both standard native widgets and custom canvas content; and
* work with local, headless, and remote management front ends.

It should not:

* expose native pointers, callbacks, or an unrestricted FFI to CCL;
* grant clipboard, screen capture, global input, or cross-application access as
  a consequence of owning a surface;
* issue one IPC operation for every property assignment or drawing primitive;
* permit an application to draw outside its assigned surface; or
* make remote UI transport appear local or failure-free.

## Architecture

```text
CCL program
    |
    | typed CCL.UI operations
    v
CCL host adapter
    |
    | batched typed IPC / bounded bulk grant
    v
desktop UI protocol
    |
    +--> native CuBit widget toolkit
    +--> compositor and isolated input routing

The same protocol is implemented by:

    +--> CCL Workbench UI emulator
    +--> future remote management renderer
```

The client-server protocol is the security boundary. An FFI may be used inside
the Workbench adapter or desktop service to call an existing native toolkit,
but it is an implementation detail behind the protocol and is not part of the
CCL authority model.

## UI authority

A CCL program receives handles only for UI resources explicitly delegated to
it. A surface handle permits operations on that surface and its descendants; it
does not permit enumeration, observation, input capture, or modification of
other applications.

UI authority is separate from the authority used to obtain displayed data or
perform an action. A network dashboard might receive:

```text
permissions {
  ui.surface.create
  network.observe
  timer.periodic
}
```

It does not thereby receive `network.control`. A service-health display and a
service restart button should be usable with different grants. The type checker
must reject a handler that invokes a control operation when only observation
authority is available.

Sensitive operations remain separate permissions, including:

* clipboard read and write;
* screen or surface capture;
* global shortcuts;
* unrestricted keyboard capture;
* drag-and-drop across applications;
* accessibility observation or control; and
* opening resources owned by another application.

## Resource types and ownership

The initial protocol should model these resources:

| Resource | Suggested mode | Completion verbs |
| --- | --- | --- |
| `Surface` | must-handle | `close`, `return` |
| `Widget_Id` | unrestricted value | none |
| `Layout` | move-only or unrestricted immutable value | none |
| `Update` | use-once | `commit`, `rollback`, `return` |
| `Frame` | use-once | `commit`, `discard`, `return` |
| `Event_Subscription` | must-handle | `unsubscribe`, `return` |
| `Image` or `Font` | borrowed-ro/shared immutable | `return` when borrowed |

`Widget_Id` is an opaque generation-tagged identifier scoped to one surface.
It is not a native pointer. The desktop validates the client, surface,
generation, widget type, and requested operation on every submitted batch.

Child widgets are normally owned transitively by their surface. Closing a
surface destroys its widget tree and closes its event stream. Individual child
handles therefore need not become independently must-handle unless they own an
external resource.

## Declarative widget model

CCL UI uses a retained, declarative tree for standard controls. The desktop
service translates the submitted description into the existing native widget
toolkit.

An illustrative BASIC-like form is:

```text
surface = ui create-surface title "Network"

layout = ui column
  ui heading "Network"
  rate_label = ui text "Waiting for samples..."
  graph = ui chart capacity 120
  refresh = ui button "Refresh"
end

await ui commit surface layout
```

The precise syntax is not settled. Its canonical structured form should
elaborate into ordinary typed CCL calls rather than introduce privileged VM
opcodes.

The first widget set should remain deliberately small:

* surface;
* row and column containers;
* text and heading;
* button;
* bounded numeric series or simple chart; and
* optional spacer and separator primitives.

Later additions can include tables, trees, text input, selection controls,
images, menus, accessibility metadata, and richer layout constraints. A rich
`Text_Editor` widget is a planned shared control, not a Workbench-specific text
area. It uses the bounded editing core described in [`editor.md`](editor.md),
including selections, multiple cursors, viewport management, undo budgets, and
versioned highlighting. Each addition must have bounded storage and event
behavior.

CCL accesses an editor through an opaque widget ID scoped to its surface. It
cannot receive a native widget pointer or gain file, clipboard, or directory
authority through the editor. The Workbench may use the editor for REPL history
and CCL source buffers; filesystem content and clipboard operations still
require separately delegated handles.

## Batched updates

Tree creation and related property changes should be submitted as one bounded
transaction rather than a sequence of synchronous IPC calls:

```text
update = ui begin-update surface
ui set-text update rate_label sample.summary
ui append update graph sample.bytes_per_second
await ui commit update
```

An `Update` is consumed by `commit`, `rollback`, or `return`. After commit it
cannot be reused. The desktop validates the complete transaction before making
it visible, preventing partially applied layouts and reducing process switches.

The initial implementation may encode an update into an inline IPC payload.
Larger trees and chart batches should use the existing asynchronous bounded
bulk-transfer mechanism. The receiver validates the transferred description
before retaining any references to it.

## Events and asynchronous execution

### Button-defined CCL handlers

Direction agreed: defining a button should be able to supply a typed CCL
function for its click handler. This is a language function executed by the
owning CCL runtime, not a native function pointer, arbitrary FFI callback, or
string of source evaluated by the desktop service. Surface/button creation and
handler registration are distinct operations internally even when the authoring
API combines them for convenience.

Ordinary named typed definitions/calls and `(handler name)` are implemented in
the analyser, compiler and VM, and in both source views. The latter requires an earlier named
`() -> Boolean` function. This is a dedicated handler type, not an integer ID,
native pointer, general closure, or arbitrary callable value.
Advertised host imports support owned handler references alongside
bounded text and integers/booleans. In CCLB, `(handler f)` compiles to an inert,
capture-free function value typed `Handler` that only a host import accepts.
Handler-valued results are not exportable; the remote wire boundary does not
serialize these references.

Implementation order:

1. Implemented: bounded text by value through `CCL.Host_Values`, with owned
   byte-string storage and explicit length limits. `ui.label-text` uses that
   boundary. Unsupported bytecode text imports are rejected, never lowered
   to scalar sentinels. Ordinary text, returned strings and Clock formatting
   have hosted regressions; the value/label packages have focused SPARK proofs.
2. Implemented: ordinary typed function definitions/application, without captures
   or recursion. Runtime preparation now checks the explicit `Boolean_Action`
   profile (`() -> Boolean`) and retains owned checked code and binding identities.
   CCL-visible handler values and registration syntax are now implemented.
   These are non-capturing handlers: bindings come from the owning
   runtime's explicit grants, not from the input sender or widget service.
   Keep general closures separate until capture and ownership rules are enforced.
3. Implemented: one host-owned button and registration that retains the admitted handler
   for the UI owner's lifetime. A click is a typed event identifying the current
   registration; it is not an instruction to call an arbitrary address. Editor
   edits, history eviction and REPL reset must not silently replace its code or
   destroy an in-flight invocation. The owning UI session retains registrations,
   rather than borrowing temporary evaluator/secondary-stack storage.
4. Implemented synchronous dispatch outside input/paint callbacks, with one invocation at a time per
   registration, a fresh fuel budget per invocation, bounded event storage and
   an explicit overflow outcome. Do not silently coalesce discrete button clicks.
   No reentrant evaluator invocation from the widget toolkit.
5. Registration identity is bound to a generation. Closing/replacing a button stops
   admission of new clicks and prevents stale queued events from reaching a new
   handler. Noncancelable work retains its owner and obligations until terminal;
   closing the widget is not evidence that its outstanding work was canceled.

### Implemented callback runtime foundation

`CCL.Language.Handlers.Prepare` analyzes a complete source snapshot, validates the
named entry's profile, and uses the same admission check as ordinary
programs. No main expression or function body runs during preparation. The
retained tree selects the checked function body as its entry point; invocation
does not reparse source or borrow an editor/history buffer. All referenced imports,
including unused functions and the main expression, still require admission.

`Execute` checks every referenced operation against the host's current grants and
requires the original runtime binding identity. Removing or rebinding a grant
therefore denies execution before effects. Binding IDs are owner-local: the host
must not recycle an identity for a different resource while retained uses exist.
Native IPC/service checks remain mandatory; dispatch checks are not a substitute
for session revocation or lifetime enforcement at the service boundary.

`CCL.Callbacks` owns a prepared handler and fuel budget. Its `Callback_Queues`
model stores up to eight pending discrete events plus one active invocation.
Enqueue returns explicit full/inactive/stale/identity-exhausted results. Dispatch
claims one event and runs it with fresh evaluator storage and fuel, outside
input/paint callbacks. Boolean false is an application result, not an automatic
retry. Runtime failure faults the registration and reports discarded queued
events. Closing also reports discards, forbids new admission, and preserves an
executing invocation in `Draining` until its matching completion arrives.
Replacement is rejected while listening/executing/draining; it requires explicit
close or a terminal failure. Generation and sequence counters never wrap.

These objects have one serialized owner; they are not concurrent containers.
References/tickets are scoped to that owner, not globally routable security
capabilities. The current dispatcher is synchronous. The independently tested
claim/close/complete state machine models later noncancelable async completion,
but does not implement suspension or a general async runtime yet.

Headless tests, a focused queue proof, and handler/dispatcher flow analysis are
in `tests/ccl-callbacks`. The queue's completion-matching and close-state
specifications are Ghost. This does not prove the whole evaluator or IPC path.
Workbench now exposes the shared `CCL.UI_Buttons` owner model through the
advertised `ui` version 1.2 interface:

* `ui.button-text(String) -> Boolean` sets its owned caption.
* `ui.button-on-click(Handler() -> Boolean) -> Boolean` admits and registers
  the handler, making the button visible. An active registration returns false;
  callers must close it before replacement.
* `ui.button-close() -> Boolean` hides the button and closes its registration.
  Workbench Stop and window teardown do the same. A handler can close itself;
  queued clicks are discarded and the current invocation completes normally.

`(handler clicked)` creates an owned, bounded source-and-entry reference when
crossing the typed host boundary. Registration analyzes that snapshot once in
the receiving owner's catalog/grants, then retains the checked tree and binding
identities through `CCL.Language.Handlers`. Clicks never reparse it. The source
copy carries code, not authority; editor changes and later REPL submissions do
not mutate the registration. No capture of temporary evaluator storage occurs.
The BASIC spelling is `handler(clicked)` and round-trips through the shared AST.

Use F5 or submit through the REPL, not periodic Watch, to register a button. See
`userspace/ccl/samples/button-clock.ccl`. The same Workbench model is used by
Linux preview and the native CuBit app. This first button occupies the existing
Workbench toolbar; it does not yet create an independent window or introduce
a new Desktop-service widget protocol.

The reference/host-value/queue proof covers 77 checks (34 flow, 38 runtime,
5 functional), all discharged, without assumptions. This is focused coverage,
not proof of the whole evaluator, host adapter, rendering or native IPC.

Later closures may capture immutable unrestricted values by owned copy. They
must not retain pointers into the evaluator's temporary string region. Captured
authority follows the language's move-only/must-handle/borrowed rules: a reusable
button handler cannot implicitly duplicate or repeatedly consume a one-shot
resource. Registering a handler does not broaden its grants. Failure after an
effect does not undo that effect; retries must be explicit.

Test the handler separately without a GUI, then exercise typed click delivery,
close/recreate with stale events, queue overflow, fuel exhaustion, denied imports,
callback failure and noncancelable completion on both hosting adapters. Headless
runtime coverage is now implemented; visible button and Desktop IPC integration
remain planned. Text imports and ordinary functions are already implemented.

Input is delivered as typed events scoped to a surface and widget. CCL handlers
run with their own fuel allocation and may suspend on asynchronous imports.

```text
on refresh clicked
  sample = await network snapshot
  update_dashboard sample
end

on timer every 1s
  sample = await network snapshot
  update_dashboard sample
end
```

Representative event types include:

* `Clicked (Button_Id)`;
* `Value_Changed (Widget_Id, Typed_Value)`;
* `Pointer_Down (Canvas_Id, Position, Buttons)`;
* `Key_Down (Surface_Id, Key, Modifiers)` when explicitly enabled;
* `Surface_Closed (Surface_Id)`; and
* `Timer_Fired (Timer_Id)` from the timer service, not the desktop.

Event queues are bounded. Each surface declares or receives a maximum queue
depth. Coalescible events such as pointer motion and resize should use
latest-value semantics. Non-coalescible events require an explicit overflow
policy such as pause, drop-with-audit, or terminate the subscription. Event
delivery must never create an unbounded number of CCL tasks.

Not every pending operation is cancellable. Closing a surface stops new input
delivery, but an already accepted service import follows its declared
cancellation and ownership semantics. UI teardown must not silently pretend
that an external operation was cancelled.

## Canvas

Canvas is the lower-level escape hatch for custom graphs, visualizations, and
novel controls. It grants drawing authority only within an assigned canvas
widget. It does not expose a framebuffer or compositor memory.

Drawing uses a retained, bounded command buffer:

```text
frame = canvas begin canvas_widget

canvas clear frame color "#101820"
canvas line frame from (10, 20) to (200, 20) color "#44ccff"
canvas text frame at (10, 40) value "42 MB/s"
canvas polyline frame points samples color "#55ff88"

await canvas commit frame
```

The desktop validates:

* command count and encoded byte length;
* point, path, and text lengths;
* coordinate and numeric ranges;
* clipping to the assigned canvas;
* image, font, and brush ownership;
* aggregate render-memory cost;
* frame submission rate; and
* per-client GPU or software-rendering budget.

Frames are use-once values consumed by `commit`, `discard`, or `return`.
Images and fonts are immutable shared resources or explicit borrowed-ro
handles. Canvas input uses the same typed, surface-scoped event mechanism as
standard widgets.

For live dashboards, a surface should support a bounded latest-frame-wins mode.
An old unrendered frame may be replaced by a newer frame, but committed resource
ownership must still be resolved deterministically. A slow renderer must not
create an unbounded frame queue.

## Limits and validation

Every surface is created with effective limits chosen by policy and no greater
than the limits requested by its manifest or host:

```text
Surface_Limits {
  max_widgets
  max_tree_depth
  max_text_bytes
  max_series_points
  max_update_bytes
  max_canvas_commands
  max_canvas_points
  max_event_queue
  max_updates_per_second
  max_render_memory
}
```

Limits are visible to the program so it can adapt rather than discover them
only through failure. Validation failure returns a typed error and applies no
partial update. Repeated quota violations may be audited or terminate the UI
session according to host policy, but do not grant the desktop permission to
cancel unrelated external operations.

All text requires an explicit encoding and maximum byte length. Numeric chart
samples should use fixed, declared element types. Layout and canvas decoding
must be implemented in SPARK or isolated behind a small proved validator before
untrusted data reaches the native widget toolkit.

## Workbench parity

The Workbench UI emulator implements the same typed host interface as CuBit.
It should support:

* deterministic mock services and timers;
* inspection of the retained widget tree;
* scripted event injection;
* pending-import and ownership-state inspection;
* fuel and event-queue visualization;
* snapshots for UI regression tests; and
* record/replay of typed event sequences.

Workbench-specific convenience features must not become implicit authorities
in production. A program receives the same declared handles in both hosts; only
the endpoint implementation changes.

## Initial demonstration

The first end-to-end example should be a read-only network dashboard:

```text
every 1s
  sample = await network snapshot
  update = ui begin-update dashboard
  ui set-text update status sample.summary
  ui append update traffic sample.bytes_per_second
  await ui commit update
end
```

It exercises typed authority, timer events, asynchronous imports, suspension
and resumption, bounded series storage, batched UI updates, and desktop IPC. A
refresh button adds interactive input without requiring control authority.

The same source should run:

1. in the Workbench against deterministic network samples;
2. in CuBit against a read-only network telemetry endpoint; and
3. eventually through a remote management renderer with explicit network
   failure semantics.

## Implementation plan

1. Define a versioned desktop UI protocol shared by native clients and CCL.
2. Add opaque generation-tagged surface and widget identifiers.
3. Implement bounded layout and update encodings plus a validating decoder.
4. Adapt the existing native widget toolkit behind the desktop protocol.
5. Implement the Workbench emulator for the same protocol.
6. Add `Surface`, `Row`, `Column`, `Text`, `Button`, and bounded `Series` CCL
   bindings.
7. Add typed click, close, and timer event dispatch with bounded queues.
8. Build the read-only network dashboard in Workbench and CuBit.
9. Add bounded bulk update transport and canvas command buffers.
10. Prove decoder runtime safety, identifier scoping, transaction atomicity,
    queue bounds, and non-amplification of UI authority.

## Open questions

* Whether layouts are immutable values replaced wholesale or persistent trees
  updated through transactions.
* Whether event subscriptions are per widget, per surface, or compiled into one
  surface event stream.
* Which existing native widget API should become the canonical shared desktop
  protocol rather than remain compositor-internal.
* How style resources and accessibility metadata are typed and bounded.
* Whether remote rendering transports UI descriptions, semantic updates, or a
  separate management-specific presentation model.
* How abandoned must-handle surfaces and accepted non-cancellable imports are
  reconciled during process teardown.
