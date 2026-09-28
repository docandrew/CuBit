# CCL: discover, inspect, connect

Status: session foundation and bounded catalog-prefix completion implemented;
graph editing, completion dropdowns/type pictures, pipeline syntax, and
distributed composition remain planned. September 2026.

## One semantic system, several ways to work

The intended experience is an interactive, strongly typed view of CuBit:
discover services the session is allowed to see, inspect their operations,
understand the objects they consume/return, and compose them into useful
programs. Text, diagrams, desktop widgets, and remote management are different
views/clients of CCL, not separate scripting or security systems.

The same catalog/type information should support:

- completion showing operation names, parameter/result types, effects, required
  authorities, and ownership/disposition verbs;
- hover cards on a service or endpoint, with provider identity and pinned schema;
- inert pictures of types: arrays with element types and bounds, tuples with
  named fields, tagged alternatives, streams and their element types;
- visual explanations of operations, such as fold reducing a bounded sequence
  of elements and an accumulator to one value;
- graph nodes/ports whose connections elaborate to the same typed CCL core as
  source code; and
- source-mapped debugging across those representations, including remote calls
  and pending operations.

Avoid creating a second graph-only runtime, hidden scripting language, or
metadata format that disagrees with the compiler. Define a shared elaborated
representation and source maps before promising arbitrary text/graph round-trip
editing; not every source program will necessarily have a convenient graph view.

## One interface definition, explicit exposure

Design decision: native applications may embed CCL for application-local
extension and control, while separately exposing selected operations for
cross-process composition. Define each typed operation once; generate local
host bindings and IPC client/server adapters from that shared semantic source.
For example, an editor's `replace-selection(String) -> Boolean` can be callable
from its embedded CCL, through an authorized IPC endpoint, or both. It need not
make a round-trip through IPC just to manipulate its own application objects.

Exposure is an explicit provider declaration, provisionally:

| Exposure | Where an operation may be offered |
| --- | --- |
| Embedded only (default) | Explicit bindings supplied to that application's CCL host |
| Published only | An explicitly published IPC interface |
| Both | Local and IPC adapters for the same semantic operation |

These are design terms, not accepted CCL declaration syntax today. Shared
definitions cover parameter/result types, effects, authority requirements,
ownership/disposition verbs, and call or stream contracts. Publication and
per-session binding policy choose which operations to expose; they are not
additional sources of authority. An embedded-only operation must not be exported
automatically when the application publishes another part of its interface.

The same definition does not require identical machine representations or
descriptor digests for every adapter. Transport-specific wire layouts, memory
transfer rules and supported profiles remain explicit and pinned. A generator
must reject an unsupported mapping rather than silently weaken its contract.
This applies to typed streams as well as request/reply operations; see
[typed IPC and stream contracts](typed-ipc.md#calls-and-streams-share-one-interface-model).

### Exposure is not authorization

* Published does not mean discoverable by everyone. Catalog visibility and
  invocation/open-stream authority are separate grants.
* An embedded host supplies a restricted catalog and binding set for each
  script/session. Loading code into an app must not give that code every
  authority or object held by the app. There is no unrestricted native FFI.
* Local host adapters enforce the script's admitted scope at use, including
  object/session lifetime checks. The kernel cannot distinguish scripts within
  one process: the interpreter and native adapters are part of that isolation
  boundary's trusted computing base. Use process isolation where this trust
  boundary is insufficient; do not claim kernel isolation between embedded scripts.
* IPC adapters use authenticated caller, endpoint and service-handle checks.
  Do not substitute the provider's broader authority merely because a shared
  implementation function executes inside the provider process.
* A system-wide Workbench or remote control client is another CCL host with
  explicit grants, not a privileged global interpreter. Publishing, knowing a
  name, or drawing a graph connection cannot elevate it.

Current implementation: Workbench advertises typed `ui` host operations backed
by reusable label/button models. Their local dispatch is not a special case in
the CCL language. In native CuBit the window uses Desktop IPC and Clock calls
use Clock IPC, but button registration itself is currently local to Workbench.
General exposure declarations, generated dual adapters, independent CCL
surfaces and CCL stream composition remain planned.

## The foundation is a headless session, not a console window

`CCL.Sessions` now owns bounded expression evaluation, typed results/diagnostics,
fuel, and a 16-entry transcript. It depends on the CCL catalog/language/VM value
types, not on a windowing library, filesystem, IPC adapter, or terminal streams.
The native and Linux Workbench call it through the same shared REPL view.
The source editor's Interpret action uses a separate session so its history
does not pollute interactive commands.

This first implementation evaluates each submitted expression independently.
`let` bindings do not persist between submissions. History recall copies source
for editing; it does not execute it. Oversized input produces a diagnostic for
the original input, never execution of a truncated prefix. Eviction only removes
the oldest transcript entry. Clearing history preserves the supplied catalog;
initialization explicitly replaces the session/context. Neither is a revocation
mechanism for external resources.

Catalog visibility is passed explicitly by the host. Default `Submit` still
returns `Host_Import_Required` for service calls. Workbench now instantiates
`Submit_With_Host` with its explicit bindings and statically bound dispatcher;
analysis and complete admission happen before evaluation, and one outcome is
recorded without retrying/replaying source. The session engine cannot acquire
bindings. A visible description is never an executable grant. See the current
[scoped UI hooks](ccl-ui.md) for the first real Workbench-owned control.

Next steps toward persistent environments must specify definition/value
lifetimes, captured handles, move-only/must-handle obligations, and resource
budgets. Do not simulate persistence by replaying command history. Once async
calls are admitted, reset/disconnect cannot simply forget noncancelable work or
its obligations; pending operations need explicit owners and terminal outcomes.

## Discovery and visual explanations are authority-scoped

Use the existing [interface descriptors](ccl-interface-descriptors.md),
[security model](security-model.md), and [CCL UI boundary](ccl-ui.md).

1. The discovery service supplies only the catalog view authorized for this
   session. Completion and graph palettes must not enumerate hidden services.
2. Keep provider identity/provenance, advertised schema, granted authority, and
   current binding state distinct. A matching operation name or schema digest
   does not establish a trusted provider. `NEEDS ... FROM ...` is a policy
   requirement, not a way to prove the provider's own claims.
3. Validate/pin versions and complete descriptor identities. Show stale,
   unverified, unavailable, and revoked states, not just one green checkmark.
4. Treat descriptions/type diagrams as untrusted bounded data. Hovering or
   completing must not execute provider code or trigger an operation for a
   "sample result." Rich type pictures use a small inert presentation grammar.
5. A type picture reveals shape, not secret contents. Live value inspection is
   a separate authorized action, with redaction and bounded sampling.

The current catalog is still a bounded snapshot with limited scalar operation
signatures, not the full live reflection service imagined here. Extend read-only
enumeration and the descriptor type graph deliberately before building elaborate
completion UI. Concrete first type cards can start with today's integers,
booleans, strings, characters, and actual advertised operation signatures.

### First completion slice

`CCL.Catalog.Completion.Find` enumerates matching qualified operation names and
their exact descriptor contracts from the supplied catalog. The result holds up
to 16 suggestions and separately reports the total (up to 256); overflow is
explicit. Prefixes are case-sensitive; no catalog access, host call, grant, or
sample evaluation is triggered. `CCL.Sessions.Complete` uses only that session's
catalog and does not change history or execute source.

In both Workbench builds, open the REPL with F6 and type `(clock.mon`. A unique
match appears automatically as a muted inline suffix; **Tab** accepts it.
A compact signature popup appears above the caret, clamped inside the REPL.
It follows the innermost open call and stays visible after its exact name when
arguments begin. Today's zero/one-scalar-argument schema supplies either
"No arguments expected" or the expected argument type (highlighted in the
signature). This describes the declared parameter, not a full argument-validation
or overload-resolution UI. Unknown inner calls do not borrow an outer signature.
Ctrl+Space explicitly shows the scalar signature (or the first match and total
when ambiguous) without inserting text. Multiple matches never guess a suffix;
refine the prefix for now. The suggestion is presentation-only: it is not in
the editor buffer, selected text, history, or submitted expression until accepted.
The shared text-field widget clips it to the field and paints the caret last.
Suggestions are refreshed on editing/navigation, not during painting or hover.
Strings, comments, selections, and a caret before existing text are not rewritten.
Insufficient input capacity rejects
the insertion rather than inserting a truncated name. This is call-head
completion after `(` (with optional whitespace), not local-variable completion
or a dropdown. `CCL.Call_Context` performs bounded, inert context inspection;
the normal language parser/type checker remains authoritative.
The Workbench REPL interprets with its explicitly admitted host bindings;
seeing/completing a name does not make it callable. Other hosts may keep the
default pure evaluator or supply their own narrower bindings.

Hosted tests cover maximum-length names, full catalog capacity, shifted string
bounds, isolation, and agreement with normal name resolution. The core completion
and context units discharge 65 SPARK checks, with no assumptions or SPARK-Off regions. This
does not prove the GUI or authenticate provider identity. See
`tests/ccl-completion` for the core tests and SDL input/screenshot regression.

## Typed pipelines: proposed `|>` surface syntax

Start with value composition. Candidate rule:

```text
value |> f(extra) |> g
```

means evaluate `value` exactly once, pass its result as the first argument of
`f` with `extra`, then pass that result to `g`. Elaborate through explicit local
bindings so evaluation order, exceptions/errors, effects, and ownership match
ordinary calls. The corresponding Lisp surface might be `(pipe value (f extra)
g)`; exact spelling/placeholder rules are open, not accepted syntax today.

Resolve overloads using the type flowing into the operation and the surrounding
expected result type. Ambiguity is a diagnostic, not a runtime guess. Do not
silently insert authority acquisition, coercions, asynchronous waits, remote
calls, retries, or sequence traversal to make a connection type-check.

A move-only value moves once; graph fan-out is not an implicit copy. A
must-handle result retains its consumed/returned/committed/rolled-back obligation.
Borrowed-ro/borrowed-rw lifetimes remain visible. Ownership applies equally to
text pipelines and graph wires.

Streams require explicit combinators with element, capacity/backpressure,
ordering, lifetime, and cancellation semantics. A pipeline into a stream
operator does not mean "drain this stream now." Async results and noncancelable
tasks are not interchangeable with immediate values. Fold must identify the
accumulator type and iteration bound; an unbounded stream fold is not a bounded
single evaluation. Feedback cycles require an explicit delayed/event boundary
and scheduling/resource analysis rather than unrestricted recursive evaluation.

## Distributed application graphs

The motivating graph might connect certificate renewal, a certificate store,
load-balancer configuration, health checks, and audit reporting. Another might
connect metrics streams, bounded aggregation, and a dashboard widget. These are
composed typed services, not shell commands sending text through stdin/stdout.

Every graph connection must answer:

- Which authenticated provider and pinned interface does this bind to?
- What type and ownership mode crosses the edge? Is this value transportable,
  or does it require an explicit, attenuated remote proxy?
- Which authority permits the operation and any delegation, and who can approve
  it? Drawing a wire is an intent, not a grant.
- Where does execution occur, who owns outstanding work, and what happens on
  timeout, disconnect, restart, duplicate delivery, or partial failure?
- What are the queue, memory, rate, execution-fuel, and concurrency budgets?

Mutual TLS authenticates a channel/peer; it does not confer local capability
authority. Raw local handles must not become remote bearer authority by being
serialized. Retries of effectful work require explicit idempotency or operation
identity; do not promise exactly-once execution across network failures.

Secret connections should normally carry narrowly scoped handles rather than
copying secret bytes into the graph editor, transcript, generated source, or
configuration. Declarative deployment must keep desired configuration, approval,
and actual execution separate, with the same WHAT/WHO/WHEN/WHERE/WHY visibility
as interactive operations. See [remote management](remote-management.md) and
the [CCL package/configuration design](ccl-packages.md).

## Delivery order

1. Exercise the extracted REPL sessions in native CuBit and the Linux preview.
2. Expose authorized descriptor enumeration/type inspection to completion and
   simple type cards; integrate admitted VM host calls with the session API.
3. Add first-class functions and settle persistent binding/ownership lifetimes;
   specify and test `|>` elaboration against ordinary calls before adding syntax.
4. Host a small typed label/button widget, then a timer-driven clock, through
   the same session/runtime boundary with explicit grants and stop behavior.
5. Add graph composition over those real interfaces, source mapping, bounded
   stream combinators, deployment plans, and explicit remote adapters.

This preserves the dream without requiring the whole distributed visual
environment before the first useful CCL programs can run.
