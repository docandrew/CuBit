# Typed application inlets and outlets and authorized runtime wiring

Status: design plus a pure, hosted-tested admission model. This is not a live
connection registry, new syscall, CCL syntax, or replacement for current streams.
See [typed IPC](typed-ipc.md), [policy roadmap](authority-policy-roadmap.md), and
[security model](security-model.md).

## Application contract

Applications declare named typed inlets and outlets. Authorized runtime
configuration determines their connections. The application emits to or consumes
from its assigned outlet or inlet; it need not discover a global collector, mixer, or backend.
Names such as `stdout` and `stdlog` are conventions, not ambient authority or Unix
file descriptors. An ordinary settings UI, CCL, a graphical connection editor,
and deployment configuration should all invoke the same authorized operations.

`stdlog` should carry a versioned structured log-record schema, including an
explicit producer timestamp/timebase and severity. `stdout` can carry text.
Their representations are different; matching a name does not make them
compatible. A first [bounded diagnostic codec and text adapter](typed-logging.md)
now exercises this distinction in a hosted flow demo; it is not a native binding.

```text
stdout: Text → text-to-log adapter input
              text-to-log adapter output: LogRecord.V1 → logstore
stdlog: LogRecord.V1 ───────────────────────────────────→ logstore
```

The adapter is real code with framing, encoding, line-size limits, timestamp and
severity rules, bounded memory/fuel, and defined malformed-input behavior. It has
two independently approved connections. There is no automatic conversion that
silently introduces a new recipient or grants authority to an adapter.

## Authority to use versus authority to wire

Each proposed connection requires:

1. Authority to reconfigure this binding, held by the controller.
2. Approval to release this source's output to this particular recipient.
3. Approval for this destination to accept this source and operation.

The latter approvals come from established resource/installation/session policy,
not self-approval by untrusted endpoints. They need not involve a prompt for each
connection. Curated defaults can attach stdlog to logstore and audio to the mixer
at launch. A manifest requests authority; declaring an outlet does not approve data
disclosure, collection, or reconfiguration. Discovery and inspection are scoped.

A UI requests a change with its own scoped authority. Human identity, clicking
Allow, possession of outlet and inlet names, and compatible types do not supply that authority.
Audio routing authority must not silently include release to a network uploader.
Every fan-out edge needs its own approval before granting access to shared memory.

These are constraints on existing capability issuance, not a second permission
hierarchy. The kernel protects IPC authority and grants; resource services enforce
the permitted operation and recipient scope. Policy evaluation is setup/control
work, not a round trip for each audio sample or diagnostic record.

Wiring authorization alone is not whole-system information-flow control. A
program already authorized to read data and transmit elsewhere may copy it. Keep
adapter authority narrow and do not claim the connection graph proves absence of
exfiltration through all other authorized channels.

## Identity, approval and atomic handoff

A stable app-facing outlet or inlet handle need not imply a permanent intermediary copying
every payload. The control plane can establish direct shared-memory transport.
Connector references (an inlet or an outlet) identify a process instance (not just a recycled PID), local connector,
and generation. Binding references identify a registry object and current
generation. Selected profiles, endpoint identity and direction are part of the
approved request; changing a buffer budget also requires new admission.

The implemented `CuBit.Protocols.Stream_Connections.Check` accepts resolved inlet and outlet
descriptors and three trusted approval records. Every approval is bound to the
controller instance, binding identity/generation, both endpoint references, and
their selected profiles/directions. Wrong or absent authority is rejected before
type/profile diagnostics. Invalid references, wrong direction, or incompatible
profiles are rejected even with all three approvals. Unknown references default
to zero and cannot become an allowed connection.

This pure function cannot authenticate evidence or know what is currently live.
A future registry must resolve kernel-backed authorities, enforce issuer scope,
pin immutable descriptors, reserve resources, and validate current generations
atomically at commit. Successful commit advances the binding generation without
wrap/reuse. Old approval records cannot authorize the next binding state.
Issuer, process and registry restart epochs must not resurrect stale authority.

Reconfiguration is prepare/commit/retire, not simply replacing a pointer:

- Prepare the approved new transport without exposing data to an unapproved peer.
- Commit routing at a defined record/frame/request boundary, with a new generation.
- Retire the old transport only after its buffer ownership obligations complete
  or are safely revoked. A lease timeout alone does not permit buffer reuse.

Logging can switch at record boundaries with visible gaps; audio needs an admitted
frame-boundary handoff and underrun policy. HTTP routing should ordinarily send
new requests to the new backend while admitted old requests complete. HTTP request
routing also needs response/reply ownership, cancellation and retry semantics;
it is not modeled fully by a one-way stream alone.

## Unconnected inlets and outlets and delivery

Define required versus optional attachment and the unconnected behavior explicitly:
reject before accepting, buffer within a budget, or discard with loss accounting.
Do not silently wait forever or silently weaken a lossless contract. Diagnostics
normally should not block application progress; durable security auditing may
require a different admitted contract and must not be represented as best-effort
diagnostics. Exact profile matching covers the current delivery-policy subset;
clocking, ownership, transport and whole-resource admission are still additional
requirements before real connections can be declared compatible.

## Provenance and validation

Producer timestamps and message contents are claims. Collector-observed time and
authenticated producer identity belong to a trusted envelope. An adapter becomes
the authenticated immediate producer of its output. Preserving an upstream origin
requires verifiable transformation/provenance evidence, not copying an origin PID
from payload text. The transformation must remain visible to observers.

CCL can check types statically. Native or malicious peers still require bounds,
encoding and schema validation on stable bytes at the receiving boundary; a schema
advertisement is not evidence that contents conform. A matching schema ID does
not establish the provenance of the service advertising it.

## Incremental implementation

### Implemented single-binding lifecycle

`CuBit.Protocols.Stream_Bindings` is now a portable SPARK ADT owned by one
serialized controller. A binding's identity is immutable; its generation starts
at one. Prepare checks exact-bound approval and the current binding reference,
then issues a non-reused transition ticket without changing the active route.
Commit checks the actor/ticket, fresh approval and resource readiness before
advancing the generation. If replacing an active route, the old transport enters
retirement. Abort also requires retirement of staged resources. Retirement ends
only on a matching ticket and trusted quiescence acknowledgement.

Only one preparation or retirement can be outstanding per binding. New preparation
is rejected until retirement completes. Separate transport tickets retain the
identity of the actual old resources: the handoff ticket and the old transport's
creation ticket are not interchangeable. Failed transitions preserve all state.
Generation/ticket exhaustion fails closed rather than wrapping.

This is executable state logic, not a live manager or transport implementation.
Resource readiness/quiescence are trusted adapter inputs; no mappings are created
or revoked by this ADT. Its Inspect function is internal and does not grant public
discovery. Active route metadata retains the request's approval generation; the
binding's current generation is returned separately by Reference. The adapter
must scope tickets by binding identity and its own registry lifetime, authenticate
process instances, revalidate live endpoints and approval at commit, and arrange
serialized access. There is no independent registry restart, forced disconnect,
timeout recovery, hard revocation or distributed atomic-commit implementation yet.

One-shot commands use typed calls, not a stream-specific signal subsystem; see
[typed commands and events](typed-commands-and-events.md).

1. Pure admission model, single-binding lifecycle and hosted tests (present).
2. Authorized log API migration using the initial LogRecord schema/codec; establish
   reader/publication separation and native rejection tests.
3. Single-host binding registry with atomic generation checks and bounded resource
   admission; app-facing emit/read API and automatic approved startup attachments.
4. Connect the hosted text-to-log adapter to a native end-to-end demonstration.
5. CCL inlet and outlet descriptors and connection operations using the same implementation.
6. Inspection/editing UI, then domain-specific audio and request-routing handoffs.

No live log API has been removed by this preparatory work. The isolated log broker
and admission model do not yet implement these bindings or increase live debugging
output. See [connection tests](../tests/stream-connections/README.md).
