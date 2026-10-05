# Typed IPC and Stream Contracts

Status: foundational metadata implemented; enforcement migration in progress

The first implementation includes the pure `CuBit.Protocols` package,
schema-bound `streamCreateTyped`/`streamWriteTyped` producer APIs, and a shared
contract used by both ends of the CCL increment test protocol. Typed writes fail
closed when their schema identity, version, or fixed wire size differs from the
stream declaration. Fixed-size and bounded-size schemas are distinct, so
variable-length entries retain an explicit maximum. Typed subscription now
validates identity, version, sizing mode, and size bound before creating the
read-only grant. Broker enforcement remains open.

CuBit IPC currently carries a numeric operation label and four untyped words.
Named streams add a 16-bit entry tag, but that tag does not globally identify a
schema or version. Services therefore can compile while disagreeing about the
meaning of the same bytes. CCL cannot safely move or borrow an owned value
through such a boundary.

`CuBit.Protocols` introduces a pure, SPARK-compatible metadata vocabulary shared
by IPC operations and streams. It is data only and does not change the syscall
ABI or grant authority.

An operation contract declares:

* a stable interface identity, operation identity, and protocol version;
* request and response schema identities, versions, and fixed wire sizes;
* inline-message, shared-grant, or typed-stream transport;
* copy, move, borrowed-ro, or borrowed-rw argument transfer;
* success and failure disposition verbs and effects; and
* a bound on outstanding operations.

Schema and interface identities are stable, nonzero integers assigned by the
interface-definition toolchain. They are not Ada type positions, addresses,
hash-table slots, capability slots, or syscall numbers. Generated packages will
emit these constants alongside Ada and C wire types. Changing a wire layout
requires a new schema identity or version. A schema is either fixed-size or has
an explicit maximum wire size. The pure `Valid` and `Compatible`
functions can be evaluated for constant declarations during compilation and can
also be reused unchanged by loaders and brokers.

## Authority boundary

Protocol metadata answers “what bytes and ownership transition does this
operation require?” An endpoint/session descriptor answers “which particular
service may this process ask?” The ELF manifest and installation/session policy
remain the only sources of initial authority. Matching a protocol identity must
never mint or discover an endpoint.

The kernel should continue to enforce endpoint identity, rights, grant mapping,
reply-cap uniqueness, and process isolation. Schema negotiation and application
protocol state belong in generated userspace stubs and the session broker, not
in ring 0. The kernel may eventually carry an opaque protocol identity on an
endpoint for defense in depth, but it must not interpret application schemas.

### Discovery descriptors and granted bindings

CCL's first bounded `Interface_Catalog` makes this separation executable. A
catalog view contains immutable operation contracts identified by a canonical
descriptor SHA-256 digest, interface version, and operation ordinal. It contains
no capability slot, process ID, driver ID, endpoint, or runtime host binding.
Catalog access therefore permits discovery and static checking but grants no
operation authority.

The compiler emits unresolved VM imports plus descriptor-pinned linkage. A
trusted host constructs a separate `Granted_Bindings` view from handles it
already possesses. Linking checks every descriptor identity and import contract
before changing any binding; a missing grant or substituted contract rejects
the complete admission. The current numeric binding remains a temporary local
adapter namespace. On CuBit it should become an opaque handle association backed
by the existing kernel capability table.

## Ownership rules at calls

The common runtime request lifecycle and its separation from service adapters
and CCL ownership are described in [IPC client lifetimes](ipc-client-lifetimes.md).
Config and filesystem channels already share the transport-independent tracker.

* `copy`: only an unrestricted argument may be duplicated; no completion verb.
* `move`: the caller loses the value when submission is accepted. Both success
  and failure must state whether it was consumed, returned, or transitioned.
* `borrowed-ro`: the caller retains ownership but cannot move or mutably borrow
  the value until completion. Both outcomes return the borrow.
* `borrowed-rw`: the same rule applies and no other borrow is permitted.

Async cancellation is an outcome, not disappearance. Every accepted move or
borrow must eventually produce exactly one terminal completion whose declared
verb closes the ownership obligation. Failure to enqueue a request leaves the
argument with the caller.

Cancellation support is part of the operation contract. An operation may be
not-cancellable, best-effort cancellable, or guarantee that an accepted cancel
request eventually produces a cancellation completion. None of these policies
permits the caller to reclaim a moved value or borrow when cancellation is
requested; ownership changes only on the terminal completion. A normal success
or failure may race with a best-effort cancellation request and remains a valid
terminal outcome.

## Typed stream subscription

`OP_STREAM_SUBSCRIBE_TYPED` uses the existing four message words and therefore
requires no kernel ABI change:

| Word | Meaning |
|---:|---|
| 0 | stream identity |
| 1 | 64-bit schema identity |
| 2 | schema version |
| 3 | low 32 bits: size/bound; bit 32: bounded rather than fixed |

All remaining bits must be zero. The producer compares the request with its
creation contract before calling `createGrant`; any mismatch returns an error
without mapping memory. The successful reply retains the existing grant ID,
cursor slot, capacity, and initial cursor fields. `OP_STREAM_SUBSCRIBE` is the
legacy untyped operation and must not be selected implicitly by typed callers.

## Calls and streams share one interface model

One-shot "do this" commands are ordinary typed operations, preferably async
calls when execution results matter. They are discoverable through an authorized
catalog view and separately invocable through an actual grant. Events/wakeups do
not promise command completion. See [commands and events](typed-commands-and-events.md)
for the existing primitive support and unfinished live-discovery boundary.

Design decision: typed calls and typed streams are complementary interaction
forms advertised by the same interface-definition system, not separate RPC and
stream security systems. A subscription is an authorized operation that returns
a scoped stream/session handle; it does not grant access to every event from
the provider. Illustrative signatures, not accepted CCL syntax today:

| Interaction | Example | Contract |
| --- | --- | --- |
| Call | `set-volume(Percent) -> Result` | One request and its terminal outcome |
| Subscription | `watch-volume() -> Stream<VolumeChanged>` | Discrete updates with explicit delivery and lifetime rules |
| Bulk stream | `open-audio(AudioFormat) -> AudioSink` | Sustained bounded transfer with explicit buffer ownership |

The shared definition may supply an embedded binding, a published IPC adapter,
or both, as described in [CCL composition](ccl-interactive-composition.md#one-interface-definition-explicit-exposure).
Exposure is not a grant. Discovering a stream, opening it, reading, writing,
inspecting contents, and delegating access are distinct actions requiring the
appropriate existing endpoint/session authority. They are rights on the same
authority model, not a new kind of ambient stream permission.

### Typed object values and batches

The element type of a CCL object stream is a **schema-pinned CCL data type**,
not an Ada record layout, native `CCL.Objects.Image`, pointer, capability slot,
or service-local type number. A provider and consumer must both have an
authorized schema binding for the same complete nominal definition. The stream
descriptor carries a generated wire-schema identity/version/maximum encoded
size; the CCL descriptor retains the complete schema identity used to validate
the decoded value. Neither identity grants subscription or inspection authority.

The canonical wire value for ordinary CCL data is the bounded, definite CBOR
object encoding already used by Config persistence. The producer validates an
owned object against its approved binding before encoding. The consumer first
checks the stream profile, then strictly decodes and re-encodes canonically
under its own approved binding. A native image is deliberately not a stream
wire format: it includes local ABI choices and must never become a remotely
interpretable object representation.

`Stream<T>` means a bounded ordered sequence of individual immutable values of
`T`. It is not an array and it does not imply an unbounded queue. Where batching
is useful, the contract names another generated schema:

```text
Stream<Preferences>
Stream<Array<Preferences, 32>>
Stream<ConfigChange<Preferences>>
```

`Array<T, N>` is a bounded product value with `0 .. N` elements and an explicit
maximum encoded byte size. It has a distinct schema identity from `T`; a
consumer of `Stream<T>` cannot accidentally receive batches, and a consumer of
`Stream<Array<T, N>>` cannot receive an oversized batch. The same rule applies
to nested `ConfigChange<T>` outcomes. Static element/batch typing is separate
from delivery semantics: each stream still declares capacity, backpressure or
observable-loss policy, close/drain behavior, ownership transfer, and timing.

For local high-rate streams, ring-buffer slots can remain shared granted memory
and avoid an additional transport copy. The bytes in each accepted slot are
still canonical CCL data and immutable until the declared return/reclaim event.
A trusted generated adapter may use a fixed-layout wire encoding for a specific
schema only when it has the same validation and version guarantees; it may not
reinterpret a native process image as that layout.

Config will expose ordinary one-shot collection operations and separately
advertised change/snapshot streams over this model. For example, a collection
for `Preferences` may offer `get`, `set`, and an authorized
`watch -> Stream<ConfigChange<Preferences>>`. Creating or subscribing to that
stream is an explicit authorized operation; possessing a collection handle,
knowing its type, or being able to read one snapshot does not automatically
authorize watching all future changes.

### More than an element type

Applications declare typed inlets and outlets; authorized runtime configuration wires them.
The [stream wiring design](stream-wiring.md) describes stable app-facing inlets and outlets,
separate reconfiguration/release/acceptance approvals, explicit adapters, and
generation-bound handoff. Its pure admission model is preparatory, not a live
replacement for today's stream protocol.

A stream contract must declare:

* **Shape:** element schema/version, fixed size or maximum encoded size, and
  stream direction. Audio also needs an explicit sample format, channel layout
  and rate; timing metadata must identify its clock/timebase.
* **Delivery:** ordering and sequence/gap semantics, whether loss is permitted,
  and whether intermediate elements may be coalesced into a latest value.
  Reliable delivery under specified conditions does not imply exactly-once
  effects after a reconnect or retry.
* **Flow control:** bounded capacity in entries/bytes, outstanding-buffer and
  in-flight-handler limits, producer/consumer pacing, and explicit behavior
  when capacity is exhausted. Backpressure must suspend/defer work rather than
  block an unrelated service event loop. Lossless streams cannot silently drop
  accepted elements; inability to continue must have a defined failure outcome.
* **Ownership:** copy, move, borrowed-ro or borrowed-rw transfers; reserve,
  publish, acquire and return/consume transitions for bulk buffers; and the
  exact event permitting a buffer to be reused. Sharing storage does not remove
  these obligations. Fan-out requires explicit copy, safe shared read access,
  or a brokered distribution contract, never duplication of move-only authority.
* **Lifetime:** who owns the subscription, normal end-of-stream, producer and
  consumer close, drain versus explicit discard, failure, revocation and restart.
  Cancellation support is declared; closing a UI/client is not proof that
  accepted work or a borrowed buffer has finished.
* **Budgets and observations:** rate, memory, execution and latency requirements,
  plus observable overflow, gap, underrun and terminal outcomes. A requested
  deadline is not a guarantee unless the host has admitted the required resources.

Compatibility checks must cover these semantics as well as element shape.
Negotiation selects an explicitly supported profile within both parties'
bounds; it cannot silently turn a lossless stream into a lossy one, introduce
coalescing, enlarge a buffer budget, or strengthen granted rights. Material
contract changes require versioned, descriptor-pinned metadata.

Volume indicators may accept latest-value delivery. Discrete clicks require
bounded admission with explicit overflow rather than silent coalescing. Audio
needs explicit frame order, pacing and underrun/overrun handling, not an
unbounded queue or arbitrarily delayed retry. These are distinct contracts,
even if each is presented in CCL as a stream.

### Transport and security invariants

Local channels, native IPC grants and remote transport should preserve a
supported semantic profile, not pretend to have identical costs or failure
modes. Prefer amortized setup, bounded batching and granted rings/buffers for
bulk local IPC; do not require a payload copy or request/reply exchange for
every sample. Transformations such as audio mixing still produce new data.
The [audio path](audio-zero-copy.md) is the concrete reference, not a claim that
all current `CuBit.Streams` paths are already copy-free.

Grant validation, memory visibility, bounds and ownership transitions remain
mandatory on the fast path. A schema match does not establish that shared bytes
are immutable or well-formed: validate untrusted contents at a stable ownership
boundary and prevent concurrent mutation of data relied upon for safety. Static
CCL checking is not a substitute for protecting against native malicious peers.
Reusing or unmapping storage requires the real terminal/return condition, not
a timeout or an assumption that the peer stopped using it. See
[buffer lifetimes](ipc-buffer-lifetimes.md).

Remote adapters authenticate peers and explicitly proxy/delegate authority;
they never serialize local handles or grant addresses as usable remote rights.
Network transport necessarily has different copying and disconnect behavior.
Reject nontransportable contracts or require an explicit compatible adapter;
do not silently convert a local borrow into a remote copy or promise exactly-once
execution across failures.

### CCL composition and implementation boundary

Planned stream combinators and `|>` share the ordinary typed/effect/ownership
core with calls. A stream map invokes bounded work per admitted element; a
subscription may remain alive indefinitely without one infinite interpreter
invocation. Long-lived aggregation requires explicit bounded state and, where
appropriate, windows. A terminal fold of an unbounded stream is not a finite
expression. Connecting a graph edge neither subscribes implicitly nor grants
authority. See [pipeline elaboration](ccl-interactive-composition.md).

Today's implementation checks stream schema identity, version and sizing on
typed creation/write/subscription, alongside the existing service/grant
mechanisms. The full delivery/profile metadata, broker enforcement, generated
local/IPC adapters, CCL stream values/combinators and remote stream runtime
described above are still design work. Existing generation-tagged callback
queues and audio rings are useful reference implementations, not proof that
this entire model is implemented or formally verified.

## Migration order

### Implemented preparatory delivery-policy model

`CuBit.Protocols.Stream_Policies` is a pure SPARK child package shared by future
native and CCL adapters. Its discriminated delivery type separates lossless
ordered events, ordered events with explicit gaps, and latest-value updates.
Lossless variants cannot express drop-oldest/drop-newest overflow. Replacement
and dropping concern pending elements, never storage held by a consumer; when
there is no reclaimable pending slot, the runtime must reject before acceptance.
Lossy delivery requires observable gaps and disposal of any ownership obligation,
not simply incrementing a cursor over live granted buffers.

The model validates schema, payload capacity and in-flight limits, and compares
schema, delivery/overflow, exact capacity and normal-close policy. Matching is
deliberately strict: selecting a different supported profile is a separate
explicit step, not an implicit weakening in `Compatible`. Payload accounting
uses a non-modular integer type; slots include pending and consumer-held data.
Close distinguishes draining accepted elements from discarding pending elements
with a report; neither choice implies cancellation of in-flight work.

The hosted matrix/boundary tests and 14 focused SPARK checks pass; see
[stream policy tests](../tests/stream-policies/README.md). The unit compiles
against CuBit's native runtime. It does **not** extend the current
`OP_STREAM_SUBSCRIBE_TYPED` wire message, enforce these policies on existing
rings, or encode them in CCL descriptors yet. Ownership/transport, cancellation,
timing profiles and whole-resource quotas remain additional contract components.
Before migration, serialize a versioned complete profile, reject incompatible
subscriptions before mapping memory, and exercise enforcement with real streams.

### Remaining integration

1. Define shared protocol contracts for the CCL test host and one small service.
2. Add protocol identity and schema checks to generated/user-space call stubs.
3. Migrate remaining stream producers and subscribers to the typed handshake;
   retain raw-byte streams only as an explicitly untyped compatibility class.
4. Serialize the same import contract in `.cclb` and have CCL ownership
   verification model accepted submission and every completion branch.
5. Add manifest protocol requirements and make the process/session broker bind
   only compatible endpoints.
6. Generate Ada/C declarations, canonical codecs, CCL signatures, manifest
   requirements, fuzz targets, and SPARK contracts from one interface source.
