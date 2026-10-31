# Lid observations owned by ACPI

ACPI_Lid identifies PNP0C0D and evaluates only the selected device's fixed _LID
member through a trusted service-selected evaluator. A sampled zero is Closed;
any sampled nonzero value is Open. Missing, failed or uncertain results remain
Unknown. Discovery requires device initialization; a description is not hardware
access authority. The evaluator may perform firmware effects and must use the
owner's admitted Field context, with no upward or peer synchronous IPC callback.

ACPI_Bootstrap.Lids.Cached separates collection, notification fanout and sample
publication. ACPI_Requests.Lids connects that cache to the actual Requests-owned
Bootstrap/Core without exposing the mutable arena. It supplies Bind, Prepare,
Observe_Claimed, Publish and pure Read operations for a serialized service owner.
Keep one Cache with one Server for its entire lifetime and use a single clock
epoch/unit. The native owner chooses bounded budgets, refresh times and policy.

Requests can retain a notification after removing it from Core's FIFO.
Observe_Claimed processes that exact Server.Staged_Notification once per active
receipt, without claiming, draining or acknowledging it. Read suppresses cached
freshness while the FIFO is nonempty or a retained receipt is unseen; publication
also rejects unseen retained events. The event-loop owner must fan out to EVERY
local consumer before acknowledging the broker receipt. Do not ACK first and
try to reconstruct the event later from client-supplied words. Uncertain routing
must be reported through Events_Healthy=False and retry policy remains separate.
No arbitrary event, method name, namespace node or register address is accepted
by this owner API. Observer clients receive copied cached samples, not _LID
invocation authority.

Hosted real-AML tests, including two cache consumers, are in
[acpi-lid-owner](../../../../tests/acpi-lid-owner/README.md). The fixture method
counts invocations and emits Notify on its third call. Checked/release tests cover
queued and staged notifications, receipt preservation/deduplication, publication
freshness, budget failures, clocks and unavailable hardware. Semantic-only checks
instantiate the bridge with the actual native-clock/runtime API snapshot. These
are not native execution, laptop validation or new full SPARK proof.

Native SCI/GPE/EC delivery, admitted region providers, event-loop fanout hooks,
typed cached-result IPC/subscriptions and separate power-policy consumers still
need integration. The existing native entry does not yet invoke this bridge.

ACPI_Requests.With_Notification_Routing supplies the request-boundary ordering
for this fanout. Bind its Route_Local callback to every local consumer's
Observe_Claimed/invalidation operation, and use its Handle for all Claim/ACK
requests. It routes immediately after successful Claim and requires successful
fanout before canonical current ACK. A failed callback returns Incomplete and
retains the event; repeated retired ACK remains idempotent. Invalid/unauthorized
requests preserve protocol outcomes without invoking local routing. The
service owner selects an optional provisioned Base_Handle, which must preserve
the existing Requests protocol. Callbacks must be bounded, idempotent across
partial retries, non-reentrant and free of AML/IPC/reset. Native wiring remains
unfinished; calling the underlying handler directly bypasses this guarantee.

The shared observation cache now has a separate scoped proof milestone:
[acpi-observation-cache](../../../../tests/acpi-observation-cache/README.md).
Its actual owned lid/power record types live in ACPI_Observation_Values; driver
state names are aliases/constants and both Bootstrap caches use those exact
record types. Descriptive values carry no interpreter identity or authority.
The executable cache policy is unchanged; Ghost observations and exact contracts
specify initial state,invalidation,stale publication preservation,exhaustion,
timestamp normalization and freshness/no-payload outcomes. Each level1/2 gate
proves14checks plus16flow/termination checks over23SPARKentities,with52ValidCVC5
goals,no explicit Assume/skip and no external claim dependency. This proves
these two cache instances,not the adapters,notification fanout,AML or arbitrary
other Sample_Type instances. Owner serialization,admitted callbacks and one
clock epoch/unit remain integration assumptions. Native/hardware execution
remains unfinished. The small pure-value proof avoids dragging mutable Core
into the payload proof; the prior full-context attempts timed out at180s
during Global-contract generation,not cache verification or a compiler crash.
