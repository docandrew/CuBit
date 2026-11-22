# Embedded Controller backend (not attached to native startup)

ACPI_EC_Transactions is the serialized byte read/write/event-query protocol
engine. ACPI_EC_Registers adds independently admitted named cell rights and
full-width owner/epoch matching. ACPI_EC_Endpoint defers replies using the
existing Field wire protocol while the backend loop steps the transaction.
Normal hardware waiting is not reported as an unknown completion.

Transactions/Registers/Endpoint are trusted backend code. EC_Methods is the
interpreter-side owner for exact local _Qxx resolution, one consumed invocation,
identity revalidation and retained-result cleanup. It has no hardware callback.
Native event transport must authenticate controller/epoch/token and bind the
corresponding independently admitted EC scope.

This is not an AML-installable authority interface.
Install/Revoke are privileged admission operations. Firmware declarations do
not grant rights. A request supplies a named ID and epoch, never a cell offset
or physical address. The native owner must authenticate full-width receive
metadata, save its reply channel before submission, and retain exact reply
correlation. It must serialize all entry points and instantiate exactly one
engine/catalog for one reserved controller. Do not call that engine directly
alongside the catalog. No peer callbacks are needed.

Raw command/data/status capabilities belong to the trusted backend. Owned must
attest actual dedicated-interface reservation or a currently held ACPI Global
Lock. Read/Write outcomes, non-reentry, termination and fresh microsecond clock
samples are trusted callback obligations. The engine polls only after a
separate postwrite sample plus at least one microsecond, covering delayed IBF
assertion. Each Step performs at most one register callback, with a finite
caller-supplied deadline and work budget. There are no unbounded internal waits.

Stop before submission has no hardware effect; stop after submission drains the
exchange within the same limits. Unknown outcomes retain controller/catalog
ownership, reject new requests and cannot be reset by acknowledgement. An
inherited busy input must become idle; unexpected output is not blindly drained.
Successful writes attest protocol consumption and idle status, not independent
readback or completion of a physical action. Failed reads publish no data.
Register ID/epoch history and operation tokens never wrap or silently recycle.
Privileged Submit_Query shares the same serialized engine/token sequence with
Field requests. Take_Query uses a separate completion route, so neither route
can steal the other operation kind. Query value zero never invokes _Q00.

Pending: resource discovery/admission, real Global Lock acquisition, native
callbacks and clock-unit conversion, saved-reply event loop, OperationRegion
binding, SCI/GPE delivery and authenticated native _Qxx dispatch, recovery/reconciliation, burst
optimization, wider SPARK contracts/proofs and native/hardware acceptance.
The transaction owner has a scoped state/timing/budget proof milestone in
tests/acpi-ec-proof. This does not prove the catalog, endpoint, method owner,
physical protocol or complete service. No native component definition or
source registration is changed by this draft.

See tests/acpi-ec for reproducible hosted evidence. Normative protocol reference:
https://uefi.org/specs/ACPI/6.6/12_Embedded_Controller_Interface_Specification.html

ACPI_EC_Boot_Table decodes copied ECDT metadata for early EC discovery. It
retains GAS/revision/UID/GPE claims, parses a rooted textual namespace path,
and separately classifies byte-register compatibility. Control precedes data
in ECDT, whereas the namespace _CRS resource order is data then control.
Neither result maps an address or grants hardware authority. GPE_Bit is a bit
in an FADT-described GPE block, not an IRQ/GSI. ECDT supplies no sharing/Global
Lock fact; do not infer exclusive ownership from its absence.

Native admission must establish actual backing/ownership, select register roles
by named IDs, bind the matching EC namespace device/UID, and acquire any needed
firmware lock before access. Read_Path and Check_Registers revalidate the bytes;
callers cannot supply a descriptor from a different table. Optional ECDT support
does not replace namespace _CRS/_GPE/_GLK discovery on machines without it.
The decoder is hosted-tested with independent ACPICA data-table fixtures; no
new parser proof or native boot acceptance is claimed.

ACPI_EC_Resources projects copied namespace _CRS buffers into data-first and
control-second byte-register claims. It uses existing typed resource decoders,
validates known IRQ/GPIO descriptors,refuses unknown descriptors and records an
optional GPIO-interrupt descriptor offset for later revalidation. Static IO,
fixed/legacy32 memory and untranslated address-space forms are handled within
the documented one-byte layout. Ten-bit IO decoding and memory writeability
claims are retained; they cannot justify aliases or write permissions by
themselves. Actual region backing,translated/source-dependent layouts and
controller/pin ownership still require independent admission.

Tests/acpi-ec-resources exercises actual AML _CRS evaluation,retained-buffer
capacity retry and twelve full ACPICA byte comparisons. Scalar _HID/_GLK/_GPE
and an ordinary package-shape helper are exercised,but the full production
discovery sequencer,budget/identity/cleanup ownership and native attachment are
not implemented by this projection. ACPICA's normal rejection of reserved
package-valued _GPE is retained as a reference gap,not an interpreter defect.

ACPI_EC_Discovery now owns a one-shot _HID/_GLK/_GPE/_CRS attempt with shared
AML budget,canonical binding revalidation and retained-result cleanup. It
accepts typed resource claims only after the whole sequence and cleanup succeed.
Copy_Resources retries a retained _CRS buffer without evaluating AML; Close
releases only retained results in the owning Core. Field_Uncertainty separately
latches lower provider obligations even when Pending (result roots) is false.
No description grants hardware access; actual backing/alias ownership,Global
Lock and GPE-block identity require independent admission. Is_Current checks
namespace bindings,not unchanged firmware values or physical ownership.

The new tests/acpi-ec-discovery gate exercises actual alias-backed package _GPE
using normal compilation and ACPICA execution. This supplies independent alias
form coverage without changing the earlier direct Name/Method rejection facts.
Native startup/ECDT UID matching,_STA/_INI/_REG/provider orchestration,locking,
event delivery/rearm/reconciliation and formal discovery-owner proof remain
pending. Existing EC transaction-owner proof does not extend to this owner.


## Trusted EC cell/provider association

ACPI_EC_Field_Bindings builds a remote descriptor from the same independently
admitted cell description used by the concrete EC catalog. It checks full64-bit
expected owner,nonzero ID/epoch,rights/mask consistency,bounded EC region and
cell containment. Returned IDs,epoch,permissions and mask remain exact; region
base plus byte offset equals the cell. No builder call installs admission,
contacts a peer or accesses hardware. Native code must independently establish
admission currency,matching controller/endpoint contexts and protocol ownership.

The joined EC Field test uses this builder for actual AML/lid/read/write flow.
It also covers all256cells in full and singleton regions,maximum64-bit IDs and
epochs,upper-owner-bit mismatch,read-only/masked rights and invalid extents or
cell association. Hosted checked/release1375joined checks/profile plus the
existing1063geometry and datum/plan regressions pass. Function postconditions
are exercised in checked builds; no new SPARK proof is claimed for this helper.


The native/ACPI_EC_Native_Calls adapter now owns deferred reply correlation
through the existing typed runtime API. It remains unattached to a launched
backend. See native/README.md and tests/acpi-ec-native-calls. Hosted1487joined
checks/profile and actual-runtime zero-object semantic checks pass; no native
execution or new proof is claimed. Native admission/ownership,launch,scheduling,
clock conversion,_REG/provider and GPE events remain integration work.


ACPI_EC_Event_Publisher now retains a nonzero completed query notice before a
later bounded nonblocking enqueue. Backpressure retains the exact identity and
uses finite delivery episodes; explicit retry reuses that notice without a
new hardware query. Unknown enqueue (or callback exception) quarantines delivery
before any retry and blocks new queries. Normal query occupancy is released
before publication,allowing later AML handler EC Field readback. Query0/no-event
never denotes _Q00. Safe failed-query resumption does not reset hardware recovery.

Internal event types are descriptive,not a wire ABI or source capability.
Configure is a one-attempt trusted source/epoch binding. Native sender/route
authentication,receiver deduplication and scope admission remain integration
work. Hosted1637joined checks/profile include query→queue→actual _Q01→EC Field
readback→Notify LID0. Fresh repair-disabled ACPICA with simulated zero-filled
region storage independently observes handler count1,field0,lid0. No new proof,
native execution or physical EC/IRQ acceptance is claimed. See tests/acpi-ec-events.


ACPI_EC_Event_Receiver now owns trusted source/epoch/sender-to-current-Device
bindings and per-epoch ordered token high-water marks. Tokens are consumed
before handler lookup/AML; exact duplicates,conflicting event numbers and old
tokens perform no evaluation. Used route slots/epoch tombstones never recycle.
Revoke disables admission while pending cleanup and Field uncertainty remain
held. Foreign Core refuses cleanup; re-enrollment needs a greater epoch and
no retained obligations. Install validates metadata,not _HID/provider readiness
or hardware/source authority; native controller must establish those first.

One serialized receiver domain and per-controller ordered transport are required.
Current kernel ring source was reviewed for per-publisher FIFO,not newly proved
or natively tested. No wire decoder/event endpoint is installed. Hosted1692joined
checks/profile cover actual query→queue→receiver→_Q01→EC Field→Notify,identity
and epoch rejection,duplicates/conflicts,cleanup retention,revocation and Field
uncertainty beside overcharge. Independent ACPICA confirms normal handler/lid
results and alias-backed buffer return. No new proof: the selected attempt
stopped in the already documented AML dependency global-contract diagnostic/
internal error before receiver analysis; no compiler research or bypass pursued.
See tests/acpi-ec-receiver. Native integration and full verified goal remain open.


ACPI_EC_Event_Policy now implements the actual receiver binding/replay state.
Its private state prevents direct field updates by receiver code; all state
changes use proved Bind/Revoke/Claim/Guard/Settle operations. Matching settlement
clears the provisional callback guard while committed uncertainty remains sticky.
Level1/2 each prove10checks including5functional posts,13SPARK entities,11flow
results. This closes the pure policy transition scope,not namespace provenance,
Core/AML correctness or native behavior. Whole-receiver proof still encounters
the known AML dependency diagnostic. Receiver integration1692/profile remains
green; see tests/acpi-ec-policy for exact retained evidence and reproduction.


## Exact receiver claim reconciliation

Receiver.Receipt returns the exact originally claimed notice,or absent for an
unused/rejected delivery. It does not attest handler success,cleanup or hardware
quiescence. Redispatch of a consumed Delivery cannot produce a receipt for a
different payload. A fresh duplicate Delivery has no receipt of its own.

Publisher.Confirm_Claim accepts only Delivery_Unknown,an exact retained notice
(controller,epoch,token,event number),and the full authenticated sender matching
the independently provisioned Expected_Consumer. Default No_Process disables
receipt acceptance. Sender must come from kernel receive metadata; these are
internal APIs,not a wire format or a source of hardware authority. Receipt loss
still retains the notice; reliable receipt transport/retrieval remains native
integration work. All entry points must remain serialized and nonreentrant.

Acceptance releases publication metadata only. It performs no EC transaction,
enqueue or AML invocation,and never clears receiver Field uncertainty,result
roots or catalog hardware recovery. Ready means publication can progress; the
catalog still rejects requests while retained for recovery.

Hosted checked/release:1705joined+1063geometry checks/profile.
Cases include upper sender bits,altered controller/epoch/token/number,repeated
receipt,an unclaimed duplicate,and confirmation beside sticky Field uncertainty
and hardware recovery. Fixture bytes unchanged from the prior ACPICA reference.
No new proof,native execution,physical hardware or native event endpoint claim.


ACPI_EC_Boot_Binding associates copied compatible ECDT metadata with its exact
namespace Device path and one bounded local canonical _UID evaluation. Requires
full integer equality,no narrowing,default,ancestor fallback or substitute search.
Retains failed compound-result cleanup and independent Field uncertainty; no AML
replay on Match/Close. UID_Matched is not PNP0C09/readiness/resource or hardware
authority validation. Early provider admission and native startup remain separate.
Hosted167checks/profile with normal ACPICA table/AML fixtures and independent
repair-disabled UID reference; no new proof/native/hardware claim.
See tests/acpi-ec-boot-binding.


ACPI_EC_Boot_Resources compares re-decoded copied ECDT/_CRS byte-register claims
by data/control role,space and full address. IO10-bit decode and read-only memory
remain explicit review outcomes; all addresses are checked before review.
Claims_Agree grants no UID/namespace/IRQ/GlobalLock/readiness or hardware rights.
Caller must establish same-device/current resource provenance independently.
Hosted292checks/profile and12whole _CRS byte comparisons with ACPICA pass;
no new proof or native/hardware validation. See tests/acpi-ec-resource-match.


## Catalog progress scheduling

Registers.Next_Progress(Now) is a serialized,read-only advisory observation of
the catalog's single accepted operation. No_Work means no catalog advancement
or uncollected completion,Run_Step means call the owning adapter's bounded
Advance,Wait_Until carries a monotonic microsecond timestamp no later than the
accepted request's original deadline. It exposes no hardware or clock callback.
Nonwait recommendations have At_Time=0. Repeated observation does not move wake
time,consume work budget or perform IO. Newly accepted work replaces the cached
deadline only after Engine.Start succeeds.

After writing,Run_Step allows the engine's fresh postwrite timestamp sample.
The next observation exposes its earliest safe poll. At/after deadline,Run_Step
allows timeout/collection rather than sleeping past it. Terminal completions
also need the owning route's Advance/Collect; calling Step alone cannot deliver
them. Field saved-reply and query/event owners must remain serialized and use
their own completion routes. Engine checks its own fresh clock,ownership and
budget even if an advisory observation was stale or ignored.

No_Work is catalog-specific,not global quiescence. Delivered hardware recovery
can remain occupied and retained while Next_Progress says No_Work. Saved replies,
pending publication/receipts,AML roots and Field uncertainty have independent
lifetimes. Pending publication work must run even when catalog progression is
empty. Recompute advice after incoming work; OS wait/deadline conversion,fairness
and polling cadence remain the native owner's job. This adds no native loop,
launcher,capability or hardware access.

Hosted startup/Field/native-reply/event integration now drives virtual time from
these recommendations:1868joined+1063geometry checks per checked/release profile.
Original EC suite passes24375engine+1043catalog+645query checks per profile.
Deadline/wake tests at ordinary and maximum clock values verify no IO on repeated
observations,immediate terminal collection,and No_Work beside retained recovery.
No new proof or native/hardware execution claimed; existing engine source and
its proof inputs are unchanged.


ACPI_EC_Backend_Progress now serializes matching catalog/native-call/publisher
instances. One bounded owning Field/query/publication action; no direct engine
step,internal wait or peer/AML callback. Alternates runnable Field/publication,
uses safe catalog wake,and retains fault/conflict/unknown/stalled obligations.
No_Work is not quiescence; Field completion report is action-specific. Hosted
1954joined+1063geometry/profile includes fairness under backpressure,unowned
query refusal and modeled invalid saved-reply quarantine. Callback metadata,
hardware/clock/queue behavior remain assumed/modeled. No new proof,native
launch/ABI/authority or hardware validation. See tests/acpi-ec-backend-progress.


ACPI_EC_Native_Clock now pairs the existing raw kernel microsecond read/sleep
clock,avoiding unestablished published/fallback-to-IPC-ms epoch conversion.
Clock policy/adapter validates representation,caps pause requests to1000us and
uses a separate finite pause-attempt budget. Returned pause is not reached time;
owner must resample and supervise exhaustion without resetting obligations.
70056hosted checks/profile and244-source actual-runtime zero-object semantic
profiles pass. No new proof,native timing/launch/hardware acceptance or receive
loop. See tests/acpi-ec-clock for assumptions and remaining integration.


## Successor bounded wait supervision

ACPI_EC_Backend_Waits combines one bounded pause and one owning adapter check,
so a stopped clock cannot defer engine budget forever. Pause failure/exhaustion
requests stop without claiming cancellation or clearing recovery. New2021joined+
1063geometry/profile cases cover advancing/frozen clocks and exactly-once saved
reply completion. See tests/acpi-ec-backend-waits for assumptions/integration gaps.
Its246-source actual-runtime semantic gate covers the changed dispatcher and
real clock callback. Earlier native-semantics records remain historical snapshots
for that dispatcher; no old hash record is relabeled as current. No new proof,
native launch,hardware execution or recovery reset is claimed.


## Receiver-domain Fatal recovery

A normally returned Fatal_Requested result now latches receiver recovery and a copied Fatal diagnostic. The original delivery remains consumed and reports Invocation_Failed. Fresh deliveries return Recovery_Required before method lookup or execution; new enrollment returns Receiver_Recovery_Required. Already-consumed deliveries retain their existing non-replay result. Revocation and retained-result Close remain callable and cannot clear recovery. No receiver reset API is added.

Recovery_Needed and Fatal_Diagnostic are local owner observations, not wire selectors or hardware authority. This latch affects one receiver instance: the native owner must stop every other AML caller in its domain after observing it. Direct Core calls and separate receiver instances are not stopped by this package. Current native source enrollment remains unfinished. Existing Field-uncertainty routing is unchanged.

The hosted composition passes 700 checks across checked/release and 32/64-bit AML. Pinned ordinary iASL compiles the fixtures. This is an explicit host Fatal policy, not ACPICA interpreter equivalence; no new SPARK proof or native/hardware execution is claimed. Exact current evidence: tests/acpi-native-notifications/FATAL-RECOVERY-EVIDENCE.json.
