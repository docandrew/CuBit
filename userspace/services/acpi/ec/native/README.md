# EC native deferred-call owner

ACPI_EC_Native_Calls belongs in a separate privileged EC backend,not the AML
service. Instantiate it with one concrete catalog/endpoint and serialize all
entry points with the event owner. Begin_Call must receive the exact kernel-
authenticated sender/message and backend-selected finite deadline/work budget.

It saves the current reply before submission,retains token/owner/register/epoch,
and answers a matching completion once. Busy callers use their current reply
without replacing the old saved authority. Failed Save_Reply submits no work;
lost Answer consumes authority and never replays the transaction or reply.
Advance performs one bounded catalog step; the outer scheduler still owns
clock/wake policy,fairness,query delivery and event serialization. Correlation
faults quarantine the owner without sending a different caller's completion.

Pending describes saved replies only. Recovery_Retained independently reports
catalog hardware recovery,even when Pending is false. Completion hardware/effect
fields apply only to an attributable completion. Neither reply delivery nor
absence of pending replies releases/reconciles lower hardware obligations.

Runtime callbacks must return normally and never reenter; saved answers consume
authority even on delivery failure. Native admission,current controller/endpoint
identity,actual backing retention and continuous reservation/GlobalLock remain
trusted integration obligations. No component,manifest,grant or launcher is
added. Existing AML service remains immutable-snapshot/observer only.

Tests/acpi-ec-native-calls exercises actual AML/Field/provider/catalog/engine
with modeled hardware,clock,ownership and saved replies. Two semantic profiles
instantiate default callbacks against actual runtime specs using -gnatc,with
zero object output. Neither test is native execution or a new SPARK proof.


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


## Actual-runtime semantic gate

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-backend-progress/semantic.sh

Use the pinned project shell. --source-root DIR and --unit-root DIR permit
isolated source snapshots. This runner is separate from the behavioral hosted
suite: it copies222actual runtime/capability specification files with the real
EC/publisher/native-call/dispatcher declarations and bodies,238sources total.
No IPC fixture declaration is selected. Default Save_Reply/Answer/reply callbacks
are instantiated,with actual opaque Process_ID sender/consumer values above32bits.

Checked/release semantic profiles compile with -gnatc and strict warnings,inspect
the emitted ALI switches,and require zero object/binder output. No executable is
linked or run. native-semantics.json retains the original commands,input hashes
and source origins. This is declaration/type compatibility,not runtime behavior,
whole native compilation,launch permission,actual message delivery,clock-unit
conversion,admitted hardware,stack fit or a SPARK proof. Hardware callbacks in
the semantic-only main are refusing placeholders; that main must never execute.

The first private attempt found a useless test-main assignment; corrected while
strict warnings remained enabled,then both profiles passed. Production code
unchanged. Native service remains snapshot-only/hardware-pending; provider
admission,backend launch,receive/wait/event routing and hardware acceptance remain.


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


## Serialized request policy owner

ACPI_EC_Backend_Controller joins the existing call/query/progress/wait owners
under backend-selected finite request and pause budgets. Use it as the sole
submission/progress entry for its actual package instances; do not mix direct
Calls/Events submissions with its pause accounting. Read_Clock must return
normally, stay bounded/nonreentrant and share the engine clock epoch. Callbacks
and backing/ownership remain external obligations, not authority inferred by
this package. Invalid policy/clock/overflow replies deny the current call
without saving it. Only a newly accepted transaction replenishes pause budget.
A failed outer clock requests stop and one fresh engine check; zero used for
advisory scheduling never replaces the engine hardware clock. Postwrite work
may drain or remain in recovery. This package creates no process, interrupt
subscription, hardware authority or release/reset path.

Hosted integrated checks and actual-runtime declaration evidence are packaged
in tests/acpi-ec-backend-waits. Full service/interpreter proof and native EC
provisioning, loop integration and hardware acceptance remain unfinished.


## Typed receive and serving turn

Controller.With_Receive is an opt-in process-lifetime receiver over the same
serialized controller. Actual native queue adapters poll completion, event and
service lanes separately. Each turn handles at most one event, one bounded
controller advance and one service request before an eligible idle wait using
Wait_Forever. Active hardware waits retain the existing bounded microsecond
pause; no epoch conversion or synchronous peer callback is introduced.
Default event/control hooks admit nothing. Trusted bindings must authenticate
source/receipt/control authority independently; a control hook that declines
a request must preserve its current reply authority for Begin_Call.

Serving stops/faults latch and retain the backend, saved replies, backing,
ownership and unknown recovery/publication obligations. Return from Serve is
not permission to exit, release leases, reset or replay. Use one receive child
per controller and arrange a separate trusted lifecycle/drain path. This does
not launch a backend, establish hardware/interrupt authority or select hardware
startup in the AML service. Tests/acpi-ec-receive packages 544hosted checks in
36processes, both profiles, and actual-runtime declaration checking for252
sources/222runtime specs with zero objects. No new proof, native execution or
hardware acceptance. Full service/interpreter verification remains unfinished.


## Bounded drain after EC serving stops

Controller.With_Receive.Drain now permits one bounded owning advance after a serving stop, without receiving queues, invoking event/control hooks, submitting new work or idle waiting. It requests stop before advancing. Precommand work can stop without writes; postwrite exchanges retain their original budgets/deadline and may finish or remain in recovery. Pre-existing serving adapter/owner/reentry/unexpected-reply faults and new drain faults quarantine the route. Repeated calls never reopen serving or replay replies/publications. Advanced is not quiescence or permission to exit/release backing. The lifecycle owner must retain engine/controller, saved replies, authority and recovery until independently resolved.

Portable tests/acpi-ec-drain passes2056checks/64processes for read/write/query prefixes and failure/reentry cases, plus544existing receive checks/36processes, both profiles. Actual-runtime semantic checking covers252sources/222runtime specs with zero objects per profile. Queues/clock/hardware/admission/ownership are modeled or trusted; no new SPARK proof, native/hardware validation, kernel/runtime/launcher/manifest change or default hardware selection. Full verified service/interpreter and actual native provisioning/lifecycle acceptance remain unfinished.


## EC catalog functional contract milestone

The actual default32-slot EC register catalog plus transaction engine now passes SPARK levels1/2:65proved checks and60flow results per level,44selected SPARK entities,191valid CVC5 attempts,local/engine initialization and checked composition. Exact Install/Revoke/Submit/Submit_Query transitions cover full-width owner/register/epoch matching, directional write safety/masks, active-record retention,increasing retired epochs,common nonwrapping tokens and refused-request state preservation. Other catalog operations preserve the stated coherence; completion routing and physical recovery are not fully proved. Imported bounded/no-alias/no-reentry truthful callbacks,exclusive pair ownership,hardware reservation/GlobalLock and time remain trusted.

Portable tests/acpi-ec-catalog-proof audits every proof tree and required public postcondition;40altered reports are rejected. Fresh54182hosted checks/70processes and252-source/222-runtime-spec zero-object semantic profiles pass. Existing EC source registration gains six transitive helpers without changing test assertions. ACPICA fixture effects/values match,with cache diagnostics qualified. Historical proof records are not renewed. No native/kernel/runtime/launcher/manifest/default startup change or native/hardware acceptance; full verified service/interpreter and actual authority/lifetime integration remain unfinished.


## Private EC backend family and receive ownership

ACPI_EC_Backend_Owner now constructs one private engine/catalog/endpoint/saved-reply/publisher/progress/wait/controller family. Trusted Configure is one-shot and performs no hardware/clock/queue callbacks; partial failure closes submission and receive paths. One receive child permanently claims the parent. Competing receivers and direct submission/progress paths cannot bypass the claim or reopen serving after stop. An admitted event hook has a bounded query-entry window. Retained drain/claim/publication paths preserve the same private family and original obligations.

Read-only histories retain the last real Field admission and attributable hardware completion after acknowledgement or lost answer delivery. No history or absent saved reply means global quiescence. Physical role/cell admission,authentic receive metadata,command-wide ownership/GlobalLock,fresh paired time,nonblocking publication and lifetime remain trusted. Default hooks admit nothing; real system-stop/source/control/receipt bindings and native launch remain pending.

Portable tests/acpi-ec-native-owner passes1148hosted checks/56processes over28cases/profile,40sources/24compiled units,including high64-bit IDs,setup and source failures,handoff,exclusive receiver ownership,dynamic epochs,uncertain hardware/publication and diagnostics. Actual-runtime semantic checks cover256sources/222runtime specs with zeroobjects per profile. Four new ACPI source files; no kernel/runtime/launcher/manifest/default startup or existing engine/catalog/AML semantic change. No new SPARK/native/hardware proof is claimed. The complete verified service/interpreter and real laptop integration remain unfinished.


## Retained EC query FIFO and hardware-aware receive

Admitted event hooks can now Request_Query into a bounded per-owner FIFO,retaining each publication budget without implicit coalescing. Busy hardware or unknown/stalled publication leaves queued triggers intact. Overflow and failed query admission stop new serving work explicitly; accepted failed/uncertain exchanges are never replayed. Queued requests start only after successful new admission,and never during stopped drain. Queue count/overflow,last queued admission and last query outcome are observable.

A bounded local scheduling hook runs after progress and before service receive. The EC owner defers service receive during active bounded hardware work,allows control access during retained recovery,gives a Field opportunity between query episodes,and suppresses idle wait only for runnable queued work. The default hook preserves old controller behavior. These are serialized scheduling rules,not native latency or global quiescence guarantees.

Fresh2460hosted checks/118processes pass (768queue+1148owner+544receive),both profiles;256-source/222-runtime-spec zero-object semantic profiles also pass. Source/callback/ownership/time/lifetime facts remain modeled or trusted. No new SPARK/native/hardware acceptance,AML/engine/catalog semantic change,kernel/runtime/launcher/manifest/default hardware selection. Full service/interpreter verification and actual laptop integration remain unfinished. Portable: tests/acpi-ec-query-queue.


## Kernel system events in the EC receive owner

The private EC receive owner now classifies reserved system labels before opening
the custom source hook's query window. An authenticated, well-formed kernel Stop
stops before progression or request receive and cannot be forgotten or vetoed by
a source hook. Malformed/foreign system messages are consumed without that hook.
Other valid system hints still reach the trusted hook for wake/logging handling,
but cannot Start_Query or Request_Query. Grant notices remain hints, never proof
of retirement or permission to release backing. Non-system source admission is
still explicit trusted work. Existing retained stop/drain/uncertainty behavior
remains; stop is not permission to exit or release ownership.

Fresh4088 hosted checks/146 runtime processes pass:1448owner (including12new
system cases),768queue and1872actual AML/lid/event checks, checked/release. Two
actual-runtime semantic profiles select258sources/222runtime specs with zero
objects. ACPICA explicit AML values/Notify still match, with cache diagnostics
retained. Two production source files plus tests/metadata/docs changed; no new
SPARK proof,native execution,hardware authority,launcher/default selection or
hardware acceptance. Native source/receipt transport,admission,command-wide
ownership/GlobalLock and lifetime integration,and full service/interpreter
verification remain unfinished.


## Explicit EC query-response diagnostics

Publisher.Observe and the private backend owner now distinguish No_Query_Response,
No_Pending_Event (the accepted query observed SCI_EVT clear), Zero_Query_Response
(completed QR_EC returned zero), and Event_Response with the exact nonzero byte.
Hardware completion remains separate: a zero response still preserves Succeeded.
Event history survives enqueue, backpressure, unknown delivery and exact claim;
absence of a retained notice cannot be used to infer an empty hardware response.
Only a newly accepted query clears response/event history. Refused submissions
preserve it. Observation performs no IO/clock calls and exposes no authority.
Empty results describe past observations, never future silence or quiescence.

Fresh4298 hosted checks/156 runtime processes pass across checked/release:
1658owner,768queue and1872actual AML/lid/event checks. Five new owner cases
cover clear SCI without command,zero QR_EC,successful publication,accepted reset
and refused-query history preservation; exact claim retention is also checked.
Both actual-runtime semantic gates select258sources/222runtime specs with zero
objects. ACPICA explicit AML values/Notify still match,cache diagnostics retained.
Four production source files plus tests/registration/docs changed; no new proof,
native execution,hardware authority,launcher/default selection or hardware claim.
Native integration and full verified service/interpreter remain unfinished.

Reference: [ACPI6.6 EC query interface](https://uefi.org/specs/ACPI/6.6/12_Embedded_Controller_Interface_Specification.html#query-embedded-controller-qr-ec-0x84).


## Bounded EC query scans

Request_Query_Scan is an opt-in, independently admitted source-hook request. One
FIFO entry remains until an accepted query observes SCI_EVT clear or returns zero.
Each distinct query consumes one allowance and keeps its original finite request,
pause and publication budgets. A scan waits through publication backpressure or
unknown delivery; exact claim or explicit publication retry settles only that
notice. No failed/unknown hardware query or handler is automatically replayed.

Scan_Exhausted retains the head entry without polling hardware or blocking Field
and control requests. Trusted Resume_Query_Scan supplies a new allowance only
after successful exhaustion, not during accepted work, publication retention,
failure or stop. The serialized supervisor must arrange a subsequent turn/wake.
Later FIFO entries stay behind the scan; direct source Start_Query cannot bypass
it. One Field opportunity separates iterations. Stopped drain can settle only an
already accepted scan iteration and never starts another query.

Observe.Scan reports head phase/remaining allowance and a historical empty-query
response. No_Head_Scan or a past empty response is not global quiescence. Source
authority, masking/GPE completion, command-wide ownership or held Global Lock,
backing lifetime and all upper AML/notification obligations remain independent.
This does not re-enable a source, release backing or launch native hardware mode.

Fresh6118 hosted checks/188 runtime processes pass, checked/release:
2316queue/scan,1658existing owner,2144actual AML/lid/event integration.
Fourteen new scan cases cover empty/zero/multiple events, exhaustion/continuation,
FIFO followers/wrap, failure/unknown completion, stop/drain, clock refusal, unknown
publication, original per-iteration publication budgets and Field fairness. The
joined event-scan case confirms scan completion before actual _Q01/Notify/_LID.
Both actual-runtime semantic gates select258sources/222runtime specs with zero
objects. ACPICA explicit values/Notify match; cache diagnostics remain qualified.
Four production files plus tests/registration/docs changed. No new SPARK proof,
native execution, kernel/runtime/launcher/default startup or hardware acceptance.
Native authority/transport/lifecycle integration and full verified AML/service
remain unfinished. Earlier counts above describe historical publications.


## Scan allowance policy proof milestone

The actual EC receive owner now uses ACPI_EC_Query_Scans for allowance/state
transitions instead of separate mutable remaining/in-flight/failure fields.
SPARK levels1/2 each discharge9proof checks and19flow checks over11entities,
including default initialization,six exact functional postconditions,range safety,
Global-null effects,all nine operations' termination and initialized refusal
output. Each report has47valid CVC5 and21valid trivial leaves,zero skipped work
and no external proof dependencies.20altered reports per level are rejected.

Create produces a fresh positive ready allowance. Accepted iteration consumes
exactly one; a published event returns to ready or exhausted. An empty observation
records its exact kind and completion. Failure preserves remaining allowance.
Extend is a total refusal/preservation operation under Valid: accepted iff the
old phase is Exhausted,otherwise it returns false with the entire state unchanged.
No caller exhaustion precondition substitutes for that refusal proof. Other
transition entry phases remain explicit preconditions checked by the owner.

The proof covers this logical policy under its declared types/contracts. It does
not establish truthful hardware/publication observations,source authority,owner
queue coherence,callback/lifetime behavior,source/GPE rearm or whole service proof.
The owner still separately refuses continuation during stop,recovery,unsettled
publication or missing admission. Existing prior proof records are not renewed.

Fresh6118hosted checks/188processes pass (1658owner,2316queue/scan,2144actual
AML/lid/event),checked/release and both AML widths. Both actual-runtime semantic
gates select260sources/222runtime specs with zeroobjects. ACPICA explicit values
and Notify match; cache diagnostics remain qualified. Native authority/transport,
command-wide ownership/held Global Lock,launch/lifecycle and full verified
interpreter/service remain unfinished. No native/kernel/image/QEMU build/run,
Git mutation,hardware validation or default startup change. Portable proof:
tests/acpi-ec-scan-proof/run.sh. Earlier validation counts above are historical.
