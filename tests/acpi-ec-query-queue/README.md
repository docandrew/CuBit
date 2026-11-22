# Bounded admitted EC query requests

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-query-queue/run.sh
    nix develop --command bash tests/acpi-ec-query-queue/semantic.sh

The hosted runner supports --source-root DIR and --validate-only. It pins40sources/24compiled units and audits strict checked/release flags and equal case inventories.768checks/26processes cover13cases/profile: FIFO and ring reuse,Field/query arbitration,overflow,stop/drain,clock/deadline refusal,unknown hardware,ownership failure,unknown publication,per-request publication budgets,spurious sources and foreign senders. Existing owner1148checks/56processes and receive544checks/36processes pass unchanged. Actual-runtime semantic profiles cover256sources/222runtime specs with zero objects. No new SPARK proof,native execution,hardware acceptance or fresh ACPICA comparison is claimed; AML and transaction/catalog implementations are unchanged.

Owner.Request_Query is only for an independently admitted event hook. Each trigger keeps its own publication-attempt budget in a bounded FIFO (default16,explicit generic capacity); requests are never silently coalesced. Overflow is sticky,retains earlier entries,and stops new serving work. A queued request is removed only after a new hardware query is accepted. An admission failure retains it and stops rather than retrying. Failed accepted queries and unknown hardware/publication are not replayed. Trusted existing Resume_Queries or exact claims may enable later distinct queued requests while serving; they do not recreate failed requests.

The receive scheduler runs after one owning progress step and before one service opportunity. It leaves service requests in the kernel during active bounded hardware work,then allows them when completion or retained recovery permits. This prevents temporary query occupancy from causing an avoidable Field busy denial. Pending ready queries prevent idle sleep; blocked queries do not. One Field opportunity separates queued query episodes; an admitted Field consumes that opportunity. Control receive remains reachable during retained hardware recovery. This is serialized scheduling,not a hard latency/throughput guarantee.

Local_Work_Stop retains old accepted work for bounded drain. Queued requests do not start during drain or after stop,even when the engine becomes idle. Queue counts,overflow,last queued admission and last query hardware outcome remain observable. Zero active hardware or saved replies is not global quiescence; queued sources and retained notices/recovery have independent lifetime. Do not exit/release backing from these observations alone.

Callbacks/source admission/authentication,command-wide ownership or held Global Lock,paired fresh time,nonblocking publication,kernel queue/wake behavior and process lifetime remain modeled/trusted. Native stop/source/receipt/control hooks,provisioning/launch and physical recovery remain integration work. No new wire opcode,physical address API,capability,manifest or default hardware selection is supplied. Full verified AML/service and actual laptop acceptance remain unfinished.


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

Earlier counts above describe earlier publications. Current suite evidence is EVIDENCE.json.


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

Earlier counts above describe earlier publications. Current suite evidence is EVIDENCE.json.


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


## Deferred masked GPE handlers

ACPI_GPE_Lifecycle.Driver now supports Begin_Deferred/Complete_Deferred alongside
Service. After Start masks the source and clears edge status as needed,positive
Begin_Deferred claims the handler with a fixed budget but invokes no callback.
Zero budget preserves Ready. Completion is accepted once only for that deferred
Executing session; synchronous in-progress handlers cannot be completed through
this route. The common completion classifier retains raw overcharge as an error.
Only successful completion permits Finish to clear level status and enable.
Failure,unknown completion,budget error and overcharge keep confirmed masking.
Unknown enable loses masking evidence. No automatic replay or source release.

Handler_Budget and Deferred are copied diagnostics. Last_Operation marks logical
handler dispatch; it is not a callback counter. Deferred=False means only that
this logical completion has been recorded,not physical quiescence or permission
to exit/release backing. The serialized owner must independently attribute the
completion and preserve admitted source identity,epoch,ownership/control and
backing throughout deferred work. No untrusted wire-completion API is supplied.

Fresh5334hosted checks/72processes pass,checked/release:198deferred lifecycle,
3112EC queue/scan (six new GPE composition cases),2024unchanged synchronous
GPE/actual AML checks. Composition keeps the source masked across scan budget
exhaustion,Field service,unknown publication,stop/drain and uncertain EC access.
The old GPE source list gains its missing existing resource-name helper pair.
ACPICA matches explicit _E01/_L02 counters/lid values at both AML widths; its
cache diagnostics remain qualified. Hardware ordering/callbacks are modeled.
No new SPARK proof,native/kernel/runtime/launcher/default startup change or
hardware acceptance. Native admitted source roles,transport,lifetime and full
verified interpreter/service remain unfinished. Historical proof/runtime
records are not renewed by source-registration updates.
