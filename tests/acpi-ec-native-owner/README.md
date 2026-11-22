# EC backend instance and receive owner

Set TMPDIR before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-native-owner/run.sh
    nix develop --command bash tests/acpi-ec-native-owner/semantic.sh

The hosted runner supports --source-root DIR and --validate-only. It pins40sources and checks the exact24compiled units, strict checked/release flags and matching case inventories.1148checks/56processes cover28cases/profile: no callbacks before successful setup,failed/partial/duplicate/oversized setup,immutable setup attempts,empty query-only admission,high64-bit owner/register/epoch matching,clock/deadline/save refusal,lost answer,unknown hardware,query publication/claims/backpressure,standalone progress,dynamic epoch re-admission,direct-to-receiver handoff,sole receiver ownership,hook failures,and stopped draining.

ACPI_EC_Backend_Owner instantiates one private engine/catalog/endpoint/saved-reply/publisher/progress/wait/controller family. Their actual package instances cannot be substituted through the public API. Configure consumes one trusted setup attempt; its local Ready means configuration succeeded,not that hardware is admitted or working. Failed setup keeps all submission/receive paths closed even after an internal partial install. It performs no clock/queue/reply/hardware callback. Subsequent Install/Revoke are trusted serialized admission operations,not wire methods.

Use direct Begin_Call/Start_Query/Advance before receiving,or one With_Receive child. The first ready receive/drain entry permanently claims the parent; a second child refuses before callbacks/queues. Claimed receive ownership closes direct Field submission and progress. Only an already admitted event hook may start a query through the guarded event window. Stops close new submissions and installs without releasing or resetting any engine,reply,notice or hardware obligation. Existing explicit claim/publication retry and bounded owning drain retain their original semantics. Default hooks admit nothing; real native binding must authenticate system stop,EC sources/control and receipts independently. Busy-source coalescing/retrigger policy belongs to that binding.

The read-only observation keeps the last actual Field admission and attributable completion,including token,hardware outcome,possible effects,recovery and failed answer delivery. Histories survive engine acknowledgement and serving stop; refused competing direct routes do not overwrite them. Histories are not the current operation or proof of quiescence. Returning from Serve/Drain is never permission to release backing or exit.

Trusted boundary: independently admitted physical roles and cell rights,authentic full-width kernel receive metadata,command-wide exclusive ownership or actually held Global Lock,paired fresh microsecond clock/pause callbacks,bounded truthful nonreentrant callbacks,exclusive serialized owner lifetime,and nonblocking event publication. The wrapper creates no physical mapping,capability,process or launcher/default startup selection. Hardware callbacks take typed roles,not caller addresses. Full physical recovery,real source/receipt transport,kernel/runtime/launcher provisioning and full service/AML verification remain unfinished.

Actual-runtime semantic profiles instantiate default saved-reply/receive callbacks with explicit native clock callbacks,256sources/222actual runtime specs,zero objects. That main must never link or execute. No new SPARK proof,native execution,hardware acceptance or fresh ACPICA comparison is claimed; AML and the existing proved engine/catalog sources are unchanged. Earlier reports retain their historical scope.


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
