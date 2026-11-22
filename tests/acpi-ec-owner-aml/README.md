# AML through the private EC backend owner

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-owner-aml/run.sh

The runner supports --source-root DIR and --validate-only. It pins 285
inputs and compiles 110 units per profile. The
actual AML Core/Field_IO, lid consumer, remote provider, EC method/receiver,
typed backend receive/controller and private EC owner are composed together.
Hardware, downward IPC, authenticated receive metadata, admitted cells, exclusive
ownership and paired clock are modeled or trusted boundaries. No alternative
Core, transaction engine or backend implementation is substituted.

1872 checks across 40 runtime processes pass with assertions/overflow
checks enabled and with checks suppressed, using both DSDT integer widths. The six
original cases still cover closed/open lid samples, Preserve writes retaining
neighboring bits, queued query publication, a foreign full-width caller, unknown
hardware, lost reply and pre-access stop. New event cases complete the path:

* An admitted source hook queues a query; the private owner reads its event number.
* Publish only copies the notice into a modeled nonblocking queue. No AML callback
  runs during publication; the AML owner dispatches it after the backend turn.
* The actual event receiver authenticates the backend and epoch before _Q01 reads
  the EC Field through the same remote provider/backend. It increments a named
  event counter, records the bit and generates Notify(LID0, 0x80).
* A fresh _LID sample reads open. Notification dequeue checks target identity/path
  and exact value; a second dequeue is empty.
* Reusing a delivery or submitting a duplicate notice does no AML or wire IO.
* Unknown publication retains the producer notice without retry. Only an exact
  notice claim from the expected full-width consumer clears that retention; wrong
  caller and changed token claims fail. A claim is not hardware reconciliation.
* An uncertain handler Field read retains uncertainty and produces no notification.
  Zero handler fuel consumes the event without AML/IO. Neither case replays it.

ACPICA independently returns [0,0,0,1] for the original STOR/_LID sequence and
[0,1,1,1] for STOR(1), _Q01, EVCT, EVST, _LID, including exactly one LID0 0x80
notification, at both widths with repairs disabled. Its cache-cleanup diagnostics
are retained. This compares explicit AML values/effects, not IPC/ownership/hardware.

No new SPARK proof, native execution or hardware validation is claimed. The event
queue and claim transport here are test adapters, not a new wire protocol. Native
role/source admission, command-wide ownership or held Global Lock, stop/receipt
hooks, provisioning, launch and backing/death recovery still require integration.
Default native selection remains snapshot-only. Prior proof/semantic milestones
retain their original scope; the full verified interpreter/service and Lenovo
laptop acceptance remain unfinished.


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


## Joined EC scan, AML and deferred GPE regression

Fresh 3916 hosted checks/60 runtime processes pass, checked/release and both AML widths. Four new cases use the actual private EC owner, bounded scan, remote Field provider, Core interpreter, authenticated EC event receiver and GPE lifecycle. Edge/level success remains masked after scan-empty, throughout _Q01 and while its lid notification awaits consumption. The test owner handles that notification with a fresh _LID read before attributable completion and rearm. Uncertain EC Field access and zero AML fuel record failed/unknown handler completion and never rearm; duplicate deliveries perform no AML or IO. Hardware callbacks assert masking on every access and no rearm before the test owner's upper-work settlement.

This is a hosted composition with trusted source admission, serialized attribution and modeled hardware/transport/clock. It does not add native interrupt authority or a production supervisor, and the caller must not infer AML/notification completion from scan-empty. ACPICA explicit _Q01/lid values match at both widths; cache diagnostics remain qualified. No new SPARK proof, native execution, runtime semantic compilation, default startup change or hardware validation is claimed. Earlier evidence is historical, not renewed. Full service/interpreter verification and native integration remain unfinished.


## Multiple EC events held under one masked GPE

Fresh 6336 hosted checks/76 runtime processes pass, checked/release and both AML widths. Four new cases join two actual EC query results from one bounded scan with ordered _Q01/_Q02 execution, distinct tokens, two lid notifications and fresh _LID reads. Edge and level sources remain masked after scan-empty and between handler/notification consumptions. Only successful settlement of both events permits attributable deferred completion and rearm. The uncertain-publication case handles the first event through the actual Field provider while its notice is retained, refuses a mismatched claim and uses the exact consumer claim to advance the remaining scan without replay. Failure in the second handler preserves the first handler's effects and keeps masking and EC recovery; duplicate deliveries and refused new work does no extra AML or IO.

ACPICA independently matches two handlers, order trace 0x12, final lid state and exactly two notifications at both widths; its cache diagnostics remain qualified. Source admission, callbacks, clock/transport and serialized completion attribution remain modeled/trusted. This supplies no native supervisor, hardware authority or wire completion API. No production change, new SPARK proof, native-runtime semantic compilation, native execution or hardware acceptance. Full verified interpreter/service and native integration remain unfinished. Earlier evidence is historical, not renewed.


## GPE lifecycle proof milestone

The actual synchronous/deferred GPE session now has a proved default initial condition, explicit termination contracts and exact preservation of the handler budget across completion and IO helpers. Idle validity includes its initial zero budget/nondeferred state. Three redundant initializers were removed: callback out values are assigned before normal use, while exception paths store their own explicit unknown reports. The analysis-only source ID is full64-bit.

The portable focused runner tests SPARK levels1/2 with full flow analysis. Each discharges15proof+21flow checks over16entities (12fully analyzed and4abstract imported callback specs),including7functional postconditions,9operation termination checks,default initialization and a client validity assertion. Each has264validCVC5+14validTrivial leaves,zero skipped work,and20altered proof reports are rejected. The four imported callbacks explicitly assume Global-null effects and termination. Truthful hardware effects/reports,exceptions,source/control/backing ownership and serialized completion attribution remain external; this is not whole-service or hardware proof.

Fresh6534hosted checks/78processes pass against these exact source hashes,checked/release:6336actual EC scan/private owner/AML/notification/lid/GPE integration and198deferred lifecycle. ACPICA explicit ordered events,trace and two notifications match both AML widths,with cache diagnostics qualified. Source/closure registration refreshes do not renew historical proof or runtime reports. No native-runtime semantic compilation,native/kernel/image/QEMU execution,kernel/runtime/launcher/default startup or hardware acceptance is claimed. Full verified interpreter/service and native authority/lifecycle integration remain unfinished. Portable: tests/acpi-gpe-proof/run.sh.
