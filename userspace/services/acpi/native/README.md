# Native ACPI entry

`component.ccl` declares `acpi.svc`, launched by the process manager. Its typed
manifest requires a sealed firmware snapshot reader, logstore, and the ACPI
registration notification. Generated `Startup` checks all three at elaboration.
There is no provider endpoint, runtime role/configure exchange or bulk grant.

`ACPI_Native_Instance.Start` latches once, obtains an absolute monotonic startup
deadline in microseconds, and loads directly through `Startup.Tables`. The
snapshot loader checks full-width metadata before converting bounds, constructs
one limited arena in place from count/total/largest, copies bounded chunks into
owned exact-extent buffers, then performs local admission and initialization.
No partial owner reaches registration or the observer runner. CuBit's allocator
logs and exits on native OOM; hosted Storage_Error recovery is not its behavior.

`ACPI_Service` in this directory selects the native clock and startup delay
provider before the hosted default in the parent directory. The explicit64MiB stack setting is retained;
semantic checks do not demonstrate whole-call-chain stack fit.

The runner accepts only the shared `ACPI_Protocol.Observer_Tag`, classified from
the received message's kernel stamp. Neither process identity nor message words
grant a role. `CuBit.Refusal_Notes` bounds repeated refusal logging. Observer
requests cannot upload tables, mutate namespace state or execute arbitrary AML.
Idle waits explicitly use Wait_Forever; unexpected asynchronous completions stop
the runner, which submits no asynchronous work. Replies are attempted once.

Bring-up stages use existing logstore logging and serial echo. Snapshot/startup
success is explicitly marked hardware-pending. Device inventory/caches, event
subscriptions, EC/register mediation, named hardware capabilities and native
I2C/GPIO/HID input still require integration and separate native/hardware tests.
No actual hardware identification is invented from the snapshot metadata.

On Startup_Refused the owner emits one additional typed startup diagnostic. Module failures include the retained failed table ID, completed modules and consumed AML work; preparation/freeze/member failures use their own statuses. The copied completion report survives failed candidate cleanup. The table ID is a catalog identity, not a memory address; no byte offset is claimed. Formatter/runtime facts are hosted-tested, while native logger delivery remains unvalidated.


The observer server polls one event and one IPC opportunity per turn, and only
waits when neither yielded work. Kernel Stop is authenticated by the event
receive publisher plus exact framing, and precedes new request dispatch. Valid
channel wakes route to the process registry. Grant notices reach the logger's
full-peer lifetime filter; unrelated peers do not disable logging. Matching
logstore notices stop new publications and call existing Disconnect once; the
publisher/pages remain process-lived even when cleanup is not confirmed. Global
logger code is unchanged. A requested stop is logged as a normal serve outcome.

This stop path is for the current observer-only server, which submits no async
work or hardware transaction. Future hardware ownership requires explicit bounded
drain/lease cleanup before exit; a stop notice is not cancellation or quiescence.
Unknown/rejected events are consumed without logging floods. Completions still
stop the synchronous runner. Poll publisher and transport/lifetime facts remain
trusted kernel/runtime boundaries.

Current hosted event/observer matrix passes726checks/profile with104actual ALIs
in checked/release. Channel registry and logger transport are modeled; actual
classification and lifetime policy run. Separate six-unit actual-runtime checks
include real server/events/logger/instance bodies,557source files,gnatc and zero
objects. Startup is explicit imported declarations using current actual types,
not generated binding execution. No new proof,native linking/execution or
hardware acceptance follows. Older native-startup/native-entry fixture gates
were already noncurrent on field/region/instance mirrors before this change;
they remain pending migration and are not relabeled as passing.


## Native startup HID detection (hosted validated,not native executed)

After successful snapshot startup,the service now performs a bounded,one-time
HID discovery pass on its own Core before observer registration/IPC. It uses
fixed evaluators and the shared search/probe budget,retains CRS for one bounded
copy,resolves both controller paths,and logs only complete current observations
through the existing logger/serial wrapper. Defaults:10000fuel,65536resource
bytes,4candidates,256bytes/source/vendor span. These are local diagnostic limits,
not ACPI limits. The first failure or bound stops the pass explicitly; a bound
is not complete enumeration. Outcomes are latched,with no automatic AML replay.
Uncertain retained cleanup keeps the process-lived probe/Core and prevents
registration/serving. Other detection failures still permit read-only observer
service. Hardware provider/source attachment,device admission,launcher routing
and HID input/publication remain separate unfinished work; observed resources
create no authority. The manifest is unchanged,tables/logstore/registration only.

Hosted adapter:312checks/profile,20processes/profile,52commands,ordinary iASL
fixtures. Empty namespace,2devices,readiness/resources/budgets/cleanup/no-replay
and bounded typed logs covered. Startup lifecycle:948checks/profile,64processes,
102ALIs; detection boundary modeled there. Actual-runtime semantic gate:
18selections/17ALIs/287mirrors,zeroobjects,generated Startup intact. Current CCL
reader validates added source directories and process-manager service kind;
this is metadata resolution only. Exact prior ACPICA budget-fixture reference
reused after AML hash match,with known14cache-allocation exit diagnostic. No new
proof,native linking/boot/hardware validation or whole-service correctness claim.
Earlier native-startup/native-entry noncurrent notes describe historical state;
these current suites have since been migrated. Full goal remains incomplete.


## Native power startup observation pass

After HID cleanup and before registration/IPC,the native instance now performs
a one-time same-Core power pass:checked first lid/AC source,then up to4battery
devices. One10000operation AML budget covers searches/status/member queries/
battery collection. Battery point admission requires completed device startup,
not notification readiness; it never enables capture,claims/ACKs events or
publishes a fresh cache. The existing notification-gated cache API is unchanged.

Battery identity,presence,BIX,BST and presence confirmation use the existing
collector. Absent batteries skip BIX/BST; malformed or changed bundles never
produce sample lines. Logged remaining quantities include known flags and BIX
units; no percentage or whole-system power decision is fabricated. Observations
are ordered historical values,not atomic/live state. First failure/bound stops
the pass. Every outcome is latched. Unexpected exceptions leave Owner_Error;
recovery is conservative while calls are in flight. Cleanup/provider/fatal/Field
uncertainty stops registration/serving and retains a final bring-up summary.

Hosted actual pass504checks/profile,28fresh processes/profile,76commands,
32/64bit AML. Covers two batteries,absence with fatal later methods,invalid BIX,
hot-swap,blocked lid/invalid PSR,empty/unready/fuel/count limits,fatal recovery,
notification-state preservation and logging-exception no replay. Actual startup
lifecycle1168checks/profile,72processes,103ALIs; power/HID boundaries modeled
there. Runtime semantic gate28selections/27ALIs/316mirrors,zeroobjects. Current
CCL reader validates expanded source closure and process-manager service kind.
New repair-disabled ACPICA direct values match three fixtures,with known4cache
exit diagnostic explicitly qualified. No new proof,native linking/boot/hardware
validation or whole-service correctness. Event-driven refresh/cache publication,
hardware providers/source enrollment,transitions and input integration remain
unfinished. The manifest still requests only tables/logstore/registration.


## Bounded EC source owner composition

ACPI_Native_Server.With_EC_Events connects the same-Core EC receiver and retained-notification pump to the native loop owner step. Each turn pumps at most four existing claims/ACKs, then takes at most one queued EC result only after explicit local notification empty. A processed event returns Pending so its notifications are routed on the next turn before another source event; IPC retains a turn between batches. Source empty is a trusted local observation, not hardware health or cache freshness.

Fatal, unhealthy source, readiness/handler refusal, retained cleanup and blocked fanout stop the owner. Callback exceptions propagate with Observation_Complete=False and the terminal guard set. No automatic AML/cleanup retry. Source descriptions and consumed receiver receipts are distinct. Explicit cleanup after a normally completed refusal cannot reopen the owner; exceptional incomplete state remains retained. The containing service must stop other AML callers and own hardware/source drain and leases.

At most one delivery allocation is live. Only successful or missing-handler non-pending deliveries are freed; refused/exceptional state stays process-owned. Native allocator/runtime behavior remains unvalidated. Bounded source/fanout callbacks, authenticated metadata/order/wakes/health, no reentry/reset/Core mutation and all-consumer invalidation are caller obligations. No hardware ACK/cancel, source enrollment or fresh-cache publication is added. The production default remains observer-only; manifest/component authority is unchanged.

Hosted matrix:4936checks,36runtime processes,42commands,checked/release and both AML widths. Cases include queued Fatal with a later event untouched, normal draining/idle, unhealthy source, unready admission, fanout refusal and source/routing exceptions. Actual runtime/generated Startup semantic gate:45selections,44ALIs,zeroobjects per profile,355source mirrors. These are hosted/semantic checks, not native execution or SPARK proof. Exact evidence: tests/acpi-native-notifications/EC-OWNER-EVIDENCE.json. Full verified ACPI completion remains open.


## Bounded startup AML delays

The owner opens one immutable absolute microsecond deadline before loading and
initializing firmware. Sleep relinquishes the CPU and uses bounded absolute
sleep slices; Stall performs bounded clock reads without invoking a scheduling
operation. Zero Sleep also yields. Every wake is resampled: an early return or
successful syscall is not evidence that the required time elapsed. Expired,
stopped, unavailable or regressing clocks and exhausted work refuse completion.

The domain closes before observer registration and on every early-return or
exception path. It cannot reopen or acquire a new deadline through AML or IPC.
The current provider is for startup only; future live EC/event AML requires an
explicit owner-scoped timing epoch. No kernel/runtime/launcher/CCL change or new
hardware authority accompanies this provider. The existing logger is unchanged.

Portable functional checks: tests/acpi-native-delays. Scoped levels1/2 proof:
tests/acpi-native-delay-proof (23checks per level, not whole-service proof).
The native facade and startup owner are hosted-tested; actual scheduler/clock
accuracy, native linking/boot, stack fit and laptop behavior remain unvalidated.
Specification timing rules: https://uefi.org/specs/ACPI/6.6/19_ASL_Reference.html#sleep-milliseconds-sleep


## Separate AML and device initialization diagnostics

Startup now logs the copied AML completion and device-initialization outcome as
separate stages. A loaded namespace is not evidence that the device walk ran.
The devices line reports the exact phase, qualified region-barrier outcome,
error count and consumed fuel; when errors exist it includes the first node
and execution reason. A barrier not attempted prints NOT_ATTEMPTED instead of
interpreting a default outcome. Incomplete, failed, error-bearing or Fatal
observations use Warning; only completed error-free initialization with an
attempted successful barrier uses Info. Tests bound the line to256 characters.

This is diagnostic behavior only: no provider admission, readiness, hardware
authority or serving policy changes. ACPI_Requests.Device_Completion copies the
same owner report already exposed through the existing observer metrics; it
does not evaluate AML or add a wire operation. Native/hardware execution is
still required to validate the real boot path.


## Retained HID channel descriptions

The startup HID pass now prepares typed I2C/GPIO channel descriptions from each
complete, current resolved observation. Its report owns a bounded array of up to
16rows with the original device and both controller descriptors, typed channel
parameters, descriptor register and all copied resource metadata. Only the
first Observed rows are complete; unused/refused rows contain no plan. A later
refusal retains earlier complete descriptions without claiming full discovery.
A logging exception before publication adds no row and the terminal latch
prevents replay.

Channel_Attempted qualifies Last_Channel_Status; these describe the most recent
preparation attempt, not controller admission or row publication. Channel_Refused
is distinct from AML, cleanup and source-resolution failures. Reports remain
historical copied data: later AML can change resources even while descriptors
remain current. Revalidate the resources and all identities before a later
launcher handoff. No IPC export, endpoint minting, power/dependency completion,
hardware admission or event enrollment occurs here. The existing manifest is
unchanged; component source directories add the two existing channel codecs.

Hosted checks cover two plans, early/later refusal, all16rows filled from17
fixture devices, zero unused payload, recovery and no replay. Actual-runtime
semantic compilation and the mocked startup owner cover the expanded report;
no new proof, native linking/boot, native stack-fit or laptop acceptance is
claimed. CCL declaration checking uses a retained reader with copied current
schema/defaults, not a native build.
