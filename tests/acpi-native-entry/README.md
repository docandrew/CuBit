# Current native ACPI API checks

Run `bash tests/acpi-native-entry/run.sh --mode checked` or `--mode release`
inside pinned Nix, with TMPDIR=/home/doc/cubit-build-tmp before and inside it.
Each profile semantically compiles45selections, producing44ALIs and zero objects
or executables. It checks main, clock, logger, native owner, observer endpoint/
runner, event routing/classification/lifetime, snapshot bindings and generic
local copy/startup units against actual current runtime declarations.

All captured source fixtures have explicit repository originals in mirrors.tsv.
`--validate-only` checks hashes, closed Ada inventory and live mirrors without
compilation. Missing or changed APIs stop validation. Eight stale mirrors were
explicitly reconciled against approved publication afterimages or the attributed
external service-catalog change; CURRENT-INTERFACE-EVIDENCE.json records them.
The historical api-pending.json retains the earlier provisional API hashes; it
is not a current API verdict or permission to bypass the live gates.

Startup requires Tables, Logstore and Registration. For this milestone, the CCL
manifest host tool was rebuilt privately from current repository sources in Nix;
its output from the current manifest/catalog/schema exactly matches both captured
Startup files. The old shared binary refused current inputs; it was not modified.
The evidence records tool/source/input/output hashes. No imported Startup stub
substitutes for these generated declarations. This does not execute elaboration
or prove launcher/kernel capability delivery. Generation source/tool evidence is
recorded for this milestone; the semantic runner itself does not rebuild CCL.

Only the unedited generated startup.adb suppresses its redundant-with warning.
Handwritten selections keep warnings as errors. Checked uses gnata+gnato, release
gnatp, both always gnatc against the host predefined runtime. Native System/runtime
objects are not mixed into the host runtime. Build180s,stack64MiB,logs1MiB,nice19/j1.

These are semantic compile checks, not execution of native runtime/ABI, allocator,
clock/register syscalls, IPC, logger or event delivery. Link/stack fit, genuine
launch bindings, kernel authorization, QEMU/laptop tests and full SPARK correctness
remain separate acceptance requirements. No new proof or ACPICA run is claimed.


Native detection integration adds actual fixed HID discovery/resolution
declarations and body,18selections/17ALIs/zeroobjects in both profiles.287exact
fixtures mirror current source. Actual native Instance hook and generated Startup
compile semantically; startup binding generation was verified at the preceding
checkpoint and its manifest is unchanged here. No native launch/execution claim.


Checked power sampling adds7actual generic/owner bodies. Current gate:
25selections/24ALIs/305exact mirrors,zeroobjects in both profiles against actual
runtime/native Core/generated Startup. These power units are not yet called
from native startup; semantic checking does not establish linking/execution.


Native power wiring now calls the checked sampling/point battery APIs before
registration. Current gate28selections/27ALIs/316mirrors,zeroobjects/profile,
including actual native Instance/power body and battery observation bridges.
Generated Startup/manifest are unchanged. No linking or execution claim.


## HID recovery milestone review

Recovery is now separate from retained-CRS cleanup. Fatal/Field/provider
uncertainty during identity,status,resource or descriptor observation prevents
subsequent power AML and registration,even with no retained value pending.
In-flight calls keep conservative recovery; unexpected exceptions latch
Owner_Error. Normal budget/resource refusal remains distinct. No automatic
retry/reset or authority change. Native main checks both cleanup and recovery.

Actual HID checked/release532checks/profile,30fresh processes,80commands.
Four fatal fixtures cover CID,STA,CRS and DSM; each requires recovery with no
retained cleanup pending. Logging-exception no replay also covered. Native
startup1228checks/profile,76fresh processes,103ALIs; a model recovery flag
with no cleanup explicitly blocks power,registration and serve. Actual-runtime
28selections/27ALIs,zeroobjects/profile. New fixtures compile with normal iASL;
no ACPICA interpreter equivalence for host fatal-signal policy is claimed.
No new proof,native linking/boot or hardware validation; full goal incomplete.


## Bounded owner notification pump

Added ACPI_Requests.Notification_Pump for serialized local owner work. Each
batch has an explicit limit of at most32claims and ACK attempts. Existing
guarded routing runs on both claim and ACK. Partial fanout or ACK fanout
failure retains the exact receipt; retry must invalidate all consumers
idempotently. No AML is re-executed by the pump. Limit_Reached never means
queue empty; Queue_Empty is only an explicit local empty observation. It
does not establish source health,hardware acknowledgement or cache freshness.
The callback must be bounded,local and non-reentrant; that remains a caller
obligation. No native source enrollment or automatic live refresh is enabled.

Hosted checked/release:262 EC/lid/AC checks and180battery checks per profile,
32/64bit AML,14commands per suite. Tests exercise zero limit,unready owner,
partial fanout,retained-receipt retry,ACK fanout failure,batch boundaries,
explicit empty observation and stale-cache refusal. Runtime semantic gate:
30selections/29ALIs/zeroobjects per profile. Startup230file inventory is
metadata-only.20previous source-pin changes explicitly reconciled to
approved current afterimages or unchanged frozen baseline. Initial EC exact
count gate refused114vs old94; corrected catalog and full rerun passed.
Existing fixtures compile with normal iASL; no new ACPICA interpreter run,
SPARK proof,native linking/boot or hardware validation. Full goal incomplete.


## Native loop bounded owner work

ACPI_Native_Server.With_Dispatch now accepts one bounded serialized owner
step per turn. Kernel Stop and unexpected completion win before owner work.
Pending runnable local work prevents idle sleep; one IPC opportunity remains
between batches. Owner refusal terminates this serving attempt before a new
dispatch. The default observer returns idle and enables no hardware source.
The callback must be bounded/non-reentrant,avoid blocking peer IPC,and retain
cleanup/uncertain state; Pending cannot mean waiting for hardware. These are
caller obligations,not newly proved properties. No automatic replay/reset.

Hosted checked/release actual loop753checks/profile covers pending drain,IPC
fairness,explicit idle wait,refusal,control stop and completion precedence,
and event-before-owner ordering. Startup1228checks/profile/76fresh processes
passes with updated model API. Actual-runtime semantic30selections/29ALIs,
zeroobjects/profile. No AML semantic changes,new ACPICA interpreter run,
SPARK proof,native linking/boot,hardware provider/source enrollment or laptop
validation. Full verified ACPI/AML goal remains incomplete.


## Native notification owner composition

ACPI_Native_Server.With_Notifications connects the retained-receipt pump to
the native server owner step. A turn attempts at most4claims/ACKs. Explicit
local empty returns idle; limit returns pending; readiness/fanout/broker
refusal stops before new dispatch. Every consumer must be installed and
routed or invalidated idempotently. The native default remains observer-only;
no source/provider enrollment,hardware ACK or cache publication is enabled.

Observation_Complete clears and the previous report resets before each pump.
An unexpected callback exception propagates without reporting an old success,
automatic retry or new dispatch. Retained state stays with the owner; an
explicit repaired-owner attempt is tested without EC AML replay. Bounded
local callback/non-reentry/all-consumer coverage remain caller obligations.

Hosted actual same-Core EC receiver/broker/native adapter/native loop/two lid
caches/AC cache passes324checks/profile checked/release,32/64bit AML,14commands.
Six real fixture notifications drain across two fair turns; partial fanout
refusal preserves the receipt,invalidated caches stay unavailable,and callback
exception does not masquerade as queue empty. IPC/event/logging are models.
Actual-runtime semantic31selections/30ALIs/zeroobjects/profile. Initial wrong
nonzero_LID expectation and a test variable scope compile error are retained
in evidence; both corrected. Normal iASL compilation only,no new ACPICA
interpreter run,SPARK proof,native link/boot or hardware validation. Full goal
remains incomplete.


## Same-owner root sleep-state observations

New Requests/Bootstrap.Sleep_States observes ROOT S0..S5 through the fixed
ACPI_Sleep_Objects evaluator and the same private service Core. Completed
device startup is required. One total budget includes1host unit per attempt
and actual AML/field inspection. Stop on first refusal/exhaustion; explicit
attempt bits preserve unqueried states and earlier ordered historical values.
Fatal/Field/cleanup/provider uncertainty requires owner recovery before later
AML. No notification enable/ACK,live cache publication,wire selector,register
authority or power transition is added. The descriptive register model must
come from the trusted owner's FADT interpretation; no backend readiness is
inferred from a declaration,missing required state or zero sleep type.

Hosted checked/release456checks/profile,8fresh processes/profile,26commands,
32/64bit AML. Covers unready/receiving owners,zero and one-unit budgets,paired
and A-only type use,ignored returns,missing required/optional declarations,
invalid type refusal,fatal recovery and no later AML,ordered effects and
explicit resampling. Current-runtime33semantic selections/32ALIs/zeroobjects.
Fresh normal repair-disabled ACPICA matches raw packages and one S3 effect
per direct query in both widths. Its preflight already runs sleep methods:
CALL1before/2after is recorded explicitly,not compared as equal initial host
state. Initial oracle mismatch/fatal preflight attempts are retained separately
and are not passes. Four cache allocations remain in raw reference logs.
No new SPARK proof,native link/boot/hardware validation or transition policy.
Full verified ACPI/AML objective remains incomplete.


## Native FADT sleep-register model detection

The native power startup pass now logs a read-only model from exactly one
retained immutable FADT before lid/AC/battery observations. Missing,duplicate
or invalid FADT stays distinct from an observed no-register model. The pure
FADT.Sleep_Registers classifier distinguishes legacy A-only/paired/none and
HW-reduced byte control,explicit legacy fallback,invalid fixed geometry and
B-only inconsistency. HW-reduced metadata ignores legacy PM1 blocks; absent
control yields unused return data. Low-power-idle bit21 is observed only for
revision5or later. Presence/geometry and PCI-space classification are metadata,
not PCI GAS target validation,physical ownership,backend or transition readiness.

Startup executes no Sx AML for this stage. The earlier six-root owner query
remains explicit because evaluation may have effects. The existing logger is
unchanged; one bounded typed stage line is emitted and latched with the pass.
No notifications,cache freshness,hardware endpoints or public wire operation
is enabled. Malformed/missing sleep metadata does not fabricate support or
prevent independent diagnostic lid/AC/battery observation.

Hosted checked/release924power checks/profile,42fresh processes/profile,
104commands; actual copied FADT decoder/catalog lookup covers missing,short,
duplicate,A-only,paired,reduced,none and invalid length. Pure descriptor cases
cover fallback,B-only,spaces,width,offset,access size and old revision flags.
Startup1228checks/profile/76processes/105ALIs; runtime semantic36selections/
35ALIs/zeroobjects/profile. One ordinary test use-clause warning fixed under
warnings-as-errors. Existing fixtures compile normally with iASL. No new
ACPICA interpreter run,SPARK proof,native link/boot or hardware validation.
The new pure classifier is tested and SPARK-marked,not yet proved.

Normative reference: ACPI6.6sections4.8.3.7and5.2.9
https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf
The full verified userspace ACPI/AML objective remains incomplete.


## FADT sleep-model functional proof milestone

The actual ACPI_FADT.Sleep_Registers.Describe now has a ghost declarative
postcondition for exact selected/refused outcomes,legacy A/B model,HW-reduced
control geometry,low-power-idle flag and explicit fallback diagnostics. Its
runtime classifier is unchanged; named constants/operator imports moved to
the spec for the contract. The relation uses actual Registers.Describe and
Hardware_Reduced interfaces. It does not prove those helper bodies,FADT byte
provenance,catalog ownership,hardware authority or transition readiness.

Levels1/2 each prove7checks(one compound functional postcondition plus6safety
checks)and7flow/termination checks over3selected all-SPARK entities. Each has
27CVC5+2Trivial Valid attempts,no unproved/skipped result or explicit Assume.
Imported helper post/effect/runtime-safety dependencies remain explicit in
full reports. Eight actual pure source files,no Core substitute. Portable
runner reproduces both levels. Ten audit cases reject empty/non-SPARK/no-post/
unproved/skipped/Assume/stopped/invalid-attempt/unreviewed-helper evidence.

The assertion-enabled/release native power matrix passes924checks/profile
on the final contract,42fresh processes/profile,104commands. Current-runtime
semantic36selections/35ALIs/zeroobjects/profile passes. Five other closed-input
suites received source-pin metadata refresh only,no new runtime claim. No new
ACPICA interpreter run,native linking/boot or hardware validation. The prior
classifier-unproved checkpoint is superseded only for this modular scope;
whole AML/Core/service verification and native integration remain incomplete.


## Bounded EC source owner composition

ACPI_Native_Server.With_EC_Events connects the same-Core EC receiver and retained-notification pump to the native loop owner step. Each turn pumps at most four existing claims/ACKs, then takes at most one queued EC result only after explicit local notification empty. A processed event returns Pending so its notifications are routed on the next turn before another source event; IPC retains a turn between batches. Source empty is a trusted local observation, not hardware health or cache freshness.

Fatal, unhealthy source, readiness/handler refusal, retained cleanup and blocked fanout stop the owner. Callback exceptions propagate with Observation_Complete=False and the terminal guard set. No automatic AML/cleanup retry. Source descriptions and consumed receiver receipts are distinct. Explicit cleanup after a normally completed refusal cannot reopen the owner; exceptional incomplete state remains retained. The containing service must stop other AML callers and own hardware/source drain and leases.

At most one delivery allocation is live. Only successful or missing-handler non-pending deliveries are freed; refused/exceptional state stays process-owned. Native allocator/runtime behavior remains unvalidated. Bounded source/fanout callbacks, authenticated metadata/order/wakes/health, no reentry/reset/Core mutation and all-consumer invalidation are caller obligations. No hardware ACK/cancel, source enrollment or fresh-cache publication is added. The production default remains observer-only; manifest/component authority is unchanged.

Hosted matrix:4936checks,36runtime processes,42commands,checked/release and both AML widths. Cases include queued Fatal with a later event untouched, normal draining/idle, unhealthy source, unready admission, fanout refusal and source/routing exceptions. Actual runtime/generated Startup semantic gate:45selections,44ALIs,zeroobjects per profile,355source mirrors. These are hosted/semantic checks, not native execution or SPARK proof. Exact evidence: tests/acpi-native-notifications/EC-OWNER-EVIDENCE.json. Full verified ACPI completion remains open.


EC boot metadata diagnostics now run after complete snapshot load and before the
device summary. The current EC-BOOT-NOTES-EVIDENCE.json records the new private
validation; earlier interface evidence remains historical. API checks select
58 units/55 ALIs per mode with zero objects. The hosted owner cases check one
informational missing-ECDT record only for loaded catalogs and its ordering.
Valid/duplicate/malformed/over-budget metadata and the actual logging hook are
covered separately by tests/acpi-ec-boot-notes with a hosted logger. No provider
installation, early _INI access, native/runtime or hardware acceptance follows.


Controlled snapshot completion changes are recorded in SNAPSHOT-DRIVER-EVIDENCE.json.
Current API checks select59units/56ALIs with zeroobjects per profile.
These checks do not wire a native provider or establish hardware readiness.


The controlled module-startup milestone is recorded in MODULE-STARTUP-EVIDENCE.json.
Its current-source input/closure refresh does not renew historical proof results.
Fresh hosted checks cover both profiles; no native execution or hardware
authority/region connection follows from these tests. The native API gate
selects 59 units and produces 56 ALIs with zero objects per profile.


Native controlled-loader integration evidence is recorded in
NATIVE-CONTROLLED-LOAD-EVIDENCE.json. The API gate now selects 60 units and emits
57 ALIs with zero objects per profile. Hosted kernel/startup/clock models are
not native authentication, provider admission or hardware validation.
