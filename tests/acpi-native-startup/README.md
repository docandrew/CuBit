# Direct native ACPI owner lifecycle

Run `bash tests/acpi-native-startup/run.sh --mode checked` or `--mode release`
inside pinned Nix with TMPDIR=/home/doc/cubit-build-tmp. The default is release;
`--validate-only` checks pinned inputs, closed directory inventory and mirrors.

This replaces the old provider/configure startup API suite. The actual new
Native_Instance.Start executes against actual Requests/Core/AML and generic local
snapshot startup. Runtime entry, clock, registration, snapshot source, serve loop
and logger delivery are MODELS here. Boundary source/declarations are exact
mirrors. The separate native-entry suite checks actual runtime declarations;
the separate native-server-loop suite executes the actual loop with modeled IPC.

72 fresh processes cover18scenarios at32/64bits with successful/throwing Critical
logging: normal startup and normal Stop return, module/root/bus/device Fatal, ordinary initializer error,
unconfigured region, unavailable/overflowing clock, snapshot/copy refusal,
registration refusal and unexpected loader exception. Assertions check one load,
finite microsecond deadline, no registration before completion, serve ordering,
one exact Critical diagnostic attempt, and no effect on a second Start. A logging
exception must preserve the load outcome; no reset of the process-lifetime latch
is exposed by the fixture. The model's native-copy routines fail if invoked.

Build180s, each process30s, stack64MiB, logs1MiB, nice19/j1. Compiler tool, source,
mirror, profile and artifact hashes are audited. This is no native/kernel boot,
stamp or capability authentication, real logger delivery, native OOM test, stack
proof, SPARK proof or hardware acceptance claim. Native allocator exits on OOM.

Current interface migration uses exact native Instance, Server/Log/Events
declarations and actual opaque Process_ID runtime units. Events dispatch, server
execution and Grant_Notice remain modeled boundaries in this suite; event
authentication/fairness belong to the separate native-server-loop matrix.
Normal Stop must emit exactly one Info serve outcome and no serve-failure line.
Other server return emits exactly one Warning; repeated Start remains inert.

Eight stale legacy pins are explicitly reconciled against the frozen checkpoint
or approved cumulative afterimages, with old/current hashes retained in
CURRENT-INTERFACE-EVIDENCE.json. No production AML or fixture bytecode changed;
no new ACPICA run or SPARK proof was needed for these lifecycle-only assertions.
Prior checkpoint checked/release passed752checks in56fresh processes.
The historical native-entry gate remains noncurrent; no native launch claim.


Current detection hook lifecycle:64fresh processes/profile,948checks,102ALIs.
Detection starts only after complete snapshot startup,before registration;
modeled cleanup uncertainty or unexpected detection exception prevents serving.
Cleanup refusal retains the final bring-up summary. Repeated Start replays no
detection. The detection body is modeled here; tests/acpi-native-hid runs it
against actual AML/Core with modeled logging. No native/hardware claim.


Current power hook lifecycle:1168checks/profile,72fresh processes,103ALIs.
Power starts after completed HID cleanup,before registration. Recovery or
unexpected power exceptions stop startup; second Start never replays power.
The power body is modeled here; tests/acpi-native-power executes it.


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
