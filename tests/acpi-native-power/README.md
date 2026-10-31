# Native power observation pass

Run run.sh in pinned Nix with TMPDIR=/home/doc/cubit-build-tmp before and inside
Nix. --validate-only checks source hashes without execution. Actual native power
body and Requests/Core/AML run here; logging transport is modeled. Builds180s,
processes30s,stack64MiB,logs1MiB,nice19/j1. Source/tool/profile/artifact hashes
are audited. BIX text capacity128bytes is explicit collector policy. No new
hardware authority,wire invocation endpoint or power-manager policy.


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
are ordered historical values,not atomic/live state. Ordinary lid/AC query failures retain explicit flags and permit independent
later AC/battery probes within the remaining total budget. If those probes
finish, the final status reports the first ordinary family failure. Readiness,
fuel exhaustion, battery failure and uncertain effects still stop the pass. Every outcome is latched. Unexpected exceptions leave Owner_Error;
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


Independent-probe regression cases cover an unavailable lid, invalid AC state,
both together, a later battery bound and Fatal after an AC failure. All
observations remain historical startup samples; failed queries never become
closed/offline/absent observations. Evidence and trusted native boundaries are
recorded in /home/doc/git/.acpi-proof-work/native-power-shared-work/VALIDATED.json.
No new SPARK proof or native/laptop validation is claimed.
