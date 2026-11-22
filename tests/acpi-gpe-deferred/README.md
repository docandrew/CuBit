# Deferred GPE lifecycle hosted tests

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-gpe-deferred/run.sh

The Node runner supports --source-root DIR and --validate-only. It pins the
actual lifecycle and test main,checks strict checked/release ALI flags and equal
results,and uses bounded private outputs with j1/nice19,180s compile/30s runtime,
64MiB stack,no core dumps and1MiB logs. It performs no native IO.


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


## GPE lifecycle proof milestone

The actual synchronous/deferred GPE session now has a proved default initial condition, explicit termination contracts and exact preservation of the handler budget across completion and IO helpers. Idle validity includes its initial zero budget/nondeferred state. Three redundant initializers were removed: callback out values are assigned before normal use, while exception paths store their own explicit unknown reports. The analysis-only source ID is full64-bit.

The portable focused runner tests SPARK levels1/2 with full flow analysis. Each discharges15proof+21flow checks over16entities (12fully analyzed and4abstract imported callback specs),including7functional postconditions,9operation termination checks,default initialization and a client validity assertion. Each has264validCVC5+14validTrivial leaves,zero skipped work,and20altered proof reports are rejected. The four imported callbacks explicitly assume Global-null effects and termination. Truthful hardware effects/reports,exceptions,source/control/backing ownership and serialized completion attribution remain external; this is not whole-service or hardware proof.

Fresh6534hosted checks/78processes pass against these exact source hashes,checked/release:6336actual EC scan/private owner/AML/notification/lid/GPE integration and198deferred lifecycle. ACPICA explicit ordered events,trace and two notifications match both AML widths,with cache diagnostics qualified. Source/closure registration refreshes do not renew historical proof or runtime reports. No native-runtime semantic compilation,native/kernel/image/QEMU execution,kernel/runtime/launcher/default startup or hardware acceptance is claimed. Full verified interpreter/service and native authority/lifecycle integration remain unfinished. Portable: tests/acpi-gpe-proof/run.sh.
