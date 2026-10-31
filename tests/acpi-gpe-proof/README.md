# Focused GPE lifecycle proof

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-gpe-proof/run.sh

The Node runner supports --source-root DIR and --validate-only. It pins the three actual proof inputs,inventory and tool paths,uses private outputs,j1/nice19,180s per level and5s solver limits,64MiB stack and1MiB logs. Strict audits require the complete source-linked obligation inventory,valid proof leaves,unchanged assumptions and explicit imported callback scope. Imported callback bodies and hardware are not verified here.


## GPE lifecycle proof milestone

The actual synchronous/deferred GPE session now has a proved default initial condition, explicit termination contracts and exact preservation of the handler budget across completion and IO helpers. Idle validity includes its initial zero budget/nondeferred state. Three redundant initializers were removed: callback out values are assigned before normal use, while exception paths store their own explicit unknown reports. The analysis-only source ID is full64-bit.

The portable focused runner tests SPARK levels1/2 with full flow analysis. Each discharges15proof+21flow checks over16entities (12fully analyzed and4abstract imported callback specs),including7functional postconditions,9operation termination checks,default initialization and a client validity assertion. Each has264validCVC5+14validTrivial leaves,zero skipped work,and20altered proof reports are rejected. The four imported callbacks explicitly assume Global-null effects and termination. Truthful hardware effects/reports,exceptions,source/control/backing ownership and serialized completion attribution remain external; this is not whole-service or hardware proof.

Fresh6534hosted checks/78processes pass against these exact source hashes,checked/release:6336actual EC scan/private owner/AML/notification/lid/GPE integration and198deferred lifecycle. ACPICA explicit ordered events,trace and two notifications match both AML widths,with cache diagnostics qualified. Source/closure registration refreshes do not renew historical proof or runtime reports. No native-runtime semantic compilation,native/kernel/image/QEMU execution,kernel/runtime/launcher/default startup or hardware acceptance is claimed. Full verified interpreter/service and native authority/lifecycle integration remain unfinished. Portable: tests/acpi-gpe-proof/run.sh.
