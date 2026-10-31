# EC supervisor policy proof

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-supervisor-proof/run.sh

Runner supports --source-root DIR and --validate-only. Inputs/tools are pinned;outputs private,build/proof180s,runtime30s,solver5s,j1/nice19,64MiB stack and1MiB logs.


## Supervisor control and completion policy proof milestone

The actual EC supervisor now uses ACPI_EC_Supervisor_Control as its sole admission/completion state; separate Completed,Halted,Awaiting_Completion and charge variables are removed. Named phases distinguish fresh/ready,open advance,open cleanup and stopped. Open states cover active or interrupted calls and close admission;they imply no physical quiescence. The policy preserves exact event identity,charge and original ceiling through stop,cleanup and refused operations. Retention refuses zero identities,overcharge,overwrite or wrong phase;confirmation requires the exact retained controller/epoch/token/number during advance. The owner invokes confirmation only after a truthful confirmed callback.

SPARK levels1/2 each discharge12proof+37flow checks over18fully analyzed entities,including7exact functional postconditions,16operation terminations,16Global-null effect checks,default initialization and discriminant safety. Each has70validCVC5+10validTrivial leaves,zero skipped work and no external proof dependencies.20altered reports per level are rejected against a source-linked complete obligation/claim inventory. Proof is of this actual pure policy under its valid-state/transition contracts. Native caller composition,callbacks,source authentication,notification/GC behavior,loop scheduling and physical ownership/lifetime are not thereby proved.

Fresh16324hosted checks/102processes pass on exact sources,checked/release:15324actual service/AML/receiver/broker/supervisor/control-loop checks at both AML widths plus1000focused policy checks. Direct tests cover full64-bit identity,invalid identity,overcharge,all identity components in confirmation,duplicate/phase refusal and payload preservation through stop/cleanup. Existing ACPICA Q10 values/6notifications still match with cache diagnostics qualified,not a reference proof of policy or hardware. Final function effect contracts were revalidated with incremental private builds after preserving preliminary source/logs;strict ALI flags and all assertions remain. No native-runtime semantic compile,native/kernel/image/QEMU execution,kernel/runtime/launcher/CCL/default startup change or hardware acceptance. Full verified interpreter/service and provisioned source/backend/recovery/lifetime integration remain unfinished.
