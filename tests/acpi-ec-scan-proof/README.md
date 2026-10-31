# Actual EC scan allowance policy proof

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-scan-proof/run.sh

The runner requires GNATprove,GPRbuild and CVC5 from Nix. --source-root DIR
and --validate-only support exact private/current source checks. It proves only
the actual acpi_ec_query_scans body with its declared EC budget types; no model
replaces the runtime policy. Sources and proof recipes are pinned. Compilation
and each proof level are bounded to180seconds,j1,nice19,solver5seconds,64MiB
stack and1MiB logs. No whole-Core proof or native build is invoked.


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
