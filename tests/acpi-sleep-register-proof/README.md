# FADT sleep-model SPARK proof

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-sleep-register-proof/run.sh

Set TMPDIR before entering Nix too. --source-root DIR and --validate-only are
supported. Cached validation cross-checks exact root sources,source snapshots,
projects,commands and raw reports; it is not a new proof run. The runner creates
a fresh private8source closure and executes levels1/2 with180s process limits,
5s solver limits,j1,nice19,64MiBstack and1MiBlogs. No Core or native build.


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
