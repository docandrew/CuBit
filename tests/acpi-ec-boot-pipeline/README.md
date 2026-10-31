# Joined EC boot association/discovery

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-boot-pipeline/run.sh

Use the pinned project shell. Runner supports --source-root DIR and --validate-only.
274 actual sources,two test inputs,normal ACPICA AML and ECDT template compilation.
Checked/release each pass197checks,including the preceding binding regressions.
This is a hosted composition,not a launched native controller or new authority API.

Actual production path:

1. Boot_Binding revalidates copied ECDT,path and local UID with finite fuel.
2. Discovery receives exactly Binding.Device and only the remaining fuel. It
   evaluates local PNP0C09/_GLK/_GPE/_CRS with the same actual Core/evaluator.
3. An intentionally undersized output leaves the original _CRS result owned.
   No incomplete description is published. Foreign-Core copy is refused.
4. Capacity retry copies that same result without AML or budget reset,then
   releases the root. Explicit _CRS side-effect counter remains exactly one.
5. Both sessions' namespace identities are current; discovery Device equals
   the UID-bound Device. GPE is explicitly FADT form and matches ECDT GPE bit.
6. Boot_Resources compares the actual returned _CRS bytes with the same ECDT.
   Data/control roles agree despite opposite table/resource ordering.

Sum of both reported charges is within the initial shared budget. Reusing the
same sessions cannot rerun UID/discovery or copy after terminal completion. A
fresh zero-fuel discovery evaluates nothing and leaves no retained root. Existing
wrong-type/cleanup/foreign-Core/contract/uncertainty binding cases remain covered.

The test begins after namespace construction and uses descriptive snapshots.
Is_Current checks binding identities,not immutable dynamic firmware values.
The absence of _GLK is not dedicated hardware ownership. Claims_Agree and the
matching GPE number grant no register/event capabilities. Reservation,GlobalLock,
source admission,provider installation and early ECDT access before namespace
evaluation remain native-controller responsibilities.

Independent reference after runner emits OUTPUT:

    bash tests/acpi-ec-boot-pipeline/oracle.sh OUTPUT/ec-fields.aml

Fresh repair-disabled ACPICA sees the expected full-width UID values,string and
alias-backed buffer,then exact EC00._CRS bytes
470162006200000147016600660000017900 and RCNT=1. It validates fixture semantics,
not the composed discovery algorithm. Four known-style ACPICA cache allocation
diagnostics remain in the raw shutdown log; no clean-shutdown claim is made.

No production code change,new proof,native execution or hardware validation.
Full SPARK interpreter/service verification and native integration remain open.
