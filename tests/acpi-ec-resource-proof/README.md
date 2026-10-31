# Register-claim comparison SPARK milestone

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-resource-proof/run.sh

Use the pinned project shell. --source-root DIR and --validate-only are supported.
check-proof.mjs independently validates cached source/report/project linkage.
The portable runner reproduces both levels with26actual pure dependency sources;
no Core stub or interpreter unit is selected. The comparison body retains its
existing runtime logic; operator imports moved to the spec for its ghost contract.

Actual ACPI_EC_Boot_Resources.Compare now has a proved functional postcondition:
Claims_Agree iff the existing boot compatibility gate succeeds and the decoded
pair agrees on both roles' spaces/full addresses,with no IO10-bit review or
declared read-only restriction. Space/address mismatch reports expose the actual
mismatch; decode/read-only review reports retain their exact attribute evidence.
Descriptions_Agree is a ghost specification,not an alternate runtime checker.

Levels1/2 each:3all-SPARK entities,5flow/termination results,16proved checks,
one compound functional postcondition,35CVC5+4Trivial Valid attempts. No skipped
or unresolved checks. Ten synthetic audit cases reject empty,non-SPARK,
missing-post,unproved and skipped trees. Full unit.spark trees,logs,original
projects,commands and source-snapshot hashes are retained and cross-checked.

This is modular comparison proof under the actual decoder interfaces/contracts.
It does not prove those parsers' implementations,byte provenance,same-device
association,physical backing,IRQ identity or hardware authority. Claims_Agree
is descriptive. It is not full AML/service verification or native acceptance.

Final-source functional regressions remain292resource checks plus197joined
ECDT/UID/discovery checks in each checked/release profile. Normal ACPICA ECDT
and AML fixtures are retained by those suites; the resource suite freshly
matches12whole _CRS buffers with acpiexec. The joined fixture is unchanged;
its earlier independent UID/CRS reference remains scoped to those exact bytes.
The checked profile exercises the proved postcondition at runtime too.

Proof attempts before final import cleanup are private history only. An early
hosted build caught redundant imports after adding specification visibility;
imports were removed with warnings still enabled,then both proof levels and
both hosted suites reran. No contract weakening or compiler-crash research.
