# Actual service DataTableRegion import regression

In the repository's pinned Nix shell, with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-table-region-import/run.py --validate-only
    bash tests/aml-table-region-import/check_reference.sh
    python3 tests/aml-table-region-import/run.py --mode release
    python3 tests/aml-table-region-import/run.py --mode checked

This opt-in suite stages the exact normal iASL DSDT in ACPI_Service, freezes the
catalog, executes firmware modules, initializes members, and evaluates named
objects through the service API. It does not use the legacy static name loader.
One DataTableRegion uses three previously declared string names. Another uses
three previously declared selector methods with a shared counter. Both regions
select the immutable staged DSDT; ByteAcc fields read header revision 2. Seven
observations match normal ACPICA: counter 3 after import, both field reads 2,
and counter still 3 after repeated reads. Immediate scalar results leave the
retained-root count unchanged. No hardware provider is invoked.

The exact production and Ada/GPR inputs passed 39 checks in each strict profile
at private table-region-import-scalar-mNavJa1f: release-nllelrpl and
checked-4jtot7my. Independent reviews checked input/tool/runtime/ALI/binary/output
hashes and compiler profiles (78 ALIs and 77 strict compile commands/profile).
The checked profile enables assertions and overflow checking. Review receipts
are in reference/hosted; original detailed logs remain at their recorded paths.
The first test-only failures are preserved in the private predecessors: unused
import rejected by warnings-as-errors, then a mistaken owned-object expectation
for the API's immediate integer result. No production change was needed.

ACPICA 20260408 compiled the fixture without errors, warnings or remarks.
Original execution logs retain four outstanding-cache-allocation diagnostics;
clean ACPICA teardown is not claimed. check_reference.sh validates all seven
successful observations, exact embedded bytes and table checksum; process exit
zero alone is insufficient. Normal runs use cached pinned oracle evidence and
do not regenerate it. Build/runtime limits remain 180/30 seconds, one compiler
job, nice 19, 64 MiB stack and 1 MiB logs. All output directories are disjoint.

Scope is this 64-bit DSDT/module-import case and these selector side effects.
It does not prove general operand coercion, forward references, all declaration
orders, malformed-selector recovery, dynamic Load/Unload, write authority,
service initialization completeness, whole-interpreter correctness or native
CuBit execution. Separate forward-reference experiments are not positive oracle
fixtures: forward method calls failed normal iASL compilation; forward string
names compiled but ACPICA reported missing objects and field evaluation errors.
No bug-fix claim or permissive oracle settings are derived from those cases.

No new SPARK proof is claimed. The serial/pin/clock proof milestone remains
independently scoped. The complete userspace ACPI and verified AML goal remains
active and incomplete.
