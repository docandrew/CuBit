# Indexed field register-width and AML-width regression

Run in the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-widths/run.py --validate-only
    bash tests/aml-index-widths/check_reference.sh
    python3 tests/aml-index-widths/run.py --mode checked
    python3 tests/aml-index-widths/run.py --mode release

The actual ACPI_Service_Core imports normal iASL AML and invokes methods through
five independently named simulated register bindings. The DSDT covers AnyAcc
and unaligned Word/DWord/QWord fields that span two access datums. It checks
read values, read-modify-write results, aligned byte offsets written to the
index register, WriteAsOnes and WriteAsZeros without data-read permission.
Both DSDT integer-width revisions run through the same main.

Normal ACPICA provides 17 observations per revision. A QWord-sized indexed read
and QWord data-field return are Integers under revision 2 and eight-byte Buffers
under revision 1; all buffer bytes, exact result length and retained-root cleanup
are checked. Buffer sources preserve the full QWord write in revision 1.
The revision-1 table differs only in revision/checksum; the AML body is identical.
check_reference.sh verifies typed observations in order, exact embedded inputs,
body identity and checksums. Exit zero alone is insufficient. The compiler
reported zero errors, warnings and remarks. Original allocation diagnostics are
retained; clean ACPICA teardown is not claimed. Normal tests use cached evidence.

The focused main has 342 checks. No production change was needed relative to
the preceding IndexField milestone. This is hosted test evidence, not a new
SPARK proof, complete access-attribute/geometry coverage, nested field routing,
Global Lock, BankField or native hardware/serialization validation. The provider
model stores fixed independently named register values, just as ACPICA's normal
simulated region; it is not evidence for a physical device's indexed bank state.

The scalar result/bit coverage and selector traces are distinct checks. The
normal oracle establishes resulting values; the ACPI index/data semantics and
retained ACPICA field-access implementation establish selector byte offsets and
reselection for Preserve reads and writes. Runtime requests still use only the
explicit named targets, never these logical offsets as addresses.

Limits remain 180 seconds/build, 30 seconds/invocation, one compiler job, nice19,
64 MiB stack and 1 MiB captured log. The full userspace ACPI/verified AML goal
remains incomplete. See ../aml-index-fields/README.md for ownership and policy
coverage and remaining integration/proof work.

Exact runtime source index-width-matrix-3cAPbdC9 passed checked-5zvqwiij and
release-j9k4pgrn. Independent receipts in reference/hosted verify all input,
reference, tool, runtime, ALI, binary and output hashes and strict switches:
77 ALIs/76 compile commands per profile, 1,367 checked and 1,058 release dependency
entries. Publication adds documentation/checker/receipts and metadata without
changing production or Ada/GPR inputs. Earlier nine-program 15,332-check/profile
Field evidence remains independently recorded on identical production code;
this two-revision test is an additional 342 checks/profile, not a rerun of that
whole earlier selection or of the full AML corpus.
