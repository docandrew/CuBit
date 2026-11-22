# ECDT and namespace register-claim comparison

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-resource-match/run.sh

Use the pinned project shell. The portable runner supports --source-root DIR
and --validate-only. The current registration pins910 inputs (505 compiled source candidates and
405 audit-only inputs), plus three unchanged test inputs. Both profiles pass
292 checks and validate the exact 98-unit compiled ALI inventory. The focused
checked profile enables assertions only for ACPI_EC_Resources and
ACPI_EC_Boot_Resources. Core and its direct test instantiation use -O0 -gnatp
to avoid the previously documented GNAT code-generation failure. This is
functional test evidence, not whole-interpreter assertion or proof evidence. Normal ACPICA AML and ECDT template
compilation are used. Twelve actual Core/AML _CRS buffers match independent
acpiexec buffers byte-for-byte,including descriptor extent/order/end tags.

ACPI_EC_Boot_Resources.Compare re-decodes both copied byte sequences. After
validating ECDT byte-register compatibility and namespace resource projection,
it compares address space and full64-bit address separately for data/control.
ECDT lists control first; _CRS lists data first. Unspecified/byte ECDT access
sizes are compatible after validation; struct byte equality is not required.

Both role/address pairs are checked before extra namespace attributes. IO10-bit
decode returns Decode_Review_Required,and declared read-only memory returns
Read_Only_Review_Required,even if addresses agree. These are conservative platform
review outcomes,not proof that the firmware is invalid. A review on the first
role cannot conceal a mismatch on the second. Addresses wider than16bits in an
IO ECDT fail the existing compatibility gate rather than being truncated.

Claims_Agree describes these copied register claims only. It establishes no
_UID/path/device identity,_GPE/IRQ/GPIO equivalence,GlobalLock,provider readiness,
hardware reservation or capability. The native controller must separately
establish that the _CRS bytes belong to the exact ECDT-bound Device and remain
current; this pure helper accepts no caller-supplied descriptor or authority.
Matching data from unrelated devices must never be used as source admission.

Report includes exact decode statuses,examined role and original GAS/namespace
claim for bounded bring-up diagnostics. No IO or AML evaluation occurs inside
Compare. The hosted composition obtains actual _CRS from Core and releases roots.

Tests exercise normal IO/fixedIO/word+dwordIO/memory32/qword-memory/GPIO/IRQ
templates,read-only/decode review,address-space/address mismatches in both roles,
compatibility failure,checksum failure and access-size normalization. Existing
resource-parser and root-cleanup regressions remain. No native or physical
hardware execution and no new proof claimed; SPARK_Mode is not solver evidence.
Whole-service/interpreter correctness and native admission remain unfinished.


## Successor functional proof

Actual Compare agreement and typed mismatch/review diagnostics now have a
level1/2 proof milestone:16checks,one functional postcondition,3SPARK entities,
5flow results per level. Decoder semantics/provenance/hardware remain separate.
Both this292-check suite and joined197-check discovery suite pass both profiles
on the final source. See tests/acpi-ec-resource-proof for exact retained scope.

The 2026-11-11 registration refresh preserves all three original test fixtures
and all explicit checks. Target runtime files remain pinned audit witnesses,
not Linux compiler inputs. The obsolete s-memory.adb audit witness is removed
because it is absent from the current tree. No old runtime body is restored.
Source-directory candidates exceed the actual compiled closure; hash coverage
does not establish execution or proof. The historical proof section above
describes its retained milestone; this refresh runs no proof.
