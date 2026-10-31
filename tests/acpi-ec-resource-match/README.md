# ECDT and namespace register-claim comparison

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-resource-match/run.sh

Use the pinned project shell. The portable runner supports --source-root DIR
and --validate-only. It pins245actual sources and three test inputs. Both
checked/release profiles pass292checks. Normal ACPICA AML and ECDT template
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
