# IndexField with an independent data-field width

Run in the pinned development shell with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-data-widths/run.py --validate-only
    bash tests/aml-index-data-widths/check_reference.sh
    python3 tests/aml-index-data-widths/run.py --mode release
    python3 tests/aml-index-data-widths/run.py --mode checked

The actual Core/AML interpreter imports the normally compiled 552-byte DSDT and
matches all 47 ordered Integer observations from ACPICA. The original conformance sequence contributes 244 checks; the expanded suite
passes 4,046 checks in each release and assertion-enabled profile. The fixture uses independently named Byte index and DWord data
registers; logical IndexFields receive no implicit hardware authority.

Coverage includes Byte/Word/Any whole-datum reads from a wider data field,
unaligned and multi-datum reads through 64 bits, QWord access through the narrower
DWord data field, whole stores, and Preserve/WriteAsOnes/WriteAsZeros updates.
Whole-datum reads expose the DataName value. Other reads assemble access-sized
chunks with final-tail masking. Preserve keeps returned bits outside the mask;
WriteAsOnes fills those bits before the independent data field stores the scalar.
A QWord store is truncated by the actual DWord field, not by the indexed access.

Primary references are ACPI 6.5 section 19.6.63 and the official ACPICA
source/components/executer/exfldio.c field-transfer, extraction and update logic.
The reference checker verifies ordered types/values, exact embedded AML, table
extent and checksum. Compiler output has zero errors, warnings and remarks.
The original ACPICA allocation diagnostics remain in the log; a clean teardown
is not claimed. The provider models fixed register values, so these observations
do not establish a physical device's indexed bank behavior.

The previous implementation rejected independent data widths. The first relaxed
implementation passed the original 22 observations but failed the expanded
WriteAsOnes observation; both the failure and correction are retained at the
origins recorded in the checkpoint HANDOFF. Expected outputs were not weakened.

This suite is hosted functionality evidence, not a new SPARK proof or native
hardware acceptance. Data fields remain scalar 1..64 bits. Wider fields,
protocol attributes, GlobalLock/concurrency, wider width combinations, definition
block lifetimes, full interpreter proof and native integration remain open.
Existing AML32/64 width, composed graph and recovery suites have separate scopes.

Limits: 180 seconds/build, 30 seconds/invocation, one compiler job, nice19,
64 MiB stack, 1 MiB captured log; normal ACPICA commands use five seconds each.

## Mixed-width permissions and interrupted operations

The expanded main retains the complete 47-observation conformance sequence and
adds an independent request plan for all 17 read/store methods. Every planned
request compares the full provider/endpoint/epoch/register target, direction and
value. The model uses the same two explicitly bound named registers.

Reads cover whole Byte/Word/Any/QWord, the unaligned 8/12-bit fields, two-byte
16-bit fields and both eight-byte 64-bit fields. Stores cover Byte/Word/QWord,
partial Preserve, partial Ones/Zeros and whole-byte Ones. These are the exact
fixture shapes, not all possible access/data-width combinations.

The permission matrix denies selector writes, data reads and data writes before
effects. Whole/Ones/Zeros stores succeed without data-read permission; partial
Preserve requires it. Revocation after the first selector write prevents the
following data access and retains that completed selector effect.

The 72 planned request positions (56 for read methods and 16 for store methods,
including their selector writes) are
each interrupted before and after their effect: 144 cases. Tests compare every
request prefix, the exact retained observation and all completed physical-register
values. Reinvocation while uncertain makes no requests. Reconciliation rejects
unconfirmed observations, wrong operation identifiers and changed request values;
only exact modeled terminal confirmation clears uncertainty. A subsequent explicit
invocation performs the complete expected request sequence and reaches the final
state, including the upper bits of the wider data register.

The confirmation source is a trusted hosted model. It does not implement native
terminal-evidence delivery or establish physical-device completion. Independently
varying indexed data, further width combinations, AML32 mixed widths, concurrency,
transport/backing lifetime and native integration remain open.
