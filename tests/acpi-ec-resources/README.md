# EC namespace resource projection and actual AML-buffer tests

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-ec-resources/run.sh
```

Export TMPDIR before Nix. Outputs are private, nice19/j1,180s build/30s runtime,
5s ACPICA commands,stack64MiB/core0,logs≤1MiB. Checked enables assertions and
overflow checks; release suppresses checks. Warnings fail Ada compilation.
`--validate-only` verifies hashes only; `--source-root DIR` selects an explicit
matching source tree. Actual ALI switches, command outcomes and source hashes
are checked. No native/kernel/image/QEMU build or execution is involved.

The actual ACPI service core evaluates twelve _CRS objects. The existing
retained-buffer reader first reports required capacity, then copies from the
same handle into larger caller storage. A method counter confirms one AML
evaluation. Complete buffer type/length/bytes match fresh repair-disabled
ACPICA output; the projection then checks data-first/control-second roles,
byte widths,full64-bit addresses,IO decoding and memory writeability claims.

Cases include basic/fixed IO,fixed/legacy32 memory,QWord memory,Word/DWord IO,
IRQ and extended IRQ prefixes,GPIO interrupt tails,duplicate GPIO rejection,
missing/extra registers,truncation/framing,item quota,malformed flags,
unknown descriptors,nonstatic ranges,aliasing,zero addresses,high array origins
and preserved encoded high bits in10-bit IO. Unknown descriptors are refused,
not skipped across a possible EC address slot. Translation/source dependencies,
nonpositive decode and unsupported layouts return typed refusals. A successful
projection grants no access; actual backing/ownership and aliases need separate
platform admission. Memory writeability flags are not write permissions.

The fixtures also exercise a compressed integer _HID,scalar _GLK/_GPE,and an
ordinary device-reference/integer package with explicit root/child cleanup.
These are Core/reader tests, not a production discovery orchestrator. Normal
iASL20260408 rejected package-valued _GPE as both Name and returning Method
(Error6105,Package found/Integer required), despite the form in ACPI6.6 12.11.
Those failures remain in publication evidence. The accepted GPES helper tests
package shape only; it is not reserved package-valued _GPE reference coverage.
No forced AML or diagnostic suppression was used. The empty-resource warning
and ACPICA shutdown cache allocations remain in raw logs.

ACPI_EC_Resources is SPARK_Mode but this change has no new formal proof. Full
_HID/_CRS/_GPE/_GLK discovery sequencing,budget/retained-root ownership across
its failures,native admission/locking,_REG/provider attachment,event routing,
hardware acceptance and the complete verified service/AML goal remain pending.

Normative EC namespace resource ordering and _GPE forms:
https://uefi.org/specs/ACPI/6.6/12_Embedded_Controller_Interface_Specification.html#defining-an-embedded-controller-device-in-acpi-namespace
Global Lock declaration/defaults:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#glk-global-lock
