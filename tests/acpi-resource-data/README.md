# Resource framing and typed descriptors

Run in the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp set before
and inside Nix:

    python3 tests/acpi-resource-data/run.py --mode release
    python3 tests/acpi-resource-data/run.py --mode checked
    bash tests/acpi-resource-data/check_reference.sh

The original ten-main milestone covered framing, basic descriptors, extended interrupts,
address spaces, common geometry, memory/registers, DMA, dependent groups, Extended Address Space and actual-service decoding. Both profiles
passed 2,712,815 checks: 5,063 framing, 132,209 basic, 67,041 extended,
7,089 address, 1,693,071 geometry, 526,925 memory/register, 221,480 DMA, 51,829 dependent, 7,760 extended address and 348 service.
Independent review verified 138 ALI files and 128 strict compiler commands per
profile, including source, tool/runtime, binary and artifact hashes. All twenty
build/runtime stages passed per profile (twenty stages total). Immutable tested
inputs/results are in resource-extended-address-AOenpOUU; publication carries
identical Ada/GPR inputs.

Basic decoding covers IRQ, I/O, fixed I/O and Memory32Fixed. Extended interrupt
tests cover flags, counts 1..255, full unsigned values, _CRS/_SRS versus _PRS use,
source indices, opaque name bounds/terminators and malformed frames. Address
checks cover Word/DWord/QWord field widths and bit positions, reserved flags and
types, full unsigned values, source tails and width-specific index policy.

Normal iASL compiled reference/resources.asl without force or suppression into
1,108 bytes of AML. Acpiexec returned sixteen exact _CRS/_PRS buffers, checked against
reference/expected.tsv. Six new address descriptors include memory above 4 GiB,
Word bus numbers and named sources in all widths. Actual Core tests evaluate,
copy, compare every byte, decode fields and release retained buffers. The copy
array is sized from each expected buffer, replacing its earlier 64-byte bound.
Earlier fixture operator-visibility and copy-size failures remain recorded.
Raw logs retain five warnings (empty template/four missing _PRS companions) and
four outstanding cache allocations. Native or hardware execution is unverified.

Names are opaque bytes, not resolved paths. Optional source tails require an
index and a final NUL; index-only tails or bytes after the first NUL fail. Empty
strings are retained. Extended interrupts and QWord addresses have independent
negotiated zero-index policies; Word/DWord address indices retain meaning.
See userspace/services/acpi/resource-data/ADDRESS-DESCRIPTORS.md for numeric and
flag scope, including raw PCC/vendor flags and unchecked address geometry.
No decoded description confers hardware authority, mapping or routing rights.

Geometry tests compare feasibility against an independent integer endpoint
enumerator for small domains in each encoded width and use. They check the
returned witness independently, then exercise every granularity power through
bit 63, full-width ranges, constructed out-of-width values and alignment failures.
Actual service tests now assess all six normal address descriptors as assigned
ranges. This common policy does not allocate, translate or establish ownership.

The unchanged parent framing proof retains 51 checks/93 goals plus 10 flow
checks for bounds/progress/quota/termination, not full checksum functionality.
The parent and three children now have a scoped milestone proof: 297 checks /
517 goals and 33 flow checks, with all 14 entities in SPARK, no assumptions,
skips or unresolved checks. This covers runtime safety and existing contracts,
not full field-value/checksum equivalence or geometry. See
tests/acpi-descriptor-proof/README.md. All applicable families, admission,
routing and native consumers remain unfinished. The geometry grandchild and new memory/register children are
tested but outside that four-unit proof. Historical predecessor counts
retain their original narrower scope.

The memory/register fixture checks all 65,536 encodings of each Memory24 field,
Memory32 field bit positions, ignored information bits, all GAS byte fields and
access codes, full 64-bit addresses, malformed lengths and high array bounds.
Actual Core tests include seven descriptors from three new buffers (Memory24,
Memory32 and generic registers); the original ten reference buffers are unchanged.
Memory24 ASL arguments are encoded units, and its wire bases/lengths expand in
256-byte units. Initial normal-compiler fixture errors are retained. Generic
registers reuse ACPI_FADT.Generic_Address and preserve zero fields/space IDs;
no backend is selected. See MEMORY-REGISTERS.md for parsing and admission limits.

A subsequent proof milestone separately covers geometry and memory/registers:
79 checks/198 goals plus 17 flow checks, all nine entities in SPARK with no
assumptions, skips or unresolved checks. This proves safety/extent and the
feasible-witness contract, not feasibility completeness or full parsing semantics.
The earlier four-unit proof is separate, on unchanged sources. See
 tests/acpi-resource-policy-proof/README.md for exact inputs and scope.

DMA tests cover all 256 masks/information bytes together, all 65,536 values of
both fixed request/channel fields, all width codes, use context and bounds.
The additional normal _CRS contains compatibility DMA, Type F DMA and FixedDMA;
all thirteen previous vectors are unchanged. Actual Core tests also reject
FixedDMA as _PRS/_SRS. New DMA functionality is outside the two prior scoped
proof receipts; see DMA-DESCRIPTORS.md for controller/authority limits.

Dependent-group tests compare all token sequences through length seven against
an independent positional grammar at several capacities. They verify all priority
bytes, marker/body spans, common item counts, source order, error atomicity and
high array bounds. The normal _PRS adds two sibling alternatives with common
vendor items before/after, preserving the fourteen earlier vectors. Actual Core
also checks the old default-priority example. Earlier unused fixture visibility
clause failures are retained; no production change was needed. See
DEPENDENT-GROUPS.md for precise structural scope; this child is not yet proved.

Extended Address Space tests cover revision-one fixed length, role and reserved
bytes, all specific/type flags, every 64-bit field bit and distinct attributes,
non-memory reserved attributes, bounds and the geometry adapter. Normal replay
adds ExtendedMemory/ExtendedIO/vendor ExtendedSpace and preserves the fifteen
previous vectors. The memory cache bits are explicitly ignored in this format;
attributes remain raw claims. See EXTENDED-ADDRESS.md. No new proof is claimed
for Extended Address Space, DMA or dependent-group code in this functionality
batch; both earlier scoped proof source sets remain unchanged.

GPIO connection coverage adds an eleventh main, resource_gpio_tests, and a
seventeenth normal ACPICA buffer (GPI0._CRS). The 590,956-check pure matrix covers
both connection kinds, all word flags/offsets, raw units/pins, reserved fields,
maximum descriptor length and high input bounds. Actual Core typed replay covers
three normal GPIO descriptors and retained-result cleanup. Existing sixteen
vectors are unchanged. See resource-data/GPIO-CONNECTIONS.md for structural
source-span semantics and the boundary between copied descriptions and controller
admission. This child is tested functionality, outside the three prior scoped
resource proof batches. Original failed compiler fixtures remain separate.

The eleven-main GPIO milestone passes 3,303,913 checks/profile, all twenty-two
build/runtime stages in both release and assertion-enabled builds. Independent
review checks 144 ALI files/133 strict compiler commands per profile plus source,
reference, toolchain, runtime and artifact hashes. Accepted immutable context:
resource-gpio-final-1VjdI7I1/source, results/release-lavz67au and
results/checked-h2bd_pgq under this suite. The normal reference is 1,244-byte AML
with seventeen buffers; prior sixteen are byte-identical. Five compiler warnings
and four cache allocations at ACPICA teardown remain recorded, not suppressed
or treated as clean teardown. Publication adds documentation/metadata after
those runs with identical Ada/GPR sources; no new proof is implied for GPIO.

The retained resource-buffer reader adds focused actual Core coverage: 480
checks/profile, source resource-buffer-reader-clean-CKA8ugtH, release-rs4tbwns
and checked-qxkiersu. Both two-stage selections passed and were independently
audited (90 ALI/89 strict commands/profile). It borrows a retained result for
exact caller-sized copy/framing and never invokes AML or releases roots. Tests
cover capacity retry without repeated _CRS effects, high output bounds, stale/
wrong-owner handles, wrong types, malformed buffers, quota and cleanup. The
normal 1,364-byte fixture preserves all seventeen earlier resource vectors and
adds counter0/_CRS six-byte buffer/counter1 observations, checked by the reference
checker. Malformed named buffers are constructed error cases. This focused run
does not claim all eleven mains were rerun after adding the reader. Parser
production code and the prior full-family evidence remain unchanged. See
userspace/services/acpi/resource-consumer/README.md; reader proof/native work
remain unfinished. Publication adds documentation/metadata after the accepted
runs with identical Ada/GPR sources.

PCI routing is the twelfth main, pci_routing_tests. It passes 263,543 checks
including both AML widths, package NamePath/device identity and pin cleanup.
Focused checked pci-routing-cases-qwLTA5XR/checked-ka321pm5 passed 264,023 checks
across routing and the 480-check resource reader (four stages, 170 ALI/168 strict
commands). Full release-1n68hsug passed 3,567,557 checks, twelve mains/24 stages,
225 ALI/213 strict commands. All tool/runtime/source/artifact hashes were audited.
No all-twelve checked run or new proof is implied. The package-specific Method
name binding fix is part of the tested namespace; normal method invocation
remains separate. See userspace/services/acpi/pci-routing/README.md.
Normal ACPICA reference checks additionally compare four typed _PRT rows in
32/64-bit modes, Method package references, call effects, and integer truncation.
The existing seventeen resource vectors remain unchanged; reference diagnostics
and the first failed candidate remain retained. Added documentation/metadata and
reference check assets follow the runs with identical Ada/GPR sources.

The serial connection extension adds resource_serial_tests as the thirteenth
default main. I2C/SPI/UART revision1/2 and CSI-2 revision1, common vendor envelopes,
flags, lengths, addressing, full-width fields and bounds pass795976 pure checks.
The normal2,092-byte AML fixture adds seven connection descriptors in SBL0._CRS;
all seventeen older resource vectors are byte-identical, and both-width PCI
route/method/counter checks still pass. The original five warnings/four remarks
and cache-allocation diagnostics remain retained, not suppressed.

Actual immutable source serial-resource-replay-JM23BNqL/source passed focused
release-mohp9te9 and checked-an4bpep8:1,060,046 checks/profile =795976serial+
263543PCI+527actualCore. All three mains/six stages pass/profile. Independent
source/reference/tool/runtime/ALI/compiler/binary/artifact audits verify176 ALI
and173 strict compiler commands each. The expanded actual Core fixture checks
all bytes, settings/spans and retained-root cleanup for the new buffer and the
previous seventeen. Publication carries all219 production and1051Ada/GPR inputs
unchanged. This is not an all-thirteen rerun or a serial-unit proof; the earlier
217 production identities and six separate proof source sets are unchanged.
The first candidate's unused test visibility clause was rejected by strict
warnings; its failure is preserved, and no production change was needed for it.
See resource-data/SERIAL-CONNECTIONS.md for controller and semantic limits.

Pin resources add resource_pin_tests as the fourteenth default main. The five
revision-one forms pass1,969,547 pure checks; direct pins, named groups and group
references have distinct typed fields/spans. The normal2,271-byte AML fixture
adds five descriptors in PIN0._CRS, preserving all eighteen prior buffers.
Normal five warnings/four remarks/cache diagnostics remain retained. Exact
source pin-resource-replay-3lfzN9S2/source passed focused release-1v8mif1s and
checked-9zikjbz7:3,029,640 checks/profile =1969547pin+795976serial+263543PCI+
574actualCore. Four mains/eight stages/profile pass, with182ALI/178strict compiler
commands independently audited per profile plus all input/tool/runtime/compiled/
binary/artifact hashes. Not an all-fourteen rerun or new pin/serial proof.
Publication carries all221 production and1056Ada/GPR files unchanged. The earlier
219 production inputs are unchanged; prior exact-source receipts retain their
own scope. Maximum-size pin-list execution and configuration-value/controller
admission remain separate gates; see resource-data/PIN-DESCRIPTORS.md.

Clock resources add resource_clock_tests as the fifteenth default main. The
197879-check pure matrix covers exact fractions, all flags/divisors/indices,
optional source rules, numerator/unit boundaries and maximum source size. Pin
maximum-list coverage adds34checks, bringing its pure matrix to1969581. The
normal2360-byte AML fixture adds three clocks in CLK0._CRS; all nineteen earlier
buffers are byte-identical and both-width PCI/reference/counter checks remain.
Original five warnings/four remarks/cache diagnostics are retained.

Actual source clock-resource-replay-GV9Zkbz3/source passes3,227,583 focused
checks/profile =197879clock+1969581pin+795976serial+263543PCI+604actualCore in
release-zf5oef1v and checked-qzpabpax. Five mains/ten stages pass per profile;
independent audits verify188ALI/183strict commands each and all input/reference/
tool/runtime/compiled/binary/artifact hashes. Publication carries223production/
1061AdaGPR inputs unchanged. Not an all-fifteen rerun or new serial/pin/clock
proof. The old221 production inputs and previous selected proof inputs remain
unchanged. Initial test operator-visibility/branch syntax diagnostics are
preserved; no production change was required to correct those fixtures.

At the three-unit proof milestone, exact source connections-contracts-A3wVB00I
also passes all3,227,583 focused checks/profile, five mains/ten stages in
release-cwuqcsj7 and checked-ug4xc2lj. Both188ALI/183strict-command audits pass,
with all source/reference/tool/runtime/compiled/binary/artifact hashes. Same
source has the scoped311checks/526goals+48flow proof; see the connections proof
README for its limited semantic claims. Publication carries223production and
1062Ada/GPR inputs unchanged; not all fifteen default mains or whole AML/native.
