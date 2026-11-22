# Retained PCI routing entries

ACPI_PCI_Routing_Reader projects one zero-based row of an already-retained _PRT
Package. It validates the outer Package, four-field row, integer kinds, PCI
address (device0..31 and functionFFFF), pin0..3, source and DWORD source index.
Global-pool source zero yields a typed Global_Interrupt. A NamePath yields a
current authenticated Device descriptor plus Source_Index. String literals,
nonzero integers, nondevice metadata and type-only snapshots cannot establish a
link identity. Absolute, relative, forward and alias NamePaths preserve the
canonical device. ACPI_PCI_Routes is the separate pure numeric address policy.

The new service Read_Result_Device facade and Collecting.Read_Device_Metadata
validate a retained live namespace metadata value, its owner/incarnation and
actual Device kind before producing the descriptor. They add or consume no
roots. Captured metadata snapshots carry only a type and are rejected here.
Callers must revalidate detached descriptors before subsequent device operations.

The owner retains the outer Package and serializes service access throughout.
The reader acquires at most two temporary pins, releases each on success/error,
and defaults failed route output. Capacity is the caller's retained-root quota;
there is no hidden table-length cap or heap allocation. A defensive cleanup
failure returns remaining owned handles in Report.Pending, preserving cleanup
ownership instead of silently discarding roots. The caller owns/release Table.
No AML invocation, interrupt programming, PCI bus association/swizzling, link-HID
validation, _CRS/_PRS/_SRS calls, descriptor-index admission, duplicate-table
policy or IRQ capability grant occurs. Source indices are raw claims, including
full DWORD values; structural parsing is not whole-table/platform admission.

Testing found that static, named-runtime and unnamed-runtime package binding
omitted Method name references. A package-only metadata predicate now includes
Method_Object in all three paths. Ordinary expression/method-call lookup keeps
its prior predicate. Package method references remain noninvoking metadata;
ordinary nested method invocation still runs exactly once. This extends AML
functionality; it does not claim complete package/reference semantics or proof.

The checked PCI program passes 263,543 checks in one bounded run covering both
32/64-bit DSDT modes. It enumerates address word boundaries and high bits, reads
normal link/global rows, checks aliases/forward paths, malformed rows/types,
width-normalized integers, metadata snapshot/owner/staleness, complete output
reset, and both retained-root acquisition boundaries. At exact two-pin headroom
it succeeds; no pins escape. Two runtime method-package forms plus the static
initializer are checked against counter0, then an ordinary nested call/counter1.
The existing resource-buffer reader passes 480 checks alongside it.

Accepted context: pci-routing-cases-qwLTA5XR/source,
tests/acpi-resource-data/results/checked-ka321pm5 (two mains/four stages,
170 ALI/168 strict compiler commands) and release-1n68hsug (all twelve mains/
24 stages, 3,567,557 checks, 225 ALI/213 strict commands). Independent audits
verify inputs/tools/runtime/compiler/ALI/binaries/artifacts. This is not an
all-twelve checked rerun. Publication adds docs/reference checks after those
runs with identical Ada/GPR sources. The initial checked-hor_3oox failure for an
uninitialized Method member remains retained, not accepted or golden-adjusted.

Normal ACPICA compiles the fixture into 1,847-byte AML and returns four _PRT
rows: a global GSI23 and three references to LNKA Device. Both widths preserve
those rows and package Method references; actual calls increment the counter.
The 32-bit oracle changes only DSDT revision/checksum, preserves every AML body
byte, and records normative truncation warnings for large constants. Exact
observations are checked by check_pci_reference.sh. The initial UTF-8 byte-output
helper failure was caught by the preservation check before any oracle ran; the
corrected helper uses the C locale. All original seventeen resource vectors are
unchanged. Compiler five warnings/four remarks, a per-RPNM allocation notice and
four cache allocations at teardown remain recorded; no clean teardown claim.

New routing, metadata facade and package fix are tested SPARK-mode code, not a
fresh proof milestone. Existing pure resource-parser proof sources remain
unchanged; no carry of historical whole-namespace proofs onto this modified
namespace body is claimed. Native routing and whole AML correctness are open.

Specification: ACPI 6.6 section 6.2.14 (_PRT), table6.16:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#prt-pci-routing-table

The pure numeric ACPI_PCI_Routes helper now has a separate accepted proof:
9 checks/14 goals plus 2 flow establish runtime safety, exact address validity
classification and decoded device value. The GPIO unit adds 99/176/17 in the
same two-unit batch. This does not extend proof to the routing reader, metadata
getter or package/namespace fix. See tests/acpi-gpio-routing-proof/README.md.
Final contracts are also exercised by 854,979 focused checks/profile across GPIO,
PCI and the actual resource reader; earlier full-release evidence stays separate.
