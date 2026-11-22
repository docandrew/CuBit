# Serial bus connection descriptions

ACPI_Resource_Data.Serial_Connections decodes copied GenericSerialBus resource
items (large name0x0E) for I2C, SPI, UART and CSI-2. It returns descriptor-local
metadata as named types and absolute spans into the caller's exact copy. No
allocation, controller lookup, transaction, mapping or hardware authority follows.
The caller retains the data copy while using its spans.

The common header preserves revision, source index, bus type, type revision,
raw specific flags, role, initiator and sharing. TypeDataLength partitions fixed
settings, opaque vendor bytes and the final source string. Declared lengths
must fit this descriptor, not merely the containing buffer. The source must have
exactly one final NUL; its syntax, nonemptiness and namespace identity remain
unchecked. Vendor bytes can contain anything. No fixed output array or name-size
cap is introduced; the wire payload is bounded by its16-bit length.

Standard bus types support type revision1. I2C/SPI/UART descriptor revisions1/2
are accepted; CSI-2 accepts revision1. V1 reserves the sharing bit. V2 sharing
for all three buses follows the explicit I2CSerialBusV2/SPISerialBusV2/
UARTSerialBusV2 macro definitions and ACPICA generation. ACPI6.6 table6.55 says
sharing is restricted to I2C, while the SPI/UART descriptor tables still describe
V1; those statements conflict with the V2 macro arguments. The distinction is
explicit rather than rejecting normal SPI/UART V2 Shared output. CSI-2 stays V1.

I2C preserves full32-bit speed and accepts7/10-bit addresses only within their
encoded width. The high specific-flags byte is retained as the ACPI6.6 LVR for
I2C devices attached to I3C hosts. Decode does not know the controller protocol;
I2C-only reserved-high-byte rules and I3C LVR/seven-bit requirements must be
checked when admitting the connection to a particular controller.

SPI preserves full speed, device selection and raw transfer-unit byte; it checks
reserved flags and clock-phase/polarity codes. Controller constraints on word
size, speed and selection are separate. UART checks defined data/stop/flow/parity
codes and reserved line bits, preserving raw speed and16-bit FIFO extents. CSI-2
checks the PHY code and exposes the local port plus remote source index. SPI/
UART source indices are reserved and must be zero; I2C/CSI-2 retain their meaning.
Zero rates, zero FIFO extents and SPI transfer size are not hardware admission.

Vendor bus types192..255 preserve arbitrary type revision/flags and all type data
as opaque vendor bytes after checking the common revision/flags/length/source
envelope. Reserved types0 and5..191 return Unsupported_Bus_Type. No vendor
semantics, compatibility quirk or bus backend is inferred.

The pure matrix covers all16-bit specific flags/type lengths, every I2C address
in both modes, SPI selection and UART FIFO values, all byte codes, rate bits,
source termination, truncated prefixes, nonzero offsets/high array bounds and
the maximum wire payload. The normal ACPICA fixture adds seven descriptors:
V1/V2 I2C, SPI and UART (including V2 sharing) plus CSI-2, with vendor bytes and
source instances. All earlier seventeen buffers are byte-identical. Actual Core
coverage evaluates/copies the resource Buffer, compares every byte, checks typed
settings/spans and releases the result; check final suite evidence for outcomes.

The original functionality batch preceded the accepted three-unit proof described
below. The six earlier proof registrations retain their precise source/scope;
the later proof covers this unit but not controller admission. Native bus
integration, GenericSerialBus OperationRegion
transactions and complete resource/AML correctness remain unfinished.

Sources: ACPI6.6 sections6.4.3.8.2 and19.6 V2 serial macros:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#genericserialbus-connection-descriptors
https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf
ACPICA generation was checked in the retained upstream aslrestype2s.c and
normal iASL20260408 output; no forced compilation or suppressed diagnostics.

Accepted hosted evidence: source serial-resource-replay-JM23BNqL/source,
tests/acpi-resource-data/results/release-mohp9te9 and checked-an4bpep8. Each
profile passes1,060,046 focused checks (795976serial/263543PCI/527actualCore),
three mains/six stages; independent audits verify176ALI/173strict commands and
all input/tool/runtime/binary/artifact hashes. All219 production/1051AdaGPR
inputs carry into publication unchanged. No all-thirteen or new proof claim.

The later pin-resource extension preserves all219 serial-milestone production
inputs. It expands the shared test fixture/registration, so the prior1051-file
Ada/GPR carry above describes the serial publication only; each newer suite
must cite its own exact inputs. No pin or serial proof follows from that carry.

The later three-unit connection proof includes this actual decoder:139checks/
234goals+15flow, all five entities SPARK/no assumptions/skips/unresolved. Scope:
runtime safety and declared extent/span contracts, initialization/dependencies/
termination; not full field equivalence, acceptance/error completeness or bus
admission. See tests/acpi-connections-proof/README.md for the accepted source.
