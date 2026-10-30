# Copied DMA descriptions

DMA_Descriptors parses small-item DMA and FixedDMA resources into named channel,
request, speed and width types. It re-reads the frame and checks exact lengths.
These are descriptions, not DMA-controller capabilities or programmed transfers.

Legacy DMA retains the eight-bit channel mask and raw information byte. The
speed and transfer preference are typed; the bus-master bit is a firmware claim.
Information bits 4:3 are ignored and preserved, while reserved bit 7 and reserved
transfer preference 3 report explicit errors. Zero and multiple channel masks
remain representable; controller and allocation policy are separate.

FixedDMA retains the full 16-bit request-line and channel numbers and supports
the six defined widths, 8 through 256 bits. Other width codes fail. It is accepted
only for Current_Resources (_CRS), with distinct Wrong_Use results for _PRS and
_SRS; dynamic arbitration is not supported. Request lines are platform-relative
and channels controller-relative. Successful parsing establishes no controller
association, memory ownership, allocation or hardware access authority.

Pure tests cover every mask/information-byte pair, every request-line value and
every channel value, all width codes, use context, exact lengths, prefixes and
high array lower bounds. The normal ASL reference adds compatibility DMA,
Type F DMA and FixedDMA to one _CRS. Actual Core tests compare retained bytes,
decode all fields, reject non-_CRS FixedDMA use and release handles.

The scoped resource-layout proof now discharges 39 checks/66 goals plus four
flow checks for this child: runtime safety and the existing decoder extent
contract. Full field-value equivalence and controller policy remain outside that
proof. See tests/acpi-resource-layout-proof/README.md. The earlier two scoped
proof receipts remain on unchanged sources.

Specification: ACPI 6.6 sections 6.4.2.2 and 6.4.2.7:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html
