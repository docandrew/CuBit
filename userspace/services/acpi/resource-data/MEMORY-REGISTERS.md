# Legacy memory ranges and generic registers

Memory_Ranges decodes exact-length Memory24 and Memory32 copied items. Minimum
and maximum describe allowed base addresses, not an address-window endpoint.
Memory24 bases and lengths are expanded from 256-byte units; alignment is in
bytes, with encoded zero represented as 65,536. Both encoded and interpreted
alignment are retained. Memory32 keeps all 32-bit fields and its alignment value
verbatim, including zero. Information bits 7:1 are ignored and preserved; the
write bit is a firmware claim. No range ordering, allocation, alignment feasibility
or cross-item mixing policy is established. In particular, these maxima must not
be passed directly to Address_Spaces.Geometry as end-of-window bounds.

Generic_Registers decodes the 12-byte payload into the existing pure
ACPI_FADT.Generic_Address record. It retains space, width, bit offset, access
size and the full unsigned 64-bit address. Access codes 0..4 are defined and
retained; other access-size codes report a separate error. Space IDs are raw
metadata, including unsupported/reserved values; they do not select a backend.
Zero address, width and access size are preserved, not converted to absence.
Register-specific geometry, access-width selection, backend support and ownership
remain separate policy. No decoded GAS becomes an admitted register ID or access
capability. Existing transaction planning requires independently selected bounds.

Both decoders re-read the parent frame, reject other item families and check
exact lengths. They perform no memory reads or writes at described addresses.
Pure tests cover all 65,536 encodings of each Memory24 field, Memory32 field bit
positions, all information bytes, every GAS byte field/access code, address bits,
malformed lengths and high array bounds. Actual Core tests evaluate seven normal
iASL descriptors in three new copied buffers and release retained handles.

The normal reference keeps Memory24 and Memory32 in separate devices. A first
ASL fixture used byte values where Memory24 takes encoded arguments, and a
Memory32 window rejected by iASL; its compiler errors remain retained. The final
reference compiles normally without force or suppression. Item decoders do not
enforce the table's prohibition on mixing the two memory families in one device.
These children are outside the prior four-unit proof; see the separate milestone below.

Specification: ACPI 6.6 sections 6.4.3.1, 6.4.3.3 and 6.4.3.7:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html

A separate subsequent proof milestone discharged memory 30 checks/56 goals/six
flow checks and generic-register 28 checks/109 goals/three flow checks. This
establishes selected safety and extent contracts, not full field-value semantics
or platform policy; see tests/acpi-resource-policy-proof/README.md.
