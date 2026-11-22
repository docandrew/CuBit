# Extended Address Space descriptions

Address_Spaces.Extended_Descriptors parses revision-one large-item 0x0B with
exactly 53 payload bytes. It checks the revision/reserved byte, general and
resource-specific flags, reserved resource types and I/O coverage. Unlike the
Word/DWord/QWord form, general bit 0 has an explicit consumer versus
produces-and-consumes meaning. There is no optional source-index/name tail.

All five numeric fields retain their full 64-bit values. The separate attributes
field uses its own named type and is reserved to zero for non-memory resources,
including vendor-defined types. Memory attributes remain raw UEFI claims without
ISA-specific validation or mapping. Memory cache bits are retained specifically
as Ignored_Cache_Bits; they do not represent a cache policy for this descriptor.
Vendor flags remain raw. PCC type 10 is reserved in this format, even though the
ordinary address form can describe it.

Geometry_Of copies the common fields into the existing QWord geometry policy.
The adapter does not call admission policy, apply translation or allocate a
range. A feasible geometry witness, resource role and attribute bits establish
no platform ownership, controller association, hardware access or cache mapping.

Pure tests cover all general/revision/reserved/type/specific bytes, vendor flags,
every bit of all six 64-bit fields, non-memory attribute rejection, length and
array bounds, full unsigned values and the geometry adapter. Normal ACPICA/Core
replay adds ExtendedMemory (including distinct cache bits/attributes), ExtendedIO
and vendor ExtendedSpace, with exact retained buffers. The scoped resource-layout
proof now discharges 66 checks/154 goals plus eleven flow checks for runtime
safety and the existing extent contract. It does not prove full field-value or
geometry-adapter equivalence. See tests/acpi-resource-layout-proof/README.md;
the two earlier scoped proof receipts apply to unchanged units.

Specification: ACPI 6.6 sections 6.4.3.5.4 and 6.4.3.5.5:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html
