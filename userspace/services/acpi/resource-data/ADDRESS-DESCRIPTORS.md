# Copied address-space descriptors

ACPI_Resource_Data.Address_Spaces parses Word, DWord and QWord address-space
items. Every numeric field is retained in a named unsigned 64-bit type with
its encoded width: granularity, minimum, maximum, translation and length.
No arithmetic translation, mapping or register admission occurs.

Decoded reports structural extraction and the implemented flag checks. It does
not establish valid address geometry: ordering, granularity shape, alignment,
fixed/variable-length combinations, bridge restrictions, overflow and platform
ownership require separate validation before use. In particular, all-ones values
and geometrically inconsistent descriptions remain representable for diagnosis.

General bit 0 is preserved as ignored, not exposed as a consumer/producer claim.
Memory and I/O flags have typed firmware claims and reserved-bit checks. Bus
flags must be zero. Reserved resource types and reserved I/O coverage fail.
PCC and vendor-defined types retain common numeric fields and raw specific flags;
no type-specific interpretation is claimed for them. Memory cache labels are
encoded claims, not cache configuration or mapping policy. The sparse I/O bit
is retained even when its controlling translation bit is clear.

Source index and name are present together. Names are opaque byte extents into
the exact caller copy, never resolved paths. The first NUL must end the item;
index-only tails and trailing bytes are rejected. Empty strings are retained.
Word/DWord indices retain descriptor-index semantics. Require_Zero affects only
QWord indices and must come from independent platform negotiation.

See ACPI 6.6 sections 6.4.3.5.1–6.4.3.5.5:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html
This child's milestone proof discharged 91 runtime-safety checks/156 goals and
11 flow checks. Full field-value equivalence and address geometry remain open;
see tests/acpi-descriptor-proof/README.md for the four-unit proof scope.

Common field combinations, alignment and existence of a nonempty witness can now
be checked separately with Address_Spaces.Geometry. Parsing still does not imply
that check succeeded. See ADDRESS-GEOMETRY.md for its exact scope; bridge,
translation and platform ownership decisions remain separate.
