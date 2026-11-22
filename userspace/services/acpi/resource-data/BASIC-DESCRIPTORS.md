# Basic copied resource descriptors

ACPI_Resource_Data.Basic_Descriptors decodes legacy IRQ, programmable I/O-port,
fixed I/O-port and fixed 32-bit memory descriptors. It re-reads the frame at the
requested offset, checks exact descriptor lengths, and returns tight named types
with no borrowed storage or hardware capability.

IRQ descriptors without information default to active-high edge-triggered,
exclusive, non-wake descriptions. Information bits 2:1 are ignored. Reserved
bits 7:6 and invalid legacy PIC mode/polarity pairs report explicit errors.
Programmable I/O information accepts only its defined decode bit. Memory
information ignores bits 7:1 while retaining the raw byte for diagnostics.

Decoded means fields were parsed, not resources admitted. Port-range ordering,
alignment, zero lengths, endpoint overflow and platform feasibility still need
resource policy. FixedIO preserves the full encoded base and reports the
10-bit-descriptor family without normalizing unused high bits. Memory's
Declared_Writeable is a firmware claim, never write permission. Unknown resource
families stay explicitly unsupported; grouping/dependent-function policy is not
implemented by these item decoders.

Hosted checks cover all 65,536 IRQ masks, all information-byte values, malformed
lengths, raw unsigned values, ignored bits and high array bounds. Normal iASL
examples also exercise typed fields through actual Core-owned _CRS buffers.
The resource descriptor milestone additionally discharged 69 runtime-safety
checks/112 goals and five flow checks for this child. It does not establish
complete field-value equivalence or platform admission. The four-unit recipe
and precise scope are in tests/acpi-descriptor-proof/README.md.
