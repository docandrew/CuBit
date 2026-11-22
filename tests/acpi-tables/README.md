# Shared ACPI table admission

Linux-hosted tests of `shared/firmware/firmware_tables.*`, intended for reuse by
boot admission and userspace ACPI. The native BIOS/UEFI adapters now call this
core; this test target itself exercises only the pure parser, not physical
mapping or IOMMU enforcement. Native UEFI coverage is in
[the Multiboot2 tests](../multiboot2/README.md).

Run in the Nix environment:

```sh
nix develop -c bash tests/acpi-tables/run.sh --prove
```

The host executable enables assertions and arithmetic checks. These flags are
local to the hosted project; no kernel build flags are changed. The shared
units contain no `Assume`, address overlays or `SPARK_Mode => Off` sections.

Admission covers:

- RSDP signatures and separate legacy/extended checksums; revision 0 and 2+
  formats; reserved revision 1 rejection; complete declared extents; XSDT
  preference with RSDT fallback only when the XSDT address is zero.
- Standard SDT header signature, declared extent and whole-table checksum.
- Typed failure results with no address/extent payload; no allocation or
  physical pointer dereferencing. Address values remain untrusted numbers.

The 26,674 regression checks cover both RSDP forms, extended revisions and
lengths, every prefix truncation, every nonzero single-byte mutation of valid
extended RSDP and SDT fixtures, malformed lengths, absent roots, non-1 array
bounds (including the highest legal index), and trailing-buffer handling.
The DMAR fixture tests only its generic SDT header, not DMAR body semantics.

SPARK result (2026-09-26): 57 obligations discharged, none unproved or justified
(39 runtime checks, 8 functional contract checks, 6 initialization and 4
termination checks). The contracts include accepted extents lying within the
input; nonzero accepted root addresses are represented by a constrained type.
This is not a proof of checksum authenticity, full ACPI semantics or native
physical-memory access safety.

Reference: [ACPI 6.6, section 5.2](https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html).
Checksums detect corruption, not authenticity. A valid table can still describe
malicious or nonexistent hardware. Future adapters must bound resource usage,
establish readable physical backing before copying, preserve firmware-memory
lifetimes and prevent changes between validation and consumption. Those are
not assumptions discharged by this parser's SPARK proof.
