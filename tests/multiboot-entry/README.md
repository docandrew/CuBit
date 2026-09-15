# Initial Multiboot entry admission

Run all builds/proofs through the repository's Nix environment:

```sh
nix develop -c bash tests/multiboot-entry/prove.sh
nix develop -c bash tests/multiboot-entry/test.sh
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/multiboot-entry/check-codegen.py
nix develop -c bash tests/multiboot-entry/text-boot.sh
```

## Production change

The 32-bit entry point now validates the loader magic and the full 118-byte
header's numeric extent before its first diagnostic dereference. A null,
out-of-window or wrapping address stops before reading loader memory. Early
rejection emits `MB!` to QEMU's debugcon port and halts without guessing a video
mode. It does not promise visible output on real hardware with invalid input.

Ada receives a scalar physical address, not a by-reference firmware record.
`Multiboot.Read_Information` repeats admission before constructing the byte-array
overlay and calls the pure `Multiboot_Entry.Snapshot` routine. Only advertised
module fields, the required map fields, and supported framebuffer fields are
copied into a kernel-owned record. CuBit currently requires a memory map and a
direct-RGB or text framebuffer; missing/unsupported information fails explicitly.
An RGB framebuffer is no longer mistaken for text mode merely because its width
is 80. Subsequent [framebuffer admission](../boot-framebuffer/README.md) now
validates geometry and mapped spans before a renderer is installed.

Ignored fields and reserved flags are zero, including the RGB union tail in
text mode. The reserved-flags type also now covers its full 19-bit field rather
than a 16-bit range. The adapter copies 116 bytes into the native record; it
does not overlay that padded 120-byte native object on a 116-byte snapshot.
Assembly include dependencies are explicit in Make so changes to the shared
gate cannot silently leave a stale boot object.

The raw extent was increased from 116 to 118 bytes on 2026-09-14. GRUB aligns
the RGB union at byte112, unlike the byte110 position in the manual's diagram.
The adapter explicitly implements this GRUB ABI, normalizing raw112..117 into
internal110..115; it does not guess layouts from whether masks look plausible.
The pure snapshot and actual assembly gate tests use the extended raw extent.
Distinct poisoned padding/mask bytes test normalization, including the previously
unread blue mask. This matches [GRUB issue 63499](https://lists.nongnu.org/archive/html/bug-grub/2022-12/msg00010.html)
and the native boot fixtures. Another loader layout needs an explicit adapter.

## Evidence and scope

GNATprove discharges **11 checks**, none unproved or justified, for the pure entry
core. A successful address admission implies the correct magic and a nonnull,
fully contained header extent without overflowing subtraction. Snapshot failure
publishes all zeroes; success establishes the retained flag bits, supported
framebuffer type, exact RGB union normalization and zero text-mode RGB tail. Exact field copying/zeroing is
also checked byte-by-byte by the hosted tests.

The hosted target passes **169,416 address/header cases**: exhaustive small
windows, both ends of the production mapping, 64-bit overflow boundaries, all
relevant flag combinations and all 256 framebuffer tags. It assembles the exact
production gate macro into a callable x86-64 harness and compares its result to
the independent arithmetic oracle and SPARK admission routine. Inputs exercise
the 32-bit register operations used by the real 32-bit boot entry, but this is
not a proof of the complete assembly control flow or instruction decoder.

Release codegen has no external or runtime proof/assertion dependencies in the
pure entry object. Stack checks retain the kernel's 2 KiB per-function limit.
Hosted tests use runtime checks; the kernel retains `-O2 -gnatp`.

The entry implementation passed the four-vCPU KVM `capability-security` fixture,
including eight partial-load rollbacks and PID reuse. After the legacy-FADT
correction below, the final kernel passed `desktop-doom` with
`CUBIT_DOOM_MULTIAPP=1` on Q35 (game pixels and responsive Apps menu) and the
older-chipset early text-boot fixture. These are native regressions, not a proof
of the full machine path. Production admission/snapshot routines each report
8 bytes of stack and the raw adapter 272 bytes; existing allocator codegen
checks also pass.

The text fixture builds an isolated temporary ISO from the current kernel, with
no modules and no writable disk, and checks text diagnostics, completed memory
initialization and legacy-chipset ACPI discovery. It does not require completion
of subsequent PCI discovery or module startup. It retains its ISO and logs under the
printed `/tmp/cubit-entry-text.*` directory, without changing shared image staging.

That fixture exposed an existing FADT overread: the old chipset supplies a
shorter legacy table, but the kernel unconditionally read its nonexistent
extended DSDT field and followed unrelated bytes as an address. The adapter now
checks the legacy prefix length, reads X_DSDT only when its complete field is
present, and selects the legacy address when the extension is absent or zero.
Field-end offsets derive from the record layout. Null/out-of-physical-range
DSDT header extents fail explicitly. This is an integration fix, **not part of
the entry SPARK proof** or a complete ACPI table admission implementation.
The field layout and precedence follow the [ACPI FADT specification](https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html#fixed-acpi-description-table-fadt).

Still outside the proof: physical RAM backing/source stability, the machine
entry state and assembly-to-Ada ABI, byte-to-record representation, complete
framebuffer validation, module payload/metadata lifetime and overlaps, firmware
truthfulness, DMA, mapping/cache attributes and overall allocator refinement.
Admission cannot establish that a plausible physical address contains RAM.
The subsequent [boot-module boundary](../boot-modules/README.md) now replaces
late descriptor/name reads with a sealed catalog and explicitly retains payloads.
