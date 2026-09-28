# Multiboot2 UEFI handoff

`Multiboot2_Info` is a pure SPARK decoder of the copied GRUB boot-information
block. `Multiboot` is the native raw-address adapter. BIOS Multiboot1 remains a
separate supported transport, not an alias interpreting v2 bytes as v1 records.
Both join the existing module catalog, reserved-memory and framebuffer admission.

```sh
nix develop -c bash tests/multiboot2/run.sh --prove
```

Hosted evidence: 130,551 checks, including all single-byte mutations of a
complete fixture, every prefix truncation, duplicate mandatory tags, bad sizes,
unbounded/invalid memory ranges, extended memory records, unsupported versions,
missing end/map/framebuffer, malformed ACPI, active EFI boot services and
publication failure. All 164 SPARK obligations discharged (109 runtime checks,
20 functional contract checks, 12 loop assertions, 18 initialization and 5
termination). These are decoder guarantees, not proof of raw physical mappings,
firmware truth or every downstream ACPI consumer.

The snapshot is limited/by-reference. Failed parsing clears published counts
and snapshot fields. No pointers into the loader's metadata escape. The native
adapter additionally checks alignment, bootstrap mapping limits, overlap with
kernel storage, metadata RAM backing, and module/metadata overlap before the
allocator runs. Boot metadata has an explicit 1 MiB budget; module/name/map
capacities use the existing boot admission budgets. EFI boot services must have
been terminated by GRUB; CuBit does not call firmware runtime services here.
The native decoder's measured primary-stack frame is 272 bytes, static; neither
the output map nor module catalog is materialized as a variable stack temporary.

The kernel has both Multiboot headers. The USB optical GRUB configuration chooses
`multiboot2`/`module2` on EFI and retains `multiboot`/`module` on BIOS. The UEFI
image has an unsigned GRUB loader, so Secure Boot must be disabled. Ordinary
USB flash-drive media are a separate storage-adapter task, not covered by the
USB optical tests.

Native validation uses the shared build lock, Nix, and QEMU OVMF. Results and
current limitations are recorded in `tests/usb-optical/README.md` and coordination.
No host USB device or installed disk is written by these tests.

Reference: [GNU Multiboot2 boot-information format](https://www.gnu.org/software/grub/manual/multiboot2/html_node/Boot-information-format.html).
