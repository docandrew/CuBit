# Device-memory admission (Linux-hosted)

Run from the repository root:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/admission.gpr && ../tests/device-memory/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/device-memory/admission.gpr -u device_memory_admission.adb --level=1 --checks-as-errors=on --report=all -j2'
```

Tests compare bounded ranges against an independent nonwrapping arithmetic
oracle, cover top-of-64-bit-space wraparound, and exhaust read/write rights.
The pure predicate's postcondition proves nonempty, nonwrapping containment.
Assertions are enabled only in this hosted test executable.

The private boot-debug-ts3jrkxp kernel uses this helper in capability admission
and adds MAP_DEVICE arg3 access mode (0 read/write, 1 read-only). It rejects
unknown modes, unaligned pages and wrapping virtual/physical requests. The
read-only path omits PG_WRITABLE; it does not relax range or ownership checks.
Existing default callers use mode 0. This is not yet integrated into the main
kernel's syscall path pending shared-file coordination.

These tests/proofs do **not** establish hardware page-table enforcement, TLB
behavior, capability lifetime/revocation atomicity, or transactional rollback
on partial mapping failure. Native read-success/write-fault enforcement is
tested separately below.

Native evidence (2026-09-27): private QEMU UEFI/4CPU/USB-flash fixture
`4n7sahgp` passes `check-native.py`: READ-only capability rejects writable
and unknown-mode requests, the RO mapping can be read, and its subsequent
write causes a user protection fault at 0x51000000. Desktop subsequently starts
and the USB hub keyboard/mouse regression passes. This verifies this native
path, not every mapping/lifetime case or real GPU register semantics.

The private `userspace/apps/map-check` fixture is launched only if its file is
in bootstrap. It allocates a dedicated RAM page; no hardware BAR is touched.
It deliberately faults and must not be included in user-facing images. The
normal private image catalog and membership audit were restored after testing.
The existing generic image on disk is still a test artifact until rebuilt.
