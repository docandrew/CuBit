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
Existing default callers use mode 0. As of2026-09-28 this access-mode handling
and the existing admission predicate are integrated into the main kernel's
syscall path. The main path additionally rejects null/non-user virtual ranges
and preserves its newer owned-memory conflict checks and locks; it was not
replaced wholesale by the private workspace version.

The capability-table consumer has a separate hosted regression:

```sh
nix develop -c bash -c 'gprbuild -P tests/device-memory/access_tests.gpr && tests/device-memory/build-access/access_checks'
```

This builds the real capability operations with only the hardware-dependent
Config replaced by a64-slot fixture matching the kernel. It checks read-only
versus writable requests, all read/write-right combinations, range edges,
zero sizes and wrapping ranges. It does not execute page-table writes.

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
That historical generic image was a test artifact until rebuilt.

Merged-kernel evidence (2026-09-28): QEMU run `b_e2rf6m` uses current main
kernel SHA256 `7b5512b9b34ff316e18fe5b1b8d2c4549d6c1484e225ebd64b7c456ff794253f`
and explicit historical fixture inputs (private devmgr and map-check binaries),
with other services from current staging. The generated test-only catalog/profile
are `/tmp/cubit-map-check.fo3xtq/{artifacts,system}.ccl`; no normal image membership
was changed. Image assembly succeeded but its first copy into /tmp hit quota;
the completed staging image was copied to `kernel/cubit_map_protection_test.img`
and booted there. This is NOT a user-facing image.

`check-native.py` now checks the current structured fault record against the
fixture's loaded PID and exact decimal address, then requires process stop and
subsequent Desktop startup. It passes on
`kernel/build/tmp/nix-shell.94R2Lw/cubit-usb-live.b_e2rf6m/serial.log`:
PID25 denied writable and unknown-mode requests, read its RO alias, then faulted
at1358954496 (`0x51000000`) with `kind=write-protection`. The USB-flash/hub,
four-CPU UEFI/no-PS2 boot regression also passes. This verifies the current
kernel boundary; it does not make the historical fixture launcher a test of
the current production devmgr or establish GPU MMIO correctness.
