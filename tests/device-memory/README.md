# Device-memory admission (Linux-hosted)

Run from the repository root:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/admission.gpr && ../tests/device-memory/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/device-memory/admission.gpr -u device_memory_admission.ads --level=1 --checks-as-errors=on --report=all -j2'
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

This builds the real device-memory authorization (Device_Memory_Authority,
over a sealed classification that describes nothing, so every page is bus
space) with only the hardware-dependent Config replaced by a 64-slot fixture
matching the kernel. It checks read-only versus writable requests, all
read/write-right combinations, range edges, zero sizes and wrapping ranges.
It does not execute page-table writes.

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

## CAP-001: RAM is never device memory (2026-10-11)

`Physical_Ranges` classifies physical memory from the firmware map (by
kind), boot modules, the boot framebuffer, low memory, the kernel image and
kernel MMIO (interrupt window, every I/O APIC, HPET). A page takes the most
restrictive class of any span touching it; undescribed pages are bus space.
`Device_Memory_Authority.Authorize` (MAP_DEVICE, MAP_INTO device mappings,
MAPFB) and `Mint_Admissible` (POLICY_MINT of CAP_DEVICE_MEM) accept only
ranges whose every page is device-mappable. devmgr's boot root is one
CAP_DEVICE_MEM per maximal device run (slots 48..55, largest first).

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/authority.gpr && ../tests/device-memory/build-authority/authority_tests'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/device-memory/authority.gpr -u physical_ranges.adb device_memory_authority.adb capabilities.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
nix develop -c python3 tests/device-memory/mutations.py   # add --no-prove for tests only
```

Proved (level 2, 0 unproved): the classification lookups (`Class_Of` is the
minimum class of touching spans), `Lemma_Device_Range_Excludes_RAM` and
`Lemma_Authorized_Mapping_Excludes_RAM` (every page of an authorized mapping
has a device-mappable class), `Lemma_Minted_Range_Excludes_RAM`, that device
runs are device ranges, and `Lemma_Attenuation_Does_Not_Amplify` (an
attenuated device capability grants nothing its parent does not: range and
rights subset). Hosted tests: a q35/UEFI-like map with hostile ranges (one
page into RAM, sub-page RAM sharing a page, wrapping, zero, huge, at the
physical limit, exactly at boundaries), map states (unsealed, overflow,
invalidated), runs (maximal, largest first, PCI hole and above-RAM present)
and an independent page-by-page oracle over 4000 generated maps.
`mutations.py` applies 16 mutants; the tests kill all but one behaviourally
equivalent mutant, which the proof kills.

Assumptions, not proved: the allocators take frames only from firmware
"available" ranges (MemoryAreas.Allocation_Map) and the kernel image lies in
[Config.MIN_PHYS_ALLOC, V2P (STACK_TOP)); the syscall glue that calls
`Authorize` is SPARK_Mode Off (reviewed, headless-tested).

## CAP-001 diagnostics: refusals made visible (2026-10-11)

Every refused device-memory request (MAP_DEVICE, MAP_INTO, MAPFB,
POLICY_MINT of CAP_DEVICE_MEM) becomes a kernel record
(`Device_Memory_Diagnostics`, layout `shared/publications/
device_memory_publication.ads`): caller identity and name, request, base,
bytes, access, and why (`Device_Memory_Authority.Explain`/`Explain_Mint`:
invalid range, classification unavailable, first offending page and its
class, no covering capability, not the boot image). The ring and the boot
classification live in one page mapped read-only into the registered
logstore only; logstore turns them into "kernel: device-memory denied"
Warnings and "kernel: physical map" records. The Desktop bootstrap screen
shows the latest Warning records and the map (Desktop_Bootstrap_Logs).

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/diagnostics.gpr && ../tests/device-memory/build-diagnostics/diagnostics_tests'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/device-memory/diagnostics.gpr -u desktop_bootstrap_notes.adb intel_gpu_map_report.adb --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
nix develop -c bash tests/headless/run.sh --test devmem-denials --accel kvm --timeout 60
```

Proved: each explanation states what it claims (`Explained`; the offending
page lies in the request and has a non-mappable class equal to its
`Class_Of`), `First_Blocked_Page`, the reader's ring arithmetic
(`First_Available`), the bootstrap note keeper and intel-gpu's report text.
Hosted tests cover the explanations, the reader arithmetic and the notes.
Not proved: the kernel/logstore/Desktop glue (overlays, locks, IPC).

Bootstrap timing (2026-10-11, after the first NUC photo was drawn 10 ms
after Desktop started): the kernel also counts refusals in the System facts
page (Device_Memory_Denials, a count only), and logstore numbers each
record ("kernel: device-memory denied #N"). Before its bootstrap frame,
Desktop gathers log records until the highest #N it has seen reaches the
kernel's count and at least 250 ms have passed, or 1 s
(Desktop_Bootstrap_Logs.Gather_Before_Bootstrap), and the frame says
"Logs as of t=... ms; kernel device-memory denials: N (all in the log)".

## Process kills (2026-10-11, NUC startfix triage)

When the kernel kills a user process for a fault (protection, no-execute,
not present, out of memory, other CPU exception) it writes a kill record
to the same page (layout 2: `Kill_Total`, `Kills`, 8 slots, same
Writing/sequence protocol) and counts it in System facts
(`Process_Kills`). Logstore logs each as a Warning: `kernel: process
killed #N name=<process> pid=<identity> kind=<kind> vector=<n> addr=0x...
rip=0x... t=<ms since boot>ms`. devmgr logs its children's exits by name
(it now takes their exit reports, which also frees their PIDs), procmgr the
exits of what it launched. The Desktop bootstrap screen shows the kill
count and the latest kill lines and waits (with the denials, 1 s ceiling)
until both counts have reached the log. Headless: `process-kill`
(fault-check writes an unmapped address).
