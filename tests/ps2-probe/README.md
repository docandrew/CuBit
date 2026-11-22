# Optional PS/2 controller boot probe

2026-09-26: hosted fixtures, native no-i8042 desktop/USB mouse, and ordinary
PS/2 desktop/DOOM/Workbench/Files tests pass. The previous SMP MAP 4 image
reproduces the stall at `devmgr: PS/2 driver started` with i8042 removed.
Physical N95 confirmation of this fix remains pending.

`ps2.drv` previously drained controller output until status bit 0 cleared, with
no limit. An absent i8042 returning `FF` therefore never completed startup.
Devmgr waits for that driver's readiness before starting xHCI: an optional legacy
input device could block USB storage and the desktop.

The shared native/hosted probe now reads at most 257 status bytes and discards
at most 256 stale data bytes. `FF` returns unavailable without reading data;
output that never quiesces returns a drain-limit failure. The driver reports
the existing `OP_NOT_PRESENT` startup outcome and exits for either failure.
Devmgr then continues to xHCI. A quiescent controller retains the existing
keyboard/mouse initialization and input decoder; there is no PS/2 ABI change.

Hosted regression (Nix, from `kernel/`):

```sh
alr exec -- gprbuild -p -P ../tests/ps2-probe/probe.gpr
../tests/ps2-probe/build/probe_tests
```

Covers absent controller, every drain length 0..256, capacity exceeded by one,
permanently full output, and disappearance while draining. These are tests of
the actual probe used by the driver, not a separate reimplementation. Hardware
port calls remain a trusted boundary; this is not a SPARK proof of the driver.

Native regressions (shared build lock or isolated workspace):

```sh
python3 tests/usb-optical/run-live.py --uefi --cpus 4 --without-ps2 --pit-free-fixture
python3 tests/usb-optical/run-live.py --uefi --cpus 4
```

The first removes QEMU's actual i8042 device and checks that PS/2 absence does
not block the desktop, then checks USB mouse motion and buttons. It deliberately
does not claim keyboard-driven app testing: the current xHCI driver rejects
QEMU's boot keyboard, although it supports boot mice. The second retains PS/2
and performs the normal desktop/DOOM/Workbench/Files launch checks.

Follow-ups, separate from this bounded-drain fix:

- Pass validated ACPI FADT i8042-presence metadata to device discovery rather
  than always spawning a legacy driver. No external sockets does not itself
  prove an internal controller is absent.
- Propagate PS/2 command/ACK timeouts through mouse initialization; an empty
  output buffer alone is not proof of a functioning keyboard or mouse.
- Add USB boot-keyboard support and its restricted keyboard-publication authority.
- Harden devmgr readiness waits (expected sender and timeout/lifetime handling).
