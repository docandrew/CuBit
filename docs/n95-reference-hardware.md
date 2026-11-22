# GMKtec N95 reference hardware

User-supplied Fedora live-session photographs, 2026-09-27. These are observed
identities, not universal N95/N100 device addresses or hard-coded policy.

## Display

- PCI 00:02.0, Intel 8086:46d2, Alder Lake-N UHD Graphics.
- Subsystem 0301:02f3; Linux driver i915 (also lists xe as an available module).
- BAR0: 64-bit non-prefetchable memory, 16 MiB; Linux base 0x6000000000.
- BAR2: 64-bit prefetchable memory, 256 MiB; Linux base 0x4000000000.
- BAR4: I/O ports 0x5000, 64 bytes; expansion ROM disabled.
- PCI capabilities were not readable in the non-root capture. Revision,
  connector details, EDID and current Intel register snapshot still needed.

Addresses/sizes describe the Linux session. Do not copy them into CuBit mapping
or authority grants. The 256 MiB aperture is not dedicated VRAM. Current CuBit
desktop uses firmware scanout; see [Intel plan](intel-gpu-bringup.md).

## USB input topology

Controller 00:14.0, Intel 8086:54ed, Linux xhci_hcd.

```text
USB2 root, port 1
  VIA VL812 hub 2109:2812, four ports, high speed (480 Mb/s)
    port 1: Microsoft Classic IntelliMouse 045e:0823
            full speed (12 Mb/s), HID interfaces 0 and 1
    port 4: Metadot Das Keyboard 4 24f0:0140
            full speed (12 Mb/s), HID interfaces 0 and 1
USB3 root
  port 1: VIA VL812 hub 2109:0812, four ports, 5 Gb/s
  port 2: Silicon Motion flash 090c:1000, 5 Gb/s, directly attached
```

Other root-attached USB2 devices: 0573:1573 USB audio/HID at port 6;
0bda:c821 Bluetooth at port 8. Do not claim these as mouse/keyboard interfaces.
Linux's bus/device numbers are transient, not CuBit identifiers.

The topology does not reveal HID report descriptors, which interface implements
boot protocol, hub single/multi-TT mode or endpoint intervals. Read actual
descriptors; do not infer those properties from these product names.

## Input implementation sequence

1. Completed pure descriptor discovery: independently retain keyboard, mouse,
   storage and USB2 hub candidates with their own interface/endpoint identity.
   Ignore unrelated HID interfaces and reject duplicated active endpoints.
   Fixture descriptors are synthetic, NOT captures of these physical devices.
2. Read/validate hub descriptor and port count, power/reset downstream ports
   with bounded asynchronous waits, and consume hub change notifications.
3. Track root port, parent slot, route string, depth and translation context
   per device; configure xHCI hub context and TT routing for full-speed children
   behind a high-speed hub. Reject unsupported depth/capacity explicitly.
4. Separate per-interface HID transfer/report state. Negotiate boot protocol
   only on eligible interfaces; decode keyboard press/release/modifiers into
   native input events, including rollover/disconnect resynchronization.
5. QEMU hub keyboard/mouse tests plus direct-device regressions, then physical
   tests with this exact wiring. QEMU success is not a hardware/TT timing proof.

Do not regress direct USB flash boot while adding hubs. Avoid polling-based
steady-state input delivery; use the existing completion/notification path.

### Private native integration checkpoint (2026-09-27)

In `boot-debug-ts3jrkxp`, root reset is separated from route-based device
enumeration. A USB2 hub slot receives the hub flag, port count and high-speed
TT think time. Each ready child is addressed before resetting its sibling;
per-call enumeration state is separate and the parent's control slot is restored
after each child. The existing interrupt-driven mouse path is reused unchanged.
QEMU's four-port hub fixture now delivers mouse motion/buttons to the desktop
and identifies the keyboard on port 4 (`cubit-usb-live.83r4t9ei`).

This is native CuBit regression evidence, not a SPARK proof of DMA/controller
behavior. It is not yet the shared driver's default implementation. Hub
hotplug/change notifications, slot reclamation and multi-TT alternate
settings remain work; protocol 2 is rejected explicitly. QEMU's full-speed hub
does not exercise the physical VIA high-speed translator. Endpoint-0 packet-size
negotiation is now implemented privately; the resize branch still needs a
hardware or emulator fixture with a non-default full-speed packet size.

The private keyboard queue now feeds USB_Keyboards and bridges usage transitions
to the existing desktop set-1 boundary. QEMU with i8042 disabled passes mouse
motion/clicks plus a, Shift+B, Left, Right and Ctrl+A (2eh2926n): 18 keyboard
bytes received across two statistics intervals, no gaps/drops/resyncs. This
checks delivery, not rendered editor text or every key mapping. Keypad/media,
PrintScreen/Pause, repeat, hotplug recovery and full-state recovery after IPC
loss remain incomplete. A composite device with both boot mouse and keyboard
still selects only one interface; the NUC's separate devices avoid that case.

## Intel read-only inspection image v20 (2026-09-27)

Private artifact:
`.build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_intel_snapshot_v20.img`

SHA256: `dba337806475f19347815f9bce69167949252c18cf93b0c94c0e122c2b41e8ef`.
Built from the isolated bring-up snapshot and its staged dependencies, not a
fresh build of every service in the evolving shared checkout.

Normal image audit: 8 bootstrap members, 21 optical payload files; no map-check
RAM/protection fixture. UEFI / four CPUs / USB flash / hub / no PS/2 QEMU input
test `oayv6642` passed. Separate automatic log viewer test `_y59qdzs` passed on
the same image. QEMU does not exercise Intel hardware; the earlier RAM-backed
fixture separately checked the Intel binary's snapshot/logstore path.

On the physical NUC, look in the rotating startup log for:
`intel-gpu: read-only snapshot; firmware=XXXXXXXX driver=XXXXXXXX; firmware scanout retained`.
Photograph that line and confirm desktop/keyboard/mouse remain responsive. The
registers are observations only: no GPU writes, modesetting or 3D acceleration.
If the line never appears, report its absence; do not infer successful inspection.
Native launch is gated on known ADLN identity, admitted BAR and discovered D0.
Failures before publisher setup can still be serial-only and need follow-up.

## Other devices observed (inventory)

- Ethernet: Realtek 10ec:8168 at 02:00.0, revision 15, Linux r8169.
- Wi-Fi: Realtek 10ec:c821 at 01:00.0, Linux rtw88_8821ce.
- HDA: Intel 8086:54c8 at 00:1f.3, Linux snd_hda_intel.
- SATA AHCI: Intel 8086:54d3 at 00:17.0.

These inventory entries do not imply CuBit driver support.
