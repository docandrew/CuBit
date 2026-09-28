# N95 USB flash boot investigation (2026-09-27)

The v14 image reproduces the physical N95 capsule when QEMU presents its
bytes as a USB BOT direct-access disk with 512-byte sectors. The earlier
tests presented the same image as a CD-ROM with 2048-byte sectors.

The old xHCI SCSI decoder admitted only CD/DVD peripheral type 5. The flash
drive was rejected; devmgr could not load `procmgr.svc` from the ISO. Its
unconditional `startup complete` message hid the failed launch. That message
does **not** prove a ready handshake occurred. Earlier conclusions about
an IPC-return failure, scheduler affinity, or dropped logs were unsupported.

Private fix in `.build-workspaces/boot-debug-ts3jrkxp`:

- Admit connected direct-access disks and CD/DVD devices, with 512- or
  2048-byte sectors. Check ISO volume-descriptor signature before selection;
  existing filesystem code still validates the full PVD and extents.
- Preserve the existing read-only 2048-byte ISO block interface. Encode a
  flash READ(10) with four native sectors per ISO block, retaining the same
  DMA buffer and 32 KiB maximum transfer. No additional data copy or writes.
- Return capacity in complete ISO blocks, enforce address bounds, and reject
  unsupported sizes and capacities requiring READ CAPACITY(16).
- Report failed procmgr launch/readiness instead of successful startup.
- Add `run-live.py --usb-flash` to exercise actual dd-style image storage.

Reproduction: private `tmp/cubit-usb-live.5cqf4h3s/serial.log` records medium
rejection, procmgr image unavailable, and the misleading completion line.
Fixed flash regression: `tmp/cubit-usb-live.i6ea7sje/` reaches the desktop,
DOOM, Workbench and Files; Workbench screenshot inspected. This is QEMU
evidence; physical N95 confirmation remains required.

The UEFI multi-LUN test `pl8gr84x` fails in OVMF loading the LUN-1 boot device,
before GRUB/CuBit. It is not evidence of a CuBit storage failure.

Ordinary UEFI optical regression `y4_uuixx` passes desktop and applications.
Hosted USB/ISO suites pass. GNATprove level 2 proves all 31 checks in
`usb_optical` (17 subprograms/packages analyzed), zero assumptions. This
covers the pure codec and capacity contracts, not xHCI hardware or DMA.

Candidate: `kernel/cubit_n95_usb_flash_v15.img` inside the private workspace.
SHA-256: `476af6642a4975199c74b742207a32099ba724c5490caaba3f463984ca0030e1`.
Required backport: `usb_optical.ad?`, `xhci.adb`, USB test `main.adb` and
`run-live.py`; the narrow required-procmgr failure branch in devmgr, and
the `BOOT FAILURE:` retained prefix in boot diagnostics. Preserve concurrent
networking changes to devmgr. The shared build lock was occupied at backport
time, so no shared implementation files were edited.

Implementation remains private pending coordination/backport. No scheduler
or affinity changes are part of the storage fix. The networking agent's
idle/wakeup latency findings remain a separate follow-up.
