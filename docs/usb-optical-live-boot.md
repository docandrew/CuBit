# USB optical live boot

Status (2026-09-12): native USB CD application loading works in QEMU/KVM.
**Real IODD/laptop diagnostics reached xHCI, which rejected setup with
`scratchpad-limit`. After fixing the decoder, the laptop reports a genuine
34-buffer requirement. The 64-buffer image progressed to connected-port
discovery, then stalled. The newest image has dynamic scratchpad sizing,
corrected reset handling and finer checkpoints; laptop validation is pending.**
SameBoy remains deferred.

## Try the laptop image

`kernel/cubit_laptop_usb.img` is the new native-USB image. Copy it to the IODD
image collection and select CD-ROM emulation. Start with legacy firmware boot.
Keep the IODD connected: executables and DOOM WAD data are read on demand.
Removing/changing media invalidates storage until reboot.
Connect the IODD and USB mouse before boot; general USB hotplug/hubs are not
implemented by this slice.

The first GRUB entry tries 1920×1080, 1600×900, 1366×768, 1280×720, then 1024×768,
all 32-bit. The second explicitly selects 1024×768. This is firmware mode
selection, not Intel GPU modesetting or DPI support.
Use Super/Windows → arrows → Enter for Apps if mouse input is awkward.

```sh
nix develop -c make -C kernel usb-live-iso
nix develop -c make -C kernel run-usb-live
```

The original `kernel/cubit_laptop_live.iso` and `run-laptop` targets remain the
initrd-only fallback. A pre-work snapshot also exists at
`/tmp/cubit-live-fallback.NFpHTY/cubit_laptop_live.iso` (temporary storage).
No changes were committed or pushed by the agent.

### Diagnosing a black screen after GRUB

Select the third entry, **EARLY BOOT DIAGNOSTICS (legacy text mode)**, using
legacy/CSM firmware boot. This is a diagnostic entry, not a desktop mode.
It uses VGA text memory without allocating a graphical backbuffer. The normal
graphical entries retain their existing output behavior.

GRUB prints kernel/archive loading and handoff messages. The kernel briefly
shows `B0` (32-bit entry), `B1` (long mode, before Ada elaboration), and `B2`
(elaboration returned). Ada then clears those markers and prints `EARLY:`
checkpoints before serial, CPU-local state, secondary stack, interrupt tables,
memory-map normalization, allocators, and process initialization. Subsequent
kernel logging remains visible. Photograph the last visible message if boot
stops; if there is still no kernel output, report the last GRUB message too.

The diagnostic also prints the reserved boot arena's end address. One suspect
is the current conservative reservation through the highest module/metadata
address: firmware placing those high can exhaust the bootstrap allocator's
low-memory window. Fragmented firmware maps also need investigation. Neither
is established as the laptop failure's cause. The subsequent laptop diagnostic
reached xHCI initialization, beyond these early memory stages.

That run reported `PAGESIZE=00000001`, then `scratchpad-limit`. PAGESIZE bit 0
indicates support for 4 KiB pages and is accepted. The actual driver defect was
reversed significance in HCSPARAMS2: bits 31:27 hold the low five count bits,
while bits 25:21 hold the high five (xHCI section 5.3.4; cross-checked against
the [Linux register definitions](https://raw.githubusercontent.com/torvalds/linux/master/drivers/usb/host/xhci-caps.h)).
For example, one required scratchpad was decoded as 32, exceeding the original
16-page scratchpad reservation. The corrected pure decoder is tested over all
1,024 counts. The next laptop test reports `required=00000022` (34 decimal),
so it also genuinely exceeds that original capacity.

An intermediate image raised capacity to 64 in a 1 MiB allocation. The current
implementation instead supports the entire architectural range, 0..1023
scratchpads, with a startup-sized allocation. XHCI_DMA_Layout is shared with
devmgr, which reads HCSPARAMS2 through a bootstrap-only uncached BAR-page
mapping under its existing mapping authority, then allocates and authorizes
the required DMA region. The mapping remains private to devmgr. No new
application authority or general DMA allocation permission is introduced.

Fixed device rings and bulk staging precede the variable pointer table and
scratchpad buffers. The table uses zero, one, or two pages. Allocations round
up to the buddy allocator's next power of two: 34 buffers need 113 pages,
allocated as 128 pages (512 KiB); 128 need 207 pages, allocated as 256 pages;
1023 need 1103 pages, allocated as 2048 pages (8 MiB). No per-I/O allocation is
added. Four-KiB controller page support remains required.

Configuration word 1 carries BAR pages in its low 32 bits and the scratchpad
count in its high 32 bits. The authenticated driver bootstrap rejects counts
outside 0..1023, independently rereads HCSPARAMS2, and requires an exact match
before touching DMA or starting the controller. Allocation failure still
stops setup. Compile-time checks cover the fixed ring layout; hosted tests
cover page nonoverlap, table capacity and minimal allocation for every count.

New logs show HCSPARAMS2, decoded count and DMA pages (hexadecimal).
`boot image unavailable: config.svc` follows when xHCI fails: config.svc lives
on the inaccessible CD, not in the initrd.

### Connected-port stall

The next laptop snapshot was `PORTSC=00001211`: connected, powered,
SuperSpeed, reset asserted, not yet enabled. The previous enumeration code
could reassert reset while a reset was already in progress. It also echoed PED
when writing PORTSC, even though writing one to PED disables the port.
XHCI_Ports now waits for an existing reset, otherwise builds a reset write
from ordinary RW controls only. Tests include the exact laptop snapshot and
USB2/USB3 states. Definitions were checked against the
[xHCI PORTSC definitions](https://raw.githubusercontent.com/torvalds/linux/master/drivers/usb/host/xhci-port.h).

Additional boot-only logs identify the root port, pre-reset/ready PORTSC,
Enable Slot submission, and timeout status. Reset/completion waits retain
their bounded iteration counts. The old last message does not establish
whether the laptop stalled in reset, a command wait, or scheduling; this is
a correction plus instrumentation, not a confirmed end-to-end hardware fix.

### Firmware ownership and sleep/wakeup diagnostics

The laptop then remained at `waiting for existing port reset` for approximately
one minute, without the nominal 10,000-iteration timeout. Each iteration sleeps
1 ms, so the iteration bound does not guarantee a wall-clock timeout if the
process is not resumed. No kernel scheduler change was made from this evidence.

The newest image prints `pre-reset sleep(1ms) begin` and `pre-reset sleep resumed`
before changing controller state. Hardware waits log entry/return around their
first sleep and progress every 1,000 iterations. A final `wait entering sleep`
without `wait resumed` points at blocking/wakeup or system-wide interference;
continued progress with a fixed PORTSC points at a hardware transition problem.
This cannot diagnose a kernel or firmware lockup without further evidence.

The driver now also walks the bounded, BAR-validated extended-capability list
for USB Legacy Support, requests OS ownership and waits for BIOS ownership to
clear **before controller stop/reset**. After ownership, it disables legacy USB
SMI enables and acknowledges their status. Firmware refusal fails setup; we do
not forcibly clear the BIOS semaphore. If the optional capability is absent,
that is explicitly logged and setup continues. The handoff wait still depends
on a working scheduler, which is why the preceding sleep probe is separate.
Register definitions/sequence were checked against
[Linux's xHCI handoff](https://raw.githubusercontent.com/torvalds/linux/master/drivers/usb/host/pci-quirks.c)
and [legacy capability definitions](https://raw.githubusercontent.com/torvalds/linux/master/drivers/usb/host/xhci-ext-caps.h).

Stay in legacy boot for this diagnostic iteration. UEFI may change firmware
ownership behavior but is not evidence that a sleep/wakeup defect is fixed;
the VGA text diagnostic entry itself is legacy-only.

```sh
nix develop -c python3 tests/usb-optical/run-live.py --early-text --timeout 45
```

## Boot and authority boundaries

Firmware loads one kernel and one bootstrap CPIO. After handoff:

`xHCI → USB BOT/SCSI → Block.Device.V1 → ISO9660 → filesystem handles → apps`

The archive contains exactly seven files: devmgr.svc, filesystem.svc, ps2.drv,
xhci.drv, init.conf, system.conf, and the 8 MiB live-rw.ext2 workspace image.
All later services/drivers, Workbench, Devices, Files, DOOM and doom1.wad live
in the ISO /apps directory, absent from every boot-loaded module. The builder
audits primary-tree filenames and CPIO membership before publishing the ISO.
devmgr loads later driver images via FS with read-only bootstrap file policy
and an 8 MiB image limit. procmgr retains existing manifest admission for apps.

No ATA/NVMe driver is admitted by this profile: no internal disk is needed or
written. The writable RAM workspace is **lost on reboot**. This work supplies
no laptop NIC/Wi-Fi driver; do not expect browser networking.

Only filesystem.svc receives the optical endpoint (bootstrap slot 12). The
driver also authenticates the caller's kernel-registered role. Application ACL
checks precede file lookup; handles remain owner/generation-bound. ISO files
reject write/create/truncate. No UNIX mode bits are interpreted. Media identity
is not publisher identity: signature admission is separate, and this live
image retains the project's existing trusted-boot-media model.

## Transport and filesystem scope

- Eight independent slot DMA/context areas and EP0 rings; mouse and storage
  retain separate runtime rings. Per-slot/endpoint completion queues preserve
  unrelated events across control/command waits.
- One SCSI-transparent BOT interface, high/super speed. GET MAX LUN is checked;
  only an actual control STALL with acknowledged endpoint recovery permits
  LUN-0 fallback. INQUIRY selects an optical LUN, not simply LUN 0.
- Bounded readiness/inquiry/sense/capacity probes; 2048-byte logical blocks,
  READ CAPACITY(10)/READ(10). Capacity-16 sentinel explicitly unsupported.
- One deferred block read, up to 16 sectors/32 KiB. HID, service requests and
  IRQ notifications continue during bulk transfers. Success requires exact
  TRB identity, slot/endpoint, controller-reported byte count, CSW tag/residue/
  status and complete requested length.
- Two-second phase deadlines. Failed/short mounted reads and removal quarantine
  storage; no further submissions or private DMA reuse until reboot. HID remains
  live. A completion-router integrity fault instead requests controller halt.
- Existing generation/owner/range/direction-checked destination loans. Copy only
  validated complete transfers into the acquired grant, then return acquisition.
  No caller memory is directly exposed to USB DMA.

This is **not zero-copy**: controller-private DMA → filesystem block buffer →
caller grant. Staging remains until derived DMA loans/cancellation/lifetimes
are sound. These smoke tests establish no latency bound.

ISO_Records checks dual-endian geometry/extents, record bounds, flags and names.
The adapter caps descriptor scans, directory bytes (1 MiB), and depth (16);
file data reads batch up to 32 KiB. The builder preserves ASCII case/hyphens/
long filenames in the primary tree using xorriso options; no Rock Ridge
dependency. Lookup ignores ASCII case and strips ;1.

Unsupported: Joliet, multi-extent files, extended attributes, interleaving,
symlinks/relocation, multi-volume sets, and general ISO directory browsing in
Files. Unqualified file lookup tries immutable CPIO, ISO /apps, then writable
backends. Read/seek/close use the existing file protocol.
After a media/read/metadata failure, unqualified lookup fails closed instead
of falling back to a same-named writable file. Explicit @mem workspace paths
remain independent of the failed CD session.

## Bootstrap-memory defect exposed by the smaller image

GRUB module pages were not excluded from early/buddy allocation. Fixed
bootstrap stacks were also invisible in the ELF memory reservation. The smaller
archive failed with "Cpio: bad magic in initrd" despite valid on-disk contents.

The kernel ELF now reserves through the 16 MiB bootstrap stack top with NOBITS
(no file padding). Multiboot memory normalization retains the low boot arena
through the highest module/metadata end, excludes it from both allocators, and
keeps it mapped. This is conservative retention, not reclaimable boot memory.
The small-initrd native boot regression covers this layout.

Follow-up: growing kernel BSS has reached the low end of the nominal 128-CPU
bootstrap stack reservation. Tested one/four-CPU stacks at its high end do not
overlap, but fixed stack placement needs replacement before high-CPU-count
support is claimed. General Multiboot validation and boot allocator limits
also need separate hardening; the USB/ISO proof does not cover them.

## Verification

```sh
nix develop -c make -C kernel test-usb-optical prove-usb-optical
nix develop -c make -C kernel test-usb-hid
nix develop -c python3 tests/usb-optical/check-image.py kernel/cubit_laptop_usb.img
nix develop -c python3 tests/usb-optical/run-live.py --cpus 1
nix develop -c python3 tests/usb-optical/run-live.py --cpus 4 --disk-first --mouse-first --eject
```

Four pure packages (USB_Optical, USB_Configurations, XHCI_Completions, ISO_Records):
177 analysis checks discharged, zero justified/unproved. No pragma Assume or
SPARK-Off sections in these packages. This does **not** prove hardware, DMA/
MMIO, native adapters, the entire kernel, or end-to-end I/O. Hosted assertions
are enabled normally; native kernel assertions are not.

USB-only KVM runs exercise optical LUN 0 and LUN 1 (disk LUN 0), one/four CPUs,
reversed root-port ordering, desktop and app startup, mouse input, and captured
DOOM gameplay. Later runs load Workbench and Files too. Removal testing requires
fail-closed storage and healthy subsequent HID; DOOM can exit when missing WAD
data is needed. Artifacts: /tmp/cubit-usb-live.* and /tmp/cubit-usb-image.*.
run-hid.sh remains a separate mouse-only regression with an NVMe app fixture.

## Remaining hardware limitations

No external hub traversal, UAS, multiple BOT devices, arbitrary mass-storage
subclasses, full BOT reset recovery, or media reinsertion/remount. A combined
mouse+BOT device currently selects storage, not both interfaces. Separate
root-attached mouse and storage devices work. IODD/controller quirks need real
hardware observation. Malformed/short USB responses are host-tested, not all
fault-injected against the native controller.

References: [USB-IF BOT](https://www.usb.org/sites/default/files/usbmassbulk_10.pdf),
[ECMA-119](https://ecma-international.org/publications-and-standards/standards/ecma-119/),
[QEMU USB](https://www.qemu.org/docs/master/system/devices/usb.html).

## 2026-09-26: N95 live-media refresh

Current artifacts and test status are recorded in
[the USB live test README](../tests/usb-optical/README.md#n95-refresh-2026-09-26).
The BIOS image includes current Config/Turso, Workbench, Config Inspector and
Servo. Config explicitly uses volatile MemoryIO, never the internal disk.

The `usb-live-uefi-iso` companion now passes OVMF desktop boot and native app
tests using an explicit, validated Multiboot2 RSDP handoff. Shared boot-map,
module-lifetime and framebuffer admission remains in force. See
[Multiboot2 validation](../tests/multiboot2/README.md) for hosted proofs and
native test scope. Secure Boot must be disabled for the unsigned loader.
NUC hardware validation remains pending.

The `_usb.iso` suffix denotes USB **optical** boot, originally the IODD target.
It does not imply ordinary flash-drive support. A `dd`-written stick still
identifies as a direct-access disk, while the current driver admits CD/DVD
LUNs with 2048-byte blocks. TODO: read-only USB disk block adapter and explicit
ISO logical-sector translation, followed by a QEMU USB-stick boot regression.
Keep this separate from firmware discovery and do not fake an optical device
identity or broaden block-write authority. No host USB disk is modified by the
build or tests.

Servo's libc also needs a coordinated boot-volume abstraction: its current
`/fonts` and `/servo/pages` resolve to `@nvme:0`, unavailable on the USB-only
profile. The optical executable renders its built-in graphics but no page text.
