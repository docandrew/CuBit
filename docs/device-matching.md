# Device matching: every driver in every image (HW-002)

Status: implemented 2026-10-23 (hw002). Backlog: HW-002 (and HW-001, the
IdeaPad). Related: [typed manifests](ccl-typed-manifests.md),
[declaration checking](declaration-checking.md) (CAP-003),
[driver manifests](ccl-driver-manifests.md).

Every live and installer image carries every driver. Each driver says in its
typed manifest which devices it drives. devmgr enumerates the hardware,
starts a driver only for a present device it matches, and logs one line per
device: which driver it bound, or that nothing matched. An image no longer
chooses the hardware.

## Declaring matches

A manifest's `drives` field lists every device the driver accepts. It
replaces the single `device` field.

```lisp
(Executable_Manifest
  identity => "com.cubit.driver.intel-gpu"
  version => "0.1.0"
  # Alder Lake-N and Twin Lake graphics (N95 NUC: 46D2; i3-N350: 46D0..46D4).
  drives => [(Device_Match.PCI_IDs (PCI_IDs 0x8086 0x46D0 0x46D4))]
  requests => [ ... ])
```

The alternatives of `Device_Match` (userspace/ccl/interfaces/executable-manifest.ccl):

| Alternative | Payload | Matches |
|---|---|---|
| `PCI_IDs` | `(PCI_IDs vendor first last)` | a PCI function with that vendor ID and a device ID in `first .. last` (inclusive). One device: `first = last`. |
| `PCI_Class` | `(PCI_Class class subclass interface)` | a PCI function whose class code is exactly those three bytes (base class, subclass, programming interface). |
| `ACPI_ID` | `"PNP0C50"` | an ACPI device whose `_HID` or `_CID` is this ID: 7 characters (`PNP` + 4 hex) or 8 (4 vendor characters + 4 hex). |
| `USB_Class` | `(USB_Class class subclass protocol)` | a USB interface with exactly that class triple. |
| `Platform` | `Platform_Device.PS2_Controller`, `ATA_Primary`, `CMOS_RTC` | a legacy device in devmgr's platform catalog. |

Integers may be written in hexadecimal with a `0x` prefix (`0x8086`), as
PCI and USB IDs are everywhere else. Write IDs in hex.

### Rules (checked by ccl-manifest)

- At most 16 entries. Two equal entries are refused.
- Vendor and device IDs are 16 bits; `0xFFFF` is not a vendor ("no device"
  in configuration space). `first <= last`.
- Class, subclass, interface and the USB triple are bytes.
- An ACPI ID is 7 or 8 characters of `A..Z` and `0..9`.
- `device-memory`, `io-ports`, `interrupt` and `dma` requests are relative
  to the matched device, so a manifest with resources must have at least one
  entry that devmgr enumerates and gives resources for: all `PCI_*`, or
  one `Platform` device. Mixing PCI and Platform entries with resources is
  refused. `ACPI_ID` and `USB_Class` entries carry no devmgr resources yet.
- A driver with no `drives` is not started by matching (ramdisk serves the
  live image, not a device).

### Precedence

Each device binds at most one driver:
1. a `PCI_IDs` entry beats a `PCI_Class` entry;
2. among `PCI_IDs` entries, the narrower range wins;
3. two drivers in one image with equally specific matching entries for the
   same device are ambiguous. devmgr binds neither and logs it.

So a vendor-specific driver can declare its exact IDs and still coexist
with a class driver (virtio-gpu's ID beats a future generic VGA class
driver).

## What devmgr does

1. Scans PCI (every bus; functions 1..7 when function 0 is multifunction).
2. Reads the image's driver index, `devices.index` in the bootstrap
   archive. The image realizer generates it from the `.cubit.devices`
   section of every payload that has one: nobody writes it by hand.
3. Picks, for each device, the best match (precedence above), and logs
   exactly one line for it:

   ```
   devmgr: match 00:02.0 8086:46D2 class 03.00.00 -> intel-gpu.drv (PCI_IDs 8086:46D0..46D4)
   devmgr: match 00:12.7 8086:54FF class 01.09.01 -> unmatched
   ```

   An ambiguous device is a Warning naming both drivers. Every line, and a
   summary (`devmgr: drivers bound to N PCI devices; M matched no driver`),
   reaches logstore (the boot-logs view: press B for the bring-up sources)
   and the serial console. Platform
   devices get the same line (`devmgr: match platform PS2_Controller ->
   ps2.drv`). ACPI and USB entries are listed once per driver as declared
   but not enumerated by devmgr (ACPI devices come from the ACPI service,
   USB interfaces from xhci).
4. Starts the bound drivers. Before a driver runs, devmgr checks that the
   driver's own `.cubit.devices` section lists exactly what the index lists
   for it (a tampered index cannot bind a driver to a device it never
   declared); a program the index does not list may declare no devices.
   A driver devmgr has dedicated setup for (nvme, ata, ps2, xhci, hda,
   virtio-net, virtio-gpu, virtio-input, intel-gpu) gets it, as before.
   That setup starts one instance: when two devices bind the same such
   driver (the IdeaPad's two xHCI controllers), the line for the other says
   "not started: devmgr starts one xhci.drv". It keeps the earlier choice:
   the first virtio-input and intel-gpu device, the last in scan order for
   the rest. Any
   other PCI driver gets the generic launch (below): memory decoding and
   bus mastering enabled, its declared windows, interrupt and DMA region.
   New drivers declare and go: they do not edit devmgr.

## Generic PCI launch: what the driver receives

A matched PCI driver with no dedicated devmgr setup is launched from its
manifest alone. Authority arrives only as capabilities in its startup table;
the typed launch configuration says where things are.

**Capabilities (startup table, `Startup.<binding>`):**
- each `device-memory` request: a device-memory capability covering exactly
  the granted part of that BAR;
- the `interrupt` request: the interrupt capability of the allocated vector
  block (MSI-X when the function has it and the request asks for it, else
  MSI). Legacy lines are not given by the generic launch: without MSI the
  driver runs `Polled`, and a required interrupt binding then fails its
  launch;
- `(service "device-manager" ...)`: devmgr's endpoint, for `OP_READY` and
  `OP_NOT_PRESENT`;
- `(service "logstore" ...)`, optional: a publisher endpoint to logstore
  badged for this driver (`Startup.Logstore`, typed, so
  `CuBit.Logging.Bound_To (Startup.Logstore)` works), for the bring-up
  logging convention.

**When.** Generic drivers start once logstore runs: devmgr launches them
from its service loop when procmgr offers it logstore, so every bring-up
line reaches the boot-logs view (the laptop has no serial port). They place
their payloads anywhere in the image (bootstrap or `apps/`). A profile that
starts no logstore starts no generic driver. devmgr does not wait for their
readiness report before going on; it handles `OP_READY` or
`OP_NOT_PRESENT` in its service loop.

**Launch configuration `Generic_PCI`** (`Launch_Configuration.
Generic_PCI_Configuration`, shared/launch/launch_configuration.ads; read
with `CuBit.Launch_Configurations.Get`):

| Field | Meaning |
|---|---|
| `Location` | PCI routing ID: bus << 8, device << 3, function |
| `Vendor`, `Device`, `Revision`, `Class_Code` | identity, for per-device facts (class code: base << 16, subclass << 8, interface) |
| `Windows (1 .. Window_Total)` | for the K-th `device-memory` request in manifest order: physical `First` and granted `Bytes` (at most the BAR's size and `max_bytes`). At most 6, one per BAR of a type-0 header (`PCI_Type0_BAR_Count`). |
| `DMA`, `DMA_Virtual` | the region allocated for the `dma` request (one per driver; it is physically contiguous, so the driver subdivides it into queues and buffers itself): physical first and bytes, and the address devmgr mapped it at in the driver. `(0, 0)` and 0 without one. |
| `Delivery`, `Vectors` | how the interrupt request was met: `Polled` (0 vectors), `MSI` (1), `MSI_X` (1 .. 32, at most the request's count and the function's table). `Line` is reserved for a later legacy-line path. |

The driver maps window K with
`CuBit.Handles.Devices.Map_Device (Startup.<binding K>, Windows (K).First,
<its own virtual address>, <pages>, Read_Only => ...)`. A physical address
is not authority: the kernel refuses any page outside the capability. No
virtual address is fixed for registers; the driver picks.

**Interrupt protocol.**
1. At launch, MSI-X entries are programmed and masked. MSI is programmed and
   enabled (it has no mask), and INTx is disabled.
2. The driver initializes the device with its interrupts off in the device,
   and arms its handler on the interrupt capability.
3. It sends `CuBit.Driver_Readiness.Ready_Label` (`OP_READY`, no words)
   to `Startup.Device_Manager` (or whatever it named the
   device-manager binding). devmgr then unmasks the MSI-X function and
   entries. Messages raised while masked are delivered then.
4. It enables interrupts in the device.

To decline the device (wrong revision, firmware missing), send
`Not_Present_Label` (`OP_NOT_PRESENT`) instead. devmgr masks and releases
the vectors and logs the refusal.

## Image check

The image realizer (`userspace/ccl/tools/ccl-image/realize.py`) passes
every `.drv` payload of the image to `ccl-device-index`, a hosted Ada tool
built with ccl-image. It reads each driver's `.cubit.devices` with the same
decoder devmgr uses, refuses an image in which two drivers have equally
specific entries that can match one device (`CCL.Device_Index.
First_Ambiguity`, naming both drivers and entries), and writes
`devices.index` into the bootstrap archive. A profile cannot place a file
of that name. That joins CAP-003's whole-image check (slice 4) when it
lands.

`devices.index`: little-endian, magic `CBDI`, version 1, the driver count
(at most 32); then per driver a 300-byte record: the file name (1 .. 32 of
`[a-z0-9._-]`, NUL-padded), the section length (u16), two zero bytes, and
the driver's `.cubit.devices` section zero-padded to its maximum
(`CCL.Device_Index`, decoded by the proved `Decode`).

## ELF section `.cubit.devices`

Little-endian. Written by `CCL.Manifests.Encoding`, decoded by the proved
`CCL.Device_Sections.Decode` (devmgr, the realizer's check, tests).

| Offset | Bytes | Field |
|---|---|---|
| 0 | 4 | magic `CBDV` (`0x56444243`) |
| 4 | 2 | version 1 |
| 6 | 2 | entry count, 1 .. 16 |
| 8 | 16 × count | entries |

Entry: byte 0 is the kind (1 `PCI_IDs`, 2 `PCI_Class`, 3 `ACPI_ID`,
4 `USB_Class`, 5 `Platform`), byte 1 the payload length, bytes 2..3 zero,
then the payload:
- `PCI_IDs`: vendor, first, last (three u16);
- `PCI_Class`, `USB_Class`: three bytes;
- `ACPI_ID`: the ASCII ID, 7 or 8 bytes;
- `Platform`: the platform device code (1 PS2_Controller, 2 ATA_Primary,
  3 CMOS_RTC).

Unused payload bytes are zero. The decoder accepts exactly what the encoder
writes.

`.cubit.resources` keeps its layout; its 16-byte header now names the
resource basis (`No_Device`, `PCI_Function`, or a `Platform` device), which
the encoder derives from `drives`.

## Assumed, not enumerated

- **Platform devices.** The PS/2 controller and CMOS RTC are taken as
  present (no ACPI/FADT check yet). The primary ATA channel is present when
  PCI shows an IDE controller (class 01.01).
- **ACPI and USB.** Declared and checked, not yet bound by devmgr.
- **Storage on a live image.** Every image now carries nvme.drv and
  ata.drv, so a live boot on a machine with an NVMe disk binds it and
  attaches it to the filesystem as `@nvme:0`, as the development image
  does. The live settings keep Config in memory; nothing writes there
  unless a program the user runs does.

## Bring-up logging (photographs of the screen)

Following the hardware bring-up logging convention
(coordination/release-stack.md), everything a laptop boot needs to explain
itself reaches logstore, one line each, and the boot-logs view shows it:

- devmgr: one line per PCI device (`devmgr: match BB:DD.F VVVV:DDDD class
  CC.SS.PP -> <driver> (<entry>)` or `-> unmatched`), one per platform
  device, the summary, and `devmgr: render admission refused: <reason>`
  when a render session is refused at once (no supported GPU bound,
  intel-gpu.drv not running).
- intel-gpu: `intel-gpu: stage <name> ok|fail <details>` for identify
  (device, family, GT, revision as the stepping, BAR0, GGTT size),
  registers, forcewake, ggtt, firmware, reset, guc, ct and render, then
  `intel-gpu: bring-up reached <stage> (n/9)` or `... complete`
  (`Intel_GPU_Bring_Up`). The render stage says whether render sessions,
  and so Desktop's GPU rendering, are admitted, and if not why.
- procmgr: `procmgr: stage render-grant ok|fail desktop.svc setting=...`
  (the `desktop.render.gpu` decision) and `procmgr: stage render-admission
  ok|fail desktop.svc ...` (whether devmgr admitted the session).

The boot-logs view filters by source (the text before the first `: `):
**Tab** cycles through every source seen, **B** shows devmgr, intel-gpu and
procmgr together, so one photograph holds the whole GPU bring-up.

## Desktop's GPU rendering is a setting

`desktop.render.gpu` (`GPU_Rendering`, `CCL.Interfaces.GPU_Power`; catalog
`userspace/ccl/interfaces/settings/startup.ccl`) decides whether Desktop
may render on the GPU. The live startup plan delegates Desktop's render
approval to it: `(start "desktop.svc" (priority 4) (render setting
"desktop.render.gpu"))`. procmgr reads it when it starts Desktop: absent
means enabled, `(GPU_Rendering enabled => false)` keeps Desktop in software
rendering, and a value that does not decode refuses. devmgr still admits
the session only for a present, supported GPU, so on QEMU or an unsupported
GPU Desktop renders in software. There is no `-gpu` image variant.

## Tests

- `tests/device-matching/run.sh`: the real drivers' index, bound against the
  IdeaPad's lspci and QEMU q35; precedence; the tool refusing an ambiguous
  image; every truncation and bit flip of the index and a section.
- `tests/headless/run.sh --test generic-launch`: QEMU's edu device binds
  the fixture driver `tests/generic-launch/driver` (in the development
  image only), which exercises the whole generic launch: window, DMA, an
  MSI raised after `OP_READY`, and its stage lines through logstore.
- `tests/ccl-manifests/test-manifests.py` (`test_typed_drives` and the
  resource-basis cases), `tests/driver-manifests/run.sh` (devmgr reads
  `.cubit.devices`), `tests/launch-configuration/run.sh` (Generic_PCI),
  `tests/ccl-types/run.sh` (hex literals), `tests/intel-gpu/adln_family.gpr`
  (the ADL-N family gates).

## Firmware

Firmware is baked into the driver's binary at build time. devmgr never
reads, checks or grants firmware, and a driver gets no filesystem endpoint
for it.

**Where it lives.** The top-level `firmware/` directory mirrors
linux-firmware, so it can become its own repository, or be swapped for
upstream, with no path changes:
- the files at their upstream paths (`firmware/i915/tgl_guc_70.bin`,
  `firmware/rtw89/rtw8852b_fw-1.bin`);
- the licence texts at the top level under their upstream names
  (`firmware/LICENSE.i915`, `firmware/LICENCE.rtlwifi_firmware.txt`);
- `firmware/WHENCE`: for each file, the driver that uses it, the upstream
  path, the tag, the SHA-256 and its licence.

`.gitattributes` keeps `firmware/**` byte-exact. Nothing outside `firmware/`
holds blobs.

**Declaring it** in the driver's `component.ccl` (schema:
`userspace/ccl/interfaces/build.ccl`):

```lisp
(Component name => "rtw89" kind => Kind.Driver
  source => (Source.Ada (Ada_Source ...))
  embedded => [(Blob name => "rtw8852b-firmware"
                 source => (vendored "firmware/rtw89/rtw8852b_fw-1.bin"
                   (SHA256_Digest 0x5b68415e 0x3bfe7271 0x5d63a707 0x03d4471b
                                  0x04b5475b 0x8ff69cfd 0xc8cdf482 0x33cd5d3a))
                 licence => "firmware/LICENCE.rtlwifi_firmware.txt")])
```

- `vendored path digest` names a file under `firmware/`. `fetch url digest`
  names an `http://` or `https://` download, which is fetched once into the
  Storehouse and then answered from it by its digest. Keep `fetch` for
  large downloads; firmware is vendored.
- A `SHA256_Digest` is the hex digest as eight 32-bit words, in order. The
  CCL type checker checks the form, and ccl-build checks the bytes. A
  mismatch is a hard error.
- The blob name is lowercase kebab and unique in the component.

**Using it from Ada.** ccl-build generates package `Embedded`, which the
driver `with`s. For the declaration above it is:

```ada
package Embedded with Preelaborate is
   --  firmware/rtw89/rtw8852b_fw-1.bin
   --  SHA-256 5b68415e3bfe72715d63a70703d4471b04b5475b8ff69cfdc8cdf48233cd5d3a
   Rtw8852b_Firmware_Bytes : constant := 1245944;
   type Rtw8852b_Firmware_Data is array (1 .. Rtw8852b_Firmware_Bytes) of Interfaces.Unsigned_8;
   Rtw8852b_Firmware : constant Rtw8852b_Firmware_Data
     with Import, Convention => C, External_Name => "cubit_blob_rtw8852b_firmware_start";
end Embedded;
```

The bytes are in the program's read-only data. The licence file is
exported beside the program (`build/licenses/`) for its bundle. See
docs/ccl-build.md, "Embedded blobs".


## Follow-ups (for the HW-002 backlog entry)

- **GuC embedded.** intel-gpu embeds `firmware/i915/tgl_guc_70.bin`
  instead of reading it through a filesystem endpoint devmgr grants
  (`Grant_Intel_Firmware`), and the images stop placing the file. Prepared
  as a separate patch, to be verified on the NUC (QEMU has no Intel GPU).
- **One instance per dedicated setup.** devmgr's xhci, nvme, hda and
  virtio setups start one driver; the IdeaPad has two xHCI controllers
  (00:0d.0 TCSS, 00:14.0 PCH) and only the PCH one is started.
- **ACPI and USB binding.** `ACPI_ID` and `USB_Class` entries are declared
  and checked but bound by no one yet (the ACPI service's device list;
  xhci's interfaces).
- **Legacy lines in the generic launch.** `Interrupt_Delivery.Line` is
  reserved; generic drivers get MSI or MSI-X, or poll.
- **Licences in bundles.** ccl-build exports an embedded blob's licence at
  `build/licenses/`; the Drivers/<name>/<version>/licenses/ bundle layout
  (APP bundles) is not built yet.
