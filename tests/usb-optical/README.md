# Native USB optical live boot tests

See [USB optical live boot](../../docs/usb-optical-live-boot.md) for image usage,
scope, security boundaries and hardware limitations.

```sh
nix develop -c make -C kernel test-usb-optical prove-usb-optical
nix develop -c make -C kernel usb-live-iso
nix develop -c python3 tests/usb-optical/run-live.py --cpus 1
nix develop -c python3 tests/usb-optical/run-live.py --cpus 4 --disk-first --mouse-first --eject
```

- USB wire tests: read-only BOT/SCSI framing, lengths, tags, residues, phase
  errors, optical geometry, sense and LUN decoding.
- Configuration tests: bounded/composite interfaces, duplicate identities,
  missing endpoints, unsupported transports, truncation and alternate settings.
- Completion tests: isolated bounded mailboxes, FIFO, wraparound, saturation.
- Capability tests: literal HCSPARAMS2 fixtures and all 1,024 scratchpad counts,
  with unrelated register bits varied. Catches reversed low/high fields.
- DMA layout tests: counts 0..1023 (including 34, 128 and the 512/513 pointer
  table boundary), every device slot, minimal power-of-two allocation, page
  alignment, table capacity, bounds and page nonoverlap.
- Port tests: the laptop's in-progress SuperSpeed reset, already-enabled USB3,
  USB2 reset, and PORTSC writes that do not echo disable/change/strobe bits.
- Legacy capability tests: xECP/relative-link decoding, MMIO range/alignment
  checks, and SMI-control bit preservation/disable/acknowledgment masks.
- ISO tests: dual-endian agreement, bounded records/extents, unsupported flags,
  truncation, primary descriptor geometry and filename matching.
- Image audit: actual ISO primary tree and CPIO contents; apps/WAD on CD only.
- USB-only boot: no ATA/NVMe or second initrd; desktop, DOOM, Workbench and Files.
  Screenshots require visual review, not just a "process spawned" marker.
- Media removal: reads fail closed; desktop/mouse survive. Apps dependent on
  missing data may exit. Reinsertion/remount is not implemented.

KVM required. Logs/screenshots stay in /tmp/cubit-usb-live.*. QMP uses request
IDs, not ambiguous human-monitor prompts. Do not run image-building/headless
tests concurrently against shared kernel staging. run-hid.sh uses the original
NVMe Devices fixture and validates mouse-only regression, not CD reads.

Bundled Workbench samples, loaded through native directory/file IPC:

```sh
nix develop -c make -C kernel test-ccl-images usb-live-iso
nix develop -c python3 tests/usb-optical/run-live.py --cpus 4 --ccl-samples
```

The image test compares all four RAM-workspace copies with their repository
sources. The boot test opens `button-clock.ccl` using Ctrl+O/Enter, interprets it
with F5, clicks Refresh, and checks that its clock label changes. Screenshots of
the picker, loaded document and callback result are retained. These tests use
the ISO's fresh `@mem:0/work`, never a persistent user workspace. See the
[sample instructions](../../userspace/ccl/samples/README.md).

Taskbar/clock/audio regression (uses the original, freely bundled test ROM):

```sh
nix develop -c python3 tests/usb-optical/run-live.py --sameboy --sameboy-audio --taskbar --timeout 240
```

This boots with a fixed UTC RTC, checks wall-clock initialization, compares the
background before/after a popup and pointer traversal, and measures actual PCM
amplitude after media-key and popup-slider operations. It verifies master mute
and restoration separately from application-local gain/mute/pause. Screenshots
still need visual review for clock text and popup layout. It does not measure
keypress-to-photon latency, acoustic latency, or certify click-free transitions.
