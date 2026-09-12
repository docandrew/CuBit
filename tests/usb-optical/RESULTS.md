# Validation snapshot — 2026-09-12

All builds/tests used the repository's Nix environment. No commits or pushes.

Overnight candidate (superseded by the diagnostic image below):
`kernel/cubit_laptop_usb.iso`, 27,054 sectors (52.84 MiB).
SHA-256: `a863f35117d28ce6cbde71ce5b3bdf7216f573baeb4ed207027b1058d4fc74ec`.
The original laptop ISO still matches its pre-work backup byte for byte.

| Check | Result / retained artifact |
|---|---|
| Full from-source USB ISO build + membership audit | PASS; `/tmp/cubit-usb-final-image.log` |
| Four-CPU final ISO, disk LUN 0 + CD LUN 1, mouse first | PASS; `/tmp/cubit-usb-live.dsox5v3j/` |
| DOOM gameplay, Workbench + Files together, 1920×1080 | Captured and visually inspected in final run |
| CD removal, fail-closed launch, no writable-backend fallback | PASS; final run |
| USB mouse after removal | 505 reports, 18 button transitions in last aggregate; zero completion errors/short reports |
| Earlier one-CPU CD LUN 0 boot + DOOM | PASS; `/tmp/cubit-usb-live.i1qvlbit/` |
| Earlier reversed-port four-CPU CD LUN 1 boot | PASS; `/tmp/cubit-usb-live._fbcoc9i/` |
| Original mouse-only/NVMe Devices regression | PASS; `/tmp/cubit-usb-hid.XjlTSq/`, `/tmp/cubit-usb-hid-final.log` |
| Hosted USB + ISO tests / focused SPARK | PASS; `/tmp/cubit-iso-proof.log` |

The proof covers four pure packages: USB_Optical, USB_Configurations,
XHCI_Completions and ISO_Records. All 177 analysis checks discharge, zero
justified/unproved. This is not proof of hardware, native adapters, DMA,
the whole kernel or end-to-end I/O.

The original mouse-only regression predates the last fail-closed ISO lookup
adjustment; the final USB-only test exercises that adjustment explicitly.
The one-CPU snapshot predates widescreen/media-removal hardening.

Real laptop/IODD boot subsequently failed with a black screen after GRUB in both
graphics modes. Sound quality listening and latency benchmarking remain
outstanding. Audio in these tests uses QEMU's non-playing audio sink. The test
does not pretend to validate audible quality. SameBoy is deliberately deferred.

## Early-boot diagnostic image (2026-09-12)

Rebuilt kernel and USB ISO in Nix; membership audit passes (7 bootstrap files,
17 CD payload files). Diagnostic image (superseded below): 27,056 sectors.
SHA-256: `316942079517e0ab88e5c918af3c3171447bf3a228acacb74daef793d892e816`.

- Third GRUB entry selected through QMP keyboard input: text checkpoints reach
  `EARLY: memory initialization complete`. Serial and actual VGA screenshot:
  `/tmp/cubit-usb-live.pn5_cyo9/`. Screenshot visually confirms text output.
- Four-CPU normal graphical USB boot and DOOM/Workbench/Files launch regression:
  PASS; `/tmp/cubit-usb-live.7ofk7_1c/`,
  `/tmp/cubit-early-boot-graphics-test.log`.
- Build log: `/tmp/cubit-early-boot-build.log`.

This adds diagnostics, not a confirmed fix for the real-hardware failure.
No new proof claims. The earlier media-removal and hosted proof results above
belong to the overnight candidate, not a rerun on this diagnostic image.

## Scratchpad-count decoder fix (2026-09-12)

Laptop diagnostics reached xHCI and reported `scratchpad-limit`. The decoder
had reversed HCSPARAMS2's low/high five-bit fields. Native code now uses the
extracted, corrected XHCI_Capabilities decoder; the 16-buffer allocation bound
is unchanged. Raw HCSPARAMS2 and required count are logged for hardware retest.

Decoder-fix image (superseded below): 27,056 sectors.
SHA-256: `6f18014abe93270fb7514998795da2f58065d48219488d2d17991a047b468180`.

- Regression verified to fail on the original decoder at the one-buffer
  literal fixture: `/tmp/cubit-scratchpad-before.log`.
- Corrected decoder: all 1,024 counts, literal boundary fixtures, and unrelated
  bits PASS; all other hosted USB/ISO tests PASS. Native xHCI rebuild and image
  membership audit PASS: `/tmp/cubit-scratchpad-build.log`.
- Focused SPARK run proves the decoder's result range check and termination:
  `/tmp/cubit-scratchpad-proof.log`. This is not a hardware-conformance proof;
  register semantics are checked by the fixtures and exhaustive tests.
- Four-CPU KVM, mouse first, disk LUN 0/CD LUN 1, desktop and DOOM/Workbench/Files
  launching, then CD removal with fail-closed storage and live HID: PASS.
  Artifacts: `/tmp/cubit-usb-live.mvyb_9fe/`, `/tmp/cubit-scratchpad-qemu.log`.
  QEMU advertises zero scratchpads, so this native test does not exercise DMA
  into nonzero scratchpad buffers. Laptop validation is still required.

## Laptop scratchpad capacity expansion (2026-09-12)

The next hardware test reported `required=00000022` (34 buffers) versus
`supported=00000010` (16). Capacity is now 64 with a shared XHCI_DMA_Layout
used by both devmgr allocation/authority sizing and native driver addressing.
DMA grows from 512 KiB to 1 MiB. Derived offsets place all device rings and
bulk data after the full scratchpad reservation; compile-time checks enforce
table, stride, native array, and arena sizes. Requirements above 64 still fail.

64-buffer image (superseded below): 27,058 sectors.
SHA-256: `b441c1c53ea433aaba8cced1116ab9ff255af8b77650af8edd7bccc29dcd3acb`.

- All hosted USB/ISO tests PASS, including a new DMA-layout test for every
  count 0..64, eight device slots, allocation bounds and page nonoverlap.
- Nix native xHCI + devmgr rebuild and image membership audit PASS:
  `/tmp/cubit-scratchpad-capacity-build.log`.
- Four-CPU KVM, mouse first, disk LUN 0/CD LUN 1, desktop/app loading, and
  media removal with storage fail-closed/HID live PASS:
  `/tmp/cubit-usb-live.bqf3vpgw/`, `/tmp/cubit-scratchpad-capacity-qemu.log`.
- Hardware use of nonzero scratchpad buffers still requires laptop validation;
  QEMU advertises zero. No new SPARK proof claim for hardware or DMA behavior.

## Dynamic scratchpads and port-reset diagnostics (2026-09-12)

Hardware progressed with the 64-buffer image to `connected PORTSC=00001211`,
then stalled. The precise wait/stall was not identified by the old logging.

Dynamic/reset diagnostic image (superseded below): 27,062 sectors.
SHA-256: `fde11a227983b0282ec0499d46ca87610702ba19d10ed6657447b48d71097215`.

- Native devmgr reads the controller count and allocates a computed DMA region.
  xHCI validates the configuration count and independently matches hardware
  before DMA access. Scratchpad pointer arrays have their exact runtime bound.
- Hosted DMA tests PASS for all counts 0..1023: pointer-table sizes 0/1/2 pages,
  minimal allocation, all device slots and no overlapping occupied pages.
- Hosted port tests PASS for the exact hardware status, existing reset waits,
  USB2/USB3 selection and neutral PORTSC reset writes. All other hosted USB/ISO
  tests PASS: `/tmp/cubit-dynamic-scratchpad-build.log` (initial native build
  failure in that log was corrected before the final native build below).
- Focused SPARK analysis of XHCI_DMA_Layout and XHCI_Ports passes runtime checks
  and termination: `/tmp/cubit-dynamic-scratchpad-proof.log`. No claim that this
  proves hardware conformance or the cause of the real-hardware stall.
- Final Nix native devmgr/xHCI builds, image audit, four-CPU USB-only boot with
  CD LUN 1 and mouse first, desktop/DOOM/Workbench/Files, and media removal with
  storage fail-closed/HID live PASS: `/tmp/cubit-dynamic-scratchpad-native.log`,
  `/tmp/cubit-usb-live.3tw6tzks/`.
- QEMU reports zero scratchpads (128 DMA pages). Nonzero hardware DMA and the
  laptop port stall still require hardware retest. Boot-only diagnostics now
  separate port reset from Enable Slot submission and report timeout registers.

## Firmware ownership and sleep diagnostics (2026-09-12)

Laptop remained at `waiting for existing port reset` for about a minute without
timing out. The driver has bounded iterations, but not a guaranteed wall-clock
timeout if a sleep does not resume. No kernel/scheduler modification was made.

Current image: `kernel/cubit_laptop_usb.iso`, 27,064 sectors.
SHA-256: `09b39ce3d2b4c5da3304ec44de0ac6718d8f11899998ea7137795cc7c65bc69e`.

- Added pre-reset sleep/wakeup markers and first-sleep/progress diagnostics for
  register waits. Added legacy firmware ownership handshake before stop/reset,
  validated capability ranges and SMI control updates after ownership. Refused
  ownership fails closed rather than forcing firmware ownership clear.
- Hosted USB/ISO tests, including new legacy register fixtures, PASS. Native
  xHCI build, ISO membership audit and four-CPU CD LUN 1/mouse-first live boot,
  app launching and media removal/HID survival PASS:
  `/tmp/cubit-xhci-handoff-native.log`, `/tmp/cubit-usb-live.44v0rs22/`.
- Native QEMU shows `pre-reset sleep resumed` and `no firmware ownership
  capability`; actual BIOS handoff is therefore not covered by this QEMU run.
- Pure XHCI_Legacy focused SPARK checks PASS:
  `/tmp/cubit-xhci-handoff-proof.log`. This is not a proof of hardware handoff
  completion, timeout wall-clock bounds, or kernel wakeup correctness.
- Real laptop cause remains unconfirmed. Retest legacy diagnostic entry and
  record the last sleep/ownership/port messages.
