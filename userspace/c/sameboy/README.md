# SameBoy for native CuBit

First playable frontend for the pinned SameBoy Game Boy / Game Boy Color core.
This is a CuBit application, not the Linux SDL frontend or a POSIX subsystem.
It uses native filesystem, mixer and desktop endpoint messages and a read-only
framebuffer grant. No network, raw-device, camera, launch, or writable-file
authority is declared. The filesystem ceiling is read access to `sameboy/`.

## Build and run

All builds use the Nix development environment. From the repository root:

```sh
nix develop -c make -C kernel usb-live-iso
```

Boot `kernel/cubit_laptop_usb.img` on the IODD or use:

```sh
nix develop -c make -C kernel run-usb-live
```

Choose **Apps → SameBoy**. ROM 00 is an original CuBit test cartridge:
four-shade stripes which scroll with Left/Right, and a continuous ~440 Hz tone.
No commercial game or
Nintendo boot firmware is bundled. The upstream open-source replacement
boot ROMs are embedded in the application.

To include your own local cartridges, explicitly supply an **absolute** directory:

```sh
SAMEBOY_ROMS_DIR="$PWD/local-roms" nix develop -c make -C kernel usb-live-iso
```

Only `.gb` / `.gbc` files in that directory are copied, sorted by filename,
as ROM 01–15. The build prints the mapping. Maximum individual size: 8 MiB.
Subdirectories are not scanned. Originals are never changed. `local-roms/`,
`.gb`, `.gbc`, build outputs and the resulting ISO are Git-ignored. The ISO
and retained `/tmp/cubit-usb-image.*` staging directory **do contain those
ROMs**: do not distribute either accidentally. Nix does not fetch or package
private ROMs as source inputs. An unspecified directory means no private ROMs.

The normal ATA/NVMe desktop images also include the app and test cartridge;
private cartridge staging currently applies only to the USB Live CD.
The older flat-initrd laptop fallback does not include SameBoy.

## Controls

| Key | Action |
| --- | --- |
| Arrows | D-pad |
| X / Z | A / B |
| Enter / Tab | Start / Select |
| P | Pause / resume |
| F2 | Next numbered cartridge; wrap to 00 at the first missing slot |
| F5 | Reset cartridge |
| F8 | Mute / unmute SameBoy |
| F9 / F10 | SameBoy volume down / up, in 5% steps |
| Escape | Close application |

The window uses fixed 3× integer scaling. Emulated 8 MHz cycle accounting
drives frame pacing against CuBit's monotonic clock; no upstream host sleeps.
Input is drained once per emulated frame. This is not a measured sub-millisecond
input-latency implementation.

Audio uses the core's 48 kHz stereo S16 callback and the shared `CuBit.Audio`
implementation through `cubit_audio.h`, not a second C implementation of the
ring protocol. Samples are batched; unwritten suffixes survive partial writes.
The outer event loop applies backpressure while remaining responsive to input.
Startup prefills 1,536 frames (32 ms); a stalled output switches to silent
playback with a diagnostic after 250 ms. This is a functional starting point,
not a final low-latency tuning result. Pause, reset and cartridge switching close
the old stream to discard queued audio; resuming opens and prefills a new one.

Volume starts at 70%, persists across cartridge changes within the process,
and controls only SameBoy's stream. Mute preserves the selected volume. Nothing
is persisted across application launches yet. Other apps' volume and hardware
master gain are unaffected; mixer service access is not master-volume authority.

## Current limits / follow-up

- No battery saves, save states, file picker, controller support, or dynamic scale.
  Saves need a separately approved writable location, not broad filesystem access.
- Cartridge RTC does not yet have a trustworthy civil-time source.
- SameBoy is upstream C, not SPARK-proved. Process isolation and existing native
  authority enforcement are the security boundary, not a claim that ROM parsing
  or the emulator itself is memory-safe.
- Global volume UI and multimedia keys need a separately authorized mixer-control
  interface. They must not give every audio-producing app control over other apps.

## Sources and licenses

SameBoy is pinned by `flake.lock` to
`213a12ce93d66b105a113debd9396306066a7cfc` from
<https://github.com/LIJI32/SameBoy>. The linked Core and BootROMs use the
upstream Expat license, copyright Lior Halphon. No upstream files are modified.
The optional ignored `userspace/c/sameboy_src` checkout is for inspection;
the build consumes the pinned Nix input.

OpenLibm comes from the pinned nixpkgs input. Its static library is rebuilt
without Linux TLS stack canaries or fortified-libc calls. Its BSD/ISC/MIT/Sun
notices, including individual source notices, accompany the USB image under
`/licenses/openlibm/`; SameBoy's full license is `/licenses/SameBoy.txt`.
Tests from OpenLibm are not linked. A port-local build of CuBit's compatibility
object renames its legacy math helpers, so OpenLibm supplies the real routines
without changing DOOM or other C ports.

## Regression

```sh
nix develop -c python3 tests/usb-optical/run-live.py --sameboy
nix develop -c python3 tests/usb-optical/run-live.py --sameboy --sameboy-audio
nix develop -c python3 tests/usb-optical/test-stage-roms.py
```

Boots the real USB-only image under KVM, launches SameBoy through Apps, waits
for 120 emulated frames, compares paused screenshots around Right input,
closes it, and launches DOOM, Workbench and Files. Logs and screenshots remain
under the printed `/tmp/cubit-usb-live.*` path. This is a smoke test, not a
commercial-game compatibility suite or formal proof. `--sameboy-audio` captures
QEMU PCM and checks the test tone, 70%→35% amplitude ratio, mute/unmute and
pause/resume, and verifies that closing the final stream stops HDA capture.
This does not measure physical speaker latency or establish
glitch-free playback on every host.

When you have explicitly staged a private ROM 01, add `--sameboy-local-rom`
to the boot test to exercise F2 cartridge switching and capture its intro,
Start-button response, and subsequent A-button input. This option never
downloads or supplies a ROM; inspect the retained screenshots for game-specific
results. Screenshots from private games remain local test artifacts too.
