# HDA amplifier initialization and laptop diagnostics

The HDA driver formerly wrote gain `0x7f` to both the DAC and output pin.
The Dell subsystem `1028:0740` codec dump identifies an ALC3253 (`10ec:0225`)
at codec address zero, with a direct speaker route from DAC `0x02` to pin
`0x14`. The DAC's maximum and 0 dB offset are both `0x57`; the pin amplifier
only supports mute/unmute and needs gain zero. HDMI is a separate codec at
address two. No private codec dumps or audio recordings are stored here.

The driver now uses output-amplifier presence and parameter-override flags,
inherits AFG parameters where required, chooses min(0 dB offset, maximum),
and reads back both channels. It waits for D0 on power-controllable widgets,
preserves EAPD balance/channel-swap bits, and verifies converter and pin state.
Failed initialization is not registered as usable output. Missing controller
allocation or failed MMIO mapping is rejected before register access.

Codec replies are polled and acknowledged by the command path; only playback
stream interrupts are enabled in INTCTL. An early version of this change wrote
zero to stream status on a non-stream IRQ while leaving RIRB status asserted.
QEMU retriggered MSI, repeatedly waking HDA and starving desktop startup on one
CPU. The USB boot test (one CPU by default) caught this; the driver now clears
the actual response source and does not write an empty stream acknowledgement.

Verb/bit definitions were cross-checked against the
[Linux HDA definitions](https://github.com/torvalds/linux/blob/master/include/sound/hda_verbs.h).
The hardware boundary remains unproved. The small pure SPARK amplifier decoder
has executable regression tests; SPARK mode is not a claim of a completed proof.

## Linux-hosted regression

From the repository root:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/hda-amplifiers/amplifiers.gpr && ../tests/hda-amplifiers/build/main'
nix develop -c bash tests/headless/run.sh --test bench-audio --accel kvm --timeout 45 --keep-logs --serial /tmp/hda-audio.log
nix develop -c python3 tests/hda-amplifiers/check-playback.py /tmp/hda-audio.log
```

Pass the WAV capture path printed by the benchmark using `--wav PATH` to also
check that QEMU received non-silent samples. This is not an analog-quality test.
The host-only test enables assertions; native driver/kernel flags are unchanged.

After building the USB ISO below, exercise the missing-controller path with:

```sh
nix develop -c python3 tests/usb-optical/run-live.py --without-audio
nix develop -c python3 tests/usb-optical/run-live.py
```

This omits HDA hardware, requires an allocation/mapping failure diagnostic, rejects a
false HDA registration, and still launches DOOM, Workbench and Files from CD.
It does not inject a codec timeout or emulate an amplifier read-back failure.

## Laptop test

Rebuild the IODD image with the existing explicitly opted-in local cartridges:

```sh
SAMEBOY_ROMS_DIR=/home/doc/git/cubit/local-roms nix develop -c make -C kernel usb-live-iso
```

Boot `kernel/cubit_laptop_usb.iso`, try DOOM, then close it. Initialization logs
include codec identity, selected nodes, power, amplifier capabilities and actual
gain/mute, EAPD, pin control, converter stream and format. Expected laptop values:

| Field | Expected hexadecimal value |
| --- | --- |
| codec 0 vendor | 10EC0225 |
| selected DAC / pin | 02 / 14 |
| DAC amplifier gain (both channels) | 57 |
| speaker pin amplifier gain/mute | 00 |
| pin EAPD enable bit | 02 set |
| pin output enable bit | 40 set |
| DAC stream/channel | 10 |
| DAC format | 11 |

The first playback session reports completion count, first completion position,
stop position and accumulated DMA error bits when it stops. No serial writes or
codec queries were added to the interrupt/refill path. Nonzero completions and
zero error bits support DMA progress, not proof of an audible analog signal.

Limits: discovery still assumes codec zero and the existing simple first-DAC /
first-output-pin route. This happens to match this laptop. General topology
selection, jack switching, master volume/media keys, and vendor-specific Realtek
initialization are separate work. QEMU does not emulate this Realtek codec;
successful virtual playback cannot establish that the laptop speakers work.
After a verb timeout the transport rejects further commands to avoid accepting
a late response as a later command's reply; recovery requires reinitialization.
