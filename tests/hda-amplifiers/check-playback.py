#!/usr/bin/env python3
"""Check native HDA read-backs and DMA diagnostics from the audio benchmark."""
import argparse
from pathlib import Path
import re
import runpy

parser = argparse.ArgumentParser()
parser.add_argument("serial", type=Path)
parser.add_argument("--wav", type=Path)
args = parser.parse_args()
log = args.serial.read_text(errors="replace")

for marker in ("hda: output configured and verified", "AUDIO-BENCH: COMPLETE",
               "mixer: HDA period IRQ active"):
    if marker not in log:
        raise SystemExit(f"FAIL: missing {marker}")
for marker in ("read-back failed", "verb timeout", "EXCEPTION", "PANIC"):
    if marker in log:
        raise SystemExit(f"FAIL: {marker}")

def value(name):
    match = re.search(r"hda: " + re.escape(name) + r"=([0-9A-F]{8})", log)
    if not match:
        raise SystemExit(f"FAIL: missing {name}")
    return int(match[1], 16)

if value("DMA completed periods") == 0 or value("DMA error bits") != 0:
    raise SystemExit("FAIL: DMA did not complete cleanly")
if value("DAC stream/channel") != 0x10 or value("DAC format") != 0x11:
    raise SystemExit("FAIL: converter configuration")

# Each present output amplifier must read back the requested unmuted gain.
amps = re.findall(
    r"hda: amp requested gain=([0-9A-F]{8})\s+"
    r"hda: amp left gain/mute=([0-9A-F]{8})\s+"
    r"hda: amp right gain/mute=([0-9A-F]{8})", log)
if not amps or any(gain != left or gain != right for gain, left, right in amps):
    raise SystemExit("FAIL: amplifier read-back")

if args.wav:
    # Reuse the benchmark analyzer, including its in-memory handling of
    # QEMU's unfinalized WAV lengths after the harness sends SIGTERM.
    analyzer = runpy.run_path(str(Path(__file__).resolve().parents[1] /
                                 "performance" / "report.py"))
    print(analyzer["wave_report"](args.wav))
print("PASS: native HDA amplifier read-back and DMA playback")
