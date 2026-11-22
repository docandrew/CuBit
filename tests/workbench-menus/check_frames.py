#!/usr/bin/env python3
"""Check the Workbench menu bar test's frames (run.sh): menus open and
change, close without leaving their popup behind, run their commands, and
an outside press only closes them. Usage: check_frames.py CAPTURE_DIR [PNG_DIR]"""
import sys
from pathlib import Path
from PIL import Image, ImageChops

frames = Path(sys.argv[1])
png = Path(sys.argv[2]) if len(sys.argv) > 2 else None
failures = 0
POPUP = (0, 22, 320, 330)          # where File, Edit and View drop down
EXECUTION = (8, 60, 300, 140)      # the Execution panel's status lines


def frame(n):
    """What the window showed after n events: the last frame presented by
    then (an event that changes nothing presents nothing)."""
    for k in range(n, -1, -1):
        path = frames / f"frame-{k:02d}.bmp"
        if path.exists():
            return Image.open(path).convert("RGB")
    raise SystemExit("WORKBENCH MENUS: FAIL no frame presented")


def differs(a, b, box):
    return ImageChops.difference(a.crop(box), b.crop(box)).getbbox() is not None


def check(ok, what):
    global failures
    print(("ok   " if ok else "FAIL ") + what)
    if not ok:
        failures += 1


first = frame(0)
check(differs(first, frame(1), POPUP), "F10 opens File")
check(all(differs(frame(n), frame(n + 1), (0, 0, 900, 330)) for n in range(1, 5)),
      "Right walks File, Edit, View, Run, Help")
check(not differs(first, frame(6), POPUP), "Escape closes it and repaints what it covered")
check(differs(first, frame(7), POPUP), "Alt+V opens View")
check(differs(frame(7), frame(8), (0, 60, 900, 500)), "View > REPL shows the REPL")
check(not differs(first, frame(9), (0, 60, 900, 470)), "F6 hides it again")
check(differs(first, frame(11), EXECUTION) or differs(first, frame(11), (0, 470, 900, 500)),
      "Help > About (its mnemonic) runs")
check(differs(frame(11), frame(12), POPUP), "a press on File opens it")
check(differs(frame(11), frame(13), POPUP), "it stays open after the release")
check(not differs(frame(11), frame(15), POPUP), "an outside press only closes it")
if png:
    png.mkdir(parents=True, exist_ok=True)
    for n, name in [(1, "file"), (2, "edit"), (3, "view"), (4, "run"), (5, "help")]:
        frame(n).save(png / f"workbench-menu-{name}.png")
print("WORKBENCH MENUS: " + ("PASS" if failures == 0 else f"FAIL {failures}"))
sys.exit(1 if failures else 0)
