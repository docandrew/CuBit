#!/usr/bin/env python3
"""Photograph the integrated Deskbar (UI-027) in a running desktop-display
session, through that run's HMP monitor socket, once its own injector is done.

  python3 tests/headless/deskbar-shots.py SERIAL_LOG MONITOR_SOCKET OUT_DIR

Opens a second Workbench (a group of two), Settings and Files, minimizes
Settings, then photographs: the taskbar, Workbench's window list, the Deskbar
panel, and Super+T keyboard focus. Screenshots only: this checks nothing
beyond the serial lines it waits for (a demonstration aid, not a test).
"""
import re
import socket
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])
out.mkdir(parents=True, exist_ok=True)


def deskbar_constant(name):
    """A Desktop_Deskbar_Style geometry constant, read from its source so
    these photographs follow the bar's geometry."""
    source = (Path(__file__).resolve().parents[2] /
              "userspace/services/desktop/desktop_deskbar_style.ads").read_text()
    return int(re.search(rf"^\s*{name}\s*:\s*constant\s*:=\s*(\d+);", source, re.M).group(1))


# Desktop_Deskbar_Style at 1024x768: task buttons start right of Apps and
# its separator. Each is as wide as its content (Desktop_Deskbar_Layout),
# so their places are found on the screen (task_centre), not computed.
SCREEN_W, SCREEN_H = 1024, 768
BAR_H = deskbar_constant("Bar_Height")
BUTTON_H = deskbar_constant("Button_Height")
BUTTON_TOP = (BAR_H - BUTTON_H) // 2
TASK_LEFT = (deskbar_constant("Bar_Inset") + deskbar_constant("Apps_Width") +
             2 * deskbar_constant("Separator_Margin") + 1)
MIN_BUTTON = 20             # narrower runs off the bar fill are not buttons
STEP = 70


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(text, count=1, seconds=60):
    deadline = time.monotonic() + seconds
    while log_text().count(text) < count:
        if time.monotonic() > deadline:
            raise SystemExit(f"deskbar-shots: timed out waiting for {text!r} x{count}")
        time.sleep(0.1)


def hmp(command):
    with socket.socket(socket.AF_UNIX) as s:
        s.settimeout(3)
        s.connect(monitor)
        s.sendall((command + "\n").encode())
        time.sleep(0.05)


pointer = [0, 0]


def reset_pointer():
    for _ in range(40):
        hmp(f"mouse_move -{STEP} -{STEP}")
        time.sleep(0.02)
    pointer[:] = [0, 0]


def move(x, y):
    while pointer != [x, y]:
        dx = max(-STEP, min(STEP, x - pointer[0]))
        dy = max(-STEP, min(STEP, y - pointer[1]))
        hmp(f"mouse_move {dx} {dy}")
        pointer[0] += dx
        pointer[1] += dy
        time.sleep(0.04)


def click():
    hmp("mouse_button 1")
    time.sleep(0.08)
    hmp("mouse_button 0")
    time.sleep(0.4)


def keys(*names, pause=0.3):
    for name in names:
        hmp(f"sendkey {name}")
        time.sleep(pause)


def shot(name):
    time.sleep(0.8)
    ppm = out / f"{name}.ppm"
    hmp(f'screendump "{ppm}"')
    for _ in range(50):
        if ppm.exists() and ppm.stat().st_size > SCREEN_W * SCREEN_H * 3:
            break
        time.sleep(0.1)
    time.sleep(0.3)
    Image.open(ppm).save(out / f"{name}.png")
    ppm.unlink()
    print(f"deskbar-shots: {out / name}.png", flush=True)


def task_centre(place):
    """Task button `place`'s centre (1 = leftmost), from a fresh screen
    dump: along the buttons' middle row, each run of pixels off the bar's
    fill is a button; the last run is the status button."""
    ppm = out / "probe.ppm"
    hmp(f'screendump "{ppm}"')
    for _ in range(50):
        if ppm.exists() and ppm.stat().st_size > SCREEN_W * SCREEN_H * 3:
            break
        time.sleep(0.1)
    time.sleep(0.3)
    image = Image.open(ppm).convert("RGB")
    ppm.unlink()
    y = SCREEN_H - BAR_H + BUTTON_TOP + BUTTON_H // 2
    fill = image.getpixel((TASK_LEFT, SCREEN_H - BAR_H + BUTTON_TOP // 2))
    runs, start = [], None
    for x in range(TASK_LEFT, SCREEN_W):
        off = image.getpixel((x, y)) != fill
        if off and start is None:
            start = x
        elif not off and start is not None:
            if x - start >= MIN_BUTTON:
                runs.append((start, x))
            start = None
    if start is not None and SCREEN_W - start >= MIN_BUTTON:
        runs.append((start, SCREEN_W))
    buttons = runs[:-1]
    if place > len(buttons):
        raise SystemExit(f"deskbar-shots: no task button {place} (found {len(buttons)})")
    left, right = buttons[place - 1]
    return (left + right) // 2, y


wait_for("ccl-workbench: native window ready")
# The desktop-display injector's last presses (title clicks) end within
# a few seconds of the first Workbench window.
time.sleep(10)
reset_pointer()

# A second Workbench: Development (second row), first entry.
keys("meta_l", "down", "right", "ret")
wait_for("ccl-workbench: native window ready", 2)
# Settings: System (first row), fourth entry (system.ccl key order).
keys("meta_l", "right", "down", "down", "down", "ret")
time.sleep(1.5)
# Files: Tools, the fifth row, first entry (opens, or posts why not).
keys("meta_l", "down", "down", "down", "down", "right", "ret")
time.sleep(3)
shot("deskbar-toast-or-files")
# Settings' button: the first press raises it (Files has focus), the
# second minimizes it, so its button is dimmed.
move(*task_centre(2))
click()
click()
move(600, 300)
shot("deskbar-taskbar")
# Workbench's two windows: its button opens their list.
move(*task_centre(1))
click()
shot("deskbar-group-list")
keys("esc")
move(600, 300)
# The Deskbar panel: Super+N.
keys("meta_l-n")
shot("deskbar-panel")
keys("esc")
# Keyboard focus on the taskbar: Super+T, then Right to the first task.
keys("meta_l-t", "right")
shot("deskbar-keyboard-focus")
keys("esc")
print("deskbar-shots: done", flush=True)
