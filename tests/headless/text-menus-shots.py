#!/usr/bin/env python3
"""Photograph the menu bars and tooltips in a running desktop-display session
(text-menus work), through that run's HMP monitor socket, once its own
injector is done.

  python3 tests/headless/text-menus-shots.py SERIAL_LOG MONITOR_SOCKET OUT_DIR

Opens a second Workbench, Settings and Files (the same steps, so the same
window places, as deskbar-shots.py), then photographs: Workbench's menu
titles, a Files toolbar tooltip, window-button tooltips (Close, and a greyed
maximize), a Deskbar status icon's tooltip, the status button's tooltip under
keyboard focus, CuBit Text with its menu bar, an open menu and the
palette, and the taskbar's content-sized buttons: several apps with labels
of different lengths, then enough windows that the buttons shrink. Screenshots only: it checks nothing beyond the serial lines it
waits for (a demonstration aid, not a test).
"""
import socket
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])
out.mkdir(parents=True, exist_ok=True)

SCREEN_W, SCREEN_H = 1024, 768
STEP = 70
TOOLTIP_WAIT = 1.2          # past the toolkit's 500 ms delay, with room
# Window places after these launch steps (see deskbar-shots.py's photos).
FILES_COPY_TOOL = (332, 186)
FILES_CLOSE = (1001, 155)
SETTINGS_MAXIMIZE = (794, 91)
STATUS_VOLUME = (948, 742)
TEXT_MINIMIZE = (953, 172)
WORKBENCH2_CLOSE = (1007, 118)
TRACE_MINIMIZE = (885, 190)


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(text, count=1, seconds=60):
    deadline = time.monotonic() + seconds
    while log_text().count(text) < count:
        if time.monotonic() > deadline:
            raise SystemExit(f"text-menus-shots: timed out waiting for {text!r} x{count}")
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
    print(f"text-menus-shots: {out / name}.png", flush=True)


def hover(x, y, name):
    move(x, y)
    time.sleep(TOOLTIP_WAIT)
    shot(name)


wait_for("ccl-workbench: native window ready")
time.sleep(10)
reset_pointer()
move(600, 300)
shot("workbench-menu-titles")

keys("meta_l", "down", "right", "ret")
wait_for("ccl-workbench: native window ready", 2)
keys("meta_l", "right", "down", "down", "down", "ret")
time.sleep(1.5)
keys("meta_l", "down", "down", "down", "down", "right", "ret")
wait_for("files: first frame presented")
time.sleep(3)
hover(*FILES_COPY_TOOL, "tooltip-files-toolbar")
hover(*FILES_CLOSE, "tooltip-window-close")
hover(*SETTINGS_MAXIMIZE, "tooltip-maximize-unavailable")
hover(*STATUS_VOLUME, "tooltip-status-volume")
move(600, 300)
# Keyboard: Super+T, then Right past Apps and the three task buttons.
keys("meta_l-t", "right", "right", "right", "right")
time.sleep(TOOLTIP_WAIT)
shot("tooltip-status-keyboard-focus")
keys("esc")
# CuBit Text: Tools (fifth row), second entry.
keys("meta_l", "down", "down", "down", "down", "right", "down", "ret")
wait_for("text: first frame presented")
time.sleep(3)
shot("text-qemu")
keys("f10")
shot("text-qemu-menu-file")
keys("right")
shot("text-qemu-menu-edit")
keys("esc")
keys("ctrl-shift-p")
shot("text-qemu-palette")
keys("esc")
# Text reaches the bottom of the screen: minimize it so the bar shows.
move(*TEXT_MINIMIZE)
click()
move(600, 300)
shot("taskbar-varied-labels")
# Enough apps that the buttons no longer fit at their content widths. The
# surface table holds seven windows: close the second Workbench, then add
# Desktop trace (minimized: it covers the bar), Devices and Config Inspector.
move(*WORKBENCH2_CLOSE)
click()
time.sleep(1.5)
keys("meta_l", "right", "down", "ret")
time.sleep(3)
move(*TRACE_MINIMIZE)
click()
keys("meta_l", "right", "down", "down", "ret")
time.sleep(3)
keys("meta_l", "right", "down", "down", "down", "down", "ret")
time.sleep(3)
move(600, 300)
shot("taskbar-many-windows")
print("text-menus-shots: done", flush=True)
