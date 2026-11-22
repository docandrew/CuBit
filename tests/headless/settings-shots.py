#!/usr/bin/env python3
"""Photograph Settings (Monitor, Displays, Date & Time, Appearance) in a
running desktop-display session through that run's HMP monitor socket, once
its own injector is done (UI-033 before/after photographs).

  python3 tests/headless/settings-shots.py SERIAL_LOG MONITOR_SOCKET OUT_DIR PREFIX

Writes OUT_DIR/PREFIX-<page>.png. Screenshots only: it checks nothing beyond
the serial line it waits for (a demonstration aid, not a test).
"""
import socket
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out, prefix = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3]), sys.argv[4]
out.mkdir(parents=True, exist_ok=True)
LAUNCH_WAIT = 3.0
KEY_PAUSE = 0.4


def wait_for(text, seconds=180):
    deadline = time.monotonic() + seconds
    while text not in (serial.read_text(errors="replace") if serial.exists() else ""):
        if time.monotonic() > deadline:
            raise SystemExit(f"settings-shots: timed out waiting for {text!r}")
        time.sleep(0.2)


def hmp(command):
    with socket.socket(socket.AF_UNIX) as s:
        s.settimeout(3)
        s.connect(monitor)
        s.sendall((command + "\n").encode())
        time.sleep(0.05)


def keys(*names):
    for name in names:
        hmp(f"sendkey {name}")
        time.sleep(KEY_PAUSE)


def shot(name):
    time.sleep(1.0)
    ppm = out / f"{prefix}-{name}.ppm"
    hmp(f'screendump "{ppm}"')
    for _ in range(100):
        if ppm.exists() and ppm.stat().st_size > 0:
            break
        time.sleep(0.1)
    time.sleep(0.5)
    Image.open(ppm).save(out / f"{prefix}-{name}.png")
    ppm.unlink()


wait_for("ccl-workbench: native window ready")
# The desktop-display injector's last presses end within a few seconds.
time.sleep(12)
# The Apps menu (Super): System is the first row; Settings its fourth entry
# (tests/headless/desktop-polish-shots.py LAUNCH).
keys("meta_l", "right", "down", "down", "down", "ret")
time.sleep(LAUNCH_WAIT)
shot("appearance")
# Settings opens with Alloy Light focused: Shift+Tab reaches the Monitor
# tab; Up and Down on a tab open the page above or below.
keys("shift-tab", "ret")
shot("monitor")
keys("up")
shot("date-time")
keys("up")
shot("displays")
