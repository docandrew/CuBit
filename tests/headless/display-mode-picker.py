#!/usr/bin/env python3
"""Drive Settings > Monitor's mode picker (DISP-016) in a running
virtio-vga session with a monitor socket (the logs test) through that run's HMP monitor socket, check the
serial log, and photograph each step.

  python3 tests/headless/display-mode-picker.py SERIAL_LOG MONITOR_SOCKET OUT_DIR

Steps: open Settings > Monitor (the virtio-gpu head's EDID modes, sorted,
refused ones with their reason); choose the smallest admitted mode and
Apply; leave "Keep this display mode?" unanswered until it
reverts by itself; choose it again, Apply, and Keep (the choice is saved
under the monitor's EDID identity). Linux-hosted QEMU: the virtio-gpu
backend, not Intel hardware.
"""
import re
import socket
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])
out.mkdir(parents=True, exist_ok=True)
# Desktop_Mode_Trial.Confirmation_Window, with room for the outputs to reopen.
CONFIRMATION_SECONDS = 15
LAUNCH_WAIT = 3.0


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(pattern, count=1, seconds=60):
    deadline = time.monotonic() + seconds
    while len(re.findall(pattern, log_text())) < count:
        if time.monotonic() > deadline:
            raise SystemExit(f"display-mode-picker: timed out waiting for {pattern!r} x{count}")
        time.sleep(0.1)
    return re.findall(pattern, log_text())[count - 1]


def hmp(command):
    with socket.socket(socket.AF_UNIX) as s:
        s.settimeout(3)
        s.connect(monitor)
        s.sendall((command + "\n").encode())
        time.sleep(0.05)


def keys(*names, pause=0.3):
    for name in names:
        hmp(f"sendkey {name}")
        time.sleep(pause)


def shot(name):
    time.sleep(1.0)
    ppm = out / f"{name}.ppm"
    if ppm.exists():
        ppm.unlink()
    hmp(f'screendump "{ppm}"')
    size = -1
    for _ in range(100):
        time.sleep(0.1)
        if ppm.exists() and ppm.stat().st_size == size and size > 0:
            break
        size = ppm.stat().st_size if ppm.exists() else -1
    Image.open(ppm).save(out / f"{name}.png")
    ppm.unlink()
    print(f"display-mode-picker: {out / name}.png", flush=True)


wait_for(r"desktop: internal shell active")
# Let the run's own input injector finish.
time.sleep(12)
# The Apps menu: System, then Settings (tests/headless/desktop-polish-shots.py).
keys("meta_l", "right", "down", "down", "down", "ret")
time.sleep(LAUNCH_WAIT)
# Settings opens on Appearance with Light focused: Shift+Tab reaches the
# Monitor tab (the last tab), Enter opens it.
keys("shift-tab", "ret")
shot("1-monitor-page")
# Tab past Apply and Revert to the mode list; End chooses the smallest
# admitted mode (refused rows, like 640 x 480 under the desktop's minimum,
# are skipped).
keys("tab", "tab", "tab", "end")
shot("2-mode-chosen")
# Back to Apply.
keys("shift-tab", "shift-tab", "ret")
trial = wait_for(r"desktop: Settings mode (\S+)(?: \(Auto\))? from (\S+)")
print(f"display-mode-picker: trying {trial}", flush=True)
wait_for(r"desktop: keep this display mode\?")
shot("3-keep-question")
# No answer: it goes back by itself.
wait_for(r"desktop: display mode trial timed out; back to", seconds=CONFIRMATION_SECONDS + 30)
time.sleep(3)
shot("4-reverted")
# The same mode again, kept this time. The question closed with the mode
# list focused and the saved row (Auto) chosen: choose again.
keys("end", "shift-tab", "shift-tab", "ret")
wait_for(r"desktop: keep this display mode\?", count=2)
keys("ret")  # Keep is focused when the question opens.
saved = wait_for(r"desktop: display mode (desktop\.display\.mode\.\S+) (.+) (saved|refused by Config)")
print(f"display-mode-picker: {saved}", flush=True)
time.sleep(2)
shot("5-kept")
print("DISPLAY-MODE-PICKER: DONE", flush=True)
