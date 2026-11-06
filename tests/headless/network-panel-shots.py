#!/usr/bin/env python3
"""Drive and photograph the Deskbar's Network section (UI-027) in a running
desktop-display session, through that run's HMP monitor socket.

  python3 tests/headless/network-panel-shots.py SERIAL_LOG MONITOR_SOCKET OUT_DIR

QEMU has one virtio-net function (pci-00-10-0, declared static in
system.ccl) and no Wi-Fi adapter. By keyboard only: Super+N opens the
panel (drawn by the toolkit), Tab moves the toolkit's focus (Mute, the
volume slider, Do not disturb, the wired row), Enter opens the addressing
editor; the address becomes 10.0.2.16/24 and Apply sends it to netmgr. Checks (exit
1 when missing), from the serial log: Desktop showed the wired item and no
Wi-Fi item, netmgr applied the client's static setting and configured
10.0.2.16/24, and Revert put 10.0.2.15/24 back. Photographs: Quick settings
as the panel opens, the editor open, after Apply, after Revert.
"""
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])
out.mkdir(parents=True, exist_ok=True)
SCREEN_W, SCREEN_H = 1024, 768
INTERFACE = "pci-00-10-0"


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(text, count=1, seconds=60):
    deadline = time.monotonic() + seconds
    while log_text().count(text) < count:
        if time.monotonic() > deadline:
            raise SystemExit(f"network-panel-shots: FAIL waiting for {text!r} x{count}")
        time.sleep(0.1)


def hmp(command):
    import socket
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
    print(f"network-panel-shots: {out / name}.png", flush=True)


wait_for("ccl-workbench: native window ready")
wait_for("desktop: status network wired=shown wifi=hidden (no adapter)")
time.sleep(10)   # the desktop-display injector's own presses end first

configured_16 = f"netmgr: {INTERFACE} configured 10.0.2.16/24"
configured_15 = f"netmgr: {INTERFACE} configured 10.0.2.15/24"
before_15 = log_text().count(configured_15)

keys("meta_l-n")                  # the panel; nothing focused yet
shot("quick-settings")
keys("tab", "tab", "tab", "tab")  # Mute, volume, Do not disturb, the wired row
keys("ret")                       # its addressing editor
shot("network-editor")
keys("tab", "tab")                # Addressing (DHCP/Static/Off), then Address
keys("backspace", "backspace", "backspace", "backspace", "backspace")
keys("1", "6", "slash", "2", "4")
keys("tab", "tab", "tab")         # Gateway, DNS, Apply (the CCL line takes no focus)
keys("ret")
wait_for(f"netmgr: {INTERFACE} policy static (applied by a client)")
wait_for(configured_16)
shot("network-applied")
keys("tab", "ret")                # Revert
wait_for(configured_15, before_15 + 1)
shot("network-reverted")
keys("esc", "esc")
print("network-panel-shots: PASS", flush=True)
