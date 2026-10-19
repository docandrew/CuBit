#!/usr/bin/env python3
"""Photograph the release-polish Desktop behaviour in a running desktop-display
session, through that run's HMP monitor socket, once its own injector is done.

  python3 tests/headless/desktop-polish-shots.py SERIAL_LOG MONITOR_SOCKET OUT_DIR [SECTION...]

Sections (all by default, in this order):
  crowd     open ten more windows (more than the surface table's first eight
            slots, so it grows) and photograph the crowded taskbar
  maximize  maximize the top window: it fills the work area above the bar
  drag      drag a title onto the taskbar (it stops above it), then grab it
            back up
  workbench open each of Workbench's menus (F10, then Right)
  dark      switch to the dark scheme (Settings > Appearance) and photograph
            the chrome, the Deskbar panel and toast, Text, Files, Workbench,
            menus and tooltips
  density2  switch the display to 200% (Settings > Displays; run with
            CUBIT_TEST_VIDEO_SIZE=1920x1080 and DESKTOP_SHOTS_SIZE to match)
            and photograph the Deskbar with a few apps open
  density125  the same at 125%: the Deskbar, the Apps menu and title bars
            with a few apps open (icons drawn 1:1 from the nearest export)
  mnemonics Workbench's File menu opened by a click (no underlines), then
            by F10 (underlines: a keyboard open shows the cues)
  audit     open every app on the image (with CUBIT_TEST_EXTRA_APPS=logs.app
            and a CUBIT_TEST_INIT_PROFILE that starts boot-logs.app,
            teapots.app and logs.app, those too), photograph each window raised, the Apps
            menu, the Deskbar panel and a toast, then switch to the dark
            scheme and photograph them all again
Screenshots only: it checks nothing beyond the serial lines it waits for
(a demonstration aid, not a test).
"""
import re
import socket
import sys
import time
from pathlib import Path

from PIL import Image

serial, monitor, out = Path(sys.argv[1]), sys.argv[2], Path(sys.argv[3])
sections = sys.argv[4:] or ["crowd", "maximize", "drag", "workbench", "dark"]
out.mkdir(parents=True, exist_ok=True)


def deskbar_constant(name):
    """A Desktop_Deskbar_Style geometry constant, read from its source so
    these photographs follow the bar's geometry."""
    source = (Path(__file__).resolve().parents[2] /
              "userspace/services/desktop/desktop_deskbar_style.ads").read_text()
    return int(re.search(rf"^\s*{name}\s*:\s*constant\s*:=\s*(\d+);", source, re.M).group(1))


import os
SCREEN_W, SCREEN_H = (int(v) for v in os.environ.get("DESKTOP_SHOTS_SIZE", "1024x768").split("x"))
BAR_H = deskbar_constant("Bar_Height")
TITLE_GRAB = 12               # a point inside the 28-pixel title bar
STEP = 70
TOOLTIP_WAIT = 1.2            # past the toolkit's 500 ms delay, with room
LAUNCH_WAIT = 3.0


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(text, count=1, seconds=60):
    deadline = time.monotonic() + seconds
    while log_text().count(text) < count:
        if time.monotonic() > deadline:
            raise SystemExit(f"desktop-polish-shots: timed out waiting for {text!r} x{count}")
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


def press():
    hmp("mouse_button 1")
    time.sleep(0.1)


def release():
    hmp("mouse_button 0")
    time.sleep(0.4)


def click():
    press()
    release()


def double_click():
    hmp("mouse_button 1")
    time.sleep(0.05)
    hmp("mouse_button 0")
    time.sleep(0.08)
    hmp("mouse_button 1")
    time.sleep(0.05)
    hmp("mouse_button 0")
    time.sleep(0.6)


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
    print(f"desktop-polish-shots: {out / name}.png", flush=True)


SURFACE = re.compile(r"desktop: surface\s+(\d+) at\s+(\d+),\s*(\d+) size\s+(\d+) x\s+(\d+)")
# CuBit.UI.Window_Chrome: the client area starts below the title and rule.
CLIENT_LEFT, CLIENT_TOP = 1, 34
# Workbench's Open toolbar button, in its client area (ccl_workbench.adb).
WORKBENCH_OPEN = (18, 38)
# A point on a title bar this far left of the window's right edge: past
# the three window buttons.
WORKBENCH_TITLE_RIGHT = 150


DRAG_UP = re.compile(r"desktop: ptr drag-up (\d+) (\d+) (\d+)")


def window(index):
    """A client window's place as Desktop logged it (its creation, then any
    move it ended in): 0 the first, -1 the newest."""
    text = log_text()
    found = SURFACE.findall(text)
    if not found:
        raise SystemExit("desktop-polish-shots: no window placement logged")
    surface, x, y, w, h = (int(v) for v in found[index])
    for moved, mx, my in DRAG_UP.findall(text):
        if int(moved) == surface:
            x, y = int(mx), int(my)
    return x, y, w, h


def last_window():
    return window(-1)


def focus(index):
    """Raise a window by a click on its title."""
    x, y, w, _ = window(index)
    move(x + w // 3, y + TITLE_GRAB)
    click()


# The Apps menu (Super): System is the first row, Development the second,
# Tools the fifth; entries in system.ccl key order.
LAUNCH = {
    "workbench": ("meta_l", "down", "right", "ret"),
    "console": ("meta_l", "down", "right", "down", "ret"),
    "logs": ("meta_l", "right", "ret"),
    "trace": ("meta_l", "right", "down", "ret"),
    "devices": ("meta_l", "right", "down", "down", "ret"),
    "settings": ("meta_l", "right", "down", "down", "down", "ret"),
    "inspector": ("meta_l", "right", "down", "down", "down", "down", "ret"),
    "files": ("meta_l", "down", "down", "down", "down", "right", "ret"),
    "text": ("meta_l", "down", "down", "down", "down", "right", "down", "ret"),
}


def launch(name):
    keys(*LAUNCH[name])
    time.sleep(LAUNCH_WAIT)


wait_for("ccl-workbench: native window ready")
# The desktop-display injector's last presses (title clicks) end within
# a few seconds of the first Workbench window.
time.sleep(10)
reset_pointer()

if "crowd" in sections:
    # Eleven windows and the shell: past the table's first eight slots, so
    # Desktop grows it (and logs that).
    for name in ("workbench", "trace", "devices", "settings", "inspector", "files",
                 "text", "workbench", "workbench", "workbench"):
        launch(name)
    wait_for("desktop: surface table grown to")
    move(600, 300)
    shot("crowded-taskbar")

if "maximize" in sections:
    x, y, w, _ = last_window()
    move(x + w // 3, y + TITLE_GRAB)
    double_click()
    move(600, 300)
    shot("maximized-above-taskbar")
    # Maximized, its title is at the top of the work area: restore it.
    move(SCREEN_W // 3, TITLE_GRAB)
    double_click()
    time.sleep(0.5)

if "drag" in sections:
    x, y, w, _ = last_window()
    grab = (x + w // 3, y + TITLE_GRAB)
    move(*grab)
    press()
    move(grab[0], SCREEN_H - BAR_H // 2)
    release()
    move(600, 200)
    shot("drag-title-onto-taskbar")
    # The title stopped above the bar: grab it there and lift the window.
    move(grab[0], SCREEN_H - BAR_H - TITLE_GRAB - 4)
    press()
    move(grab[0], 200)
    release()
    move(600, 120)
    shot("drag-title-grabbed-back")

def focus_workbench():
    """Raise the first Workbench by its title's right end (Settings, when
    open, covers the left of it)."""
    x, y, w, _ = window(0)
    move(x + w - WORKBENCH_TITLE_RIGHT, y + TITLE_GRAB)
    click()


def workbench_menus(prefix):
    """The first Workbench's menus: F10 opens File, Right walks on."""
    focus_workbench()
    move(600, 700)
    keys("f10")
    shot(f"{prefix}menu-file")
    keys("right")
    shot(f"{prefix}menu-edit")
    keys("right", "right")
    shot(f"{prefix}menu-run")
    keys("esc")


if "workbench" in sections:
    workbench_menus("workbench-")

if "dark" in sections:
    # Settings opens on Appearance with Light focused: Tab to Dark, choose
    # it, Tab on to Apply (Image, Cubie, Slate, Ocean, Fill, Fit, Center).
    launch("settings")
    keys("tab", "spc")
    keys(*(["tab"] * 8), "ret")
    time.sleep(1.5)
    shot("dark-settings-greyed-maximize")
    workbench_menus("dark-workbench-")
    x, y, _, _ = window(0)
    move(x + CLIENT_LEFT + WORKBENCH_OPEN[0], y + CLIENT_TOP + WORKBENCH_OPEN[1])
    time.sleep(TOOLTIP_WAIT)
    shot("dark-workbench-tooltip")
    launch("text")
    wait_for("text: first frame presented")
    shot("dark-text")
    launch("files")
    wait_for("files: first frame presented")
    shot("dark-files")
    move(600, 700)
    keys("meta_l-n")
    shot("dark-deskbar-panel")
    keys("esc")
    # Logs is not on the test image: its launch fails into a toast.
    keys("meta_l", "right", "ret")
    time.sleep(1.0)
    shot("dark-toast")
    # The Apps menu with a category open (keyboard in its submenu).
    keys("meta_l", "down", "right")
    shot("dark-apps-menu")
    keys("esc", "esc")

if "density2" in sections:
    # Displays: Shift+Tab from Light reaches its tab; Tab on to Scale Up
    # (past Apply, Revert, Make Primary, Scale Down); four steps from 100%
    # to 200%; back to Apply.
    launch("settings")
    keys("shift-tab", "ret")
    keys(*(["tab"] * 5))
    keys(*(["ret"] * 4))
    keys(*(["shift-tab"] * 4), "ret")
    time.sleep(3)
    shot("density2-settings")
    launch("text")
    launch("files")
    time.sleep(2)
    shot("density2-taskbar")

# Workbench's menu bar: the File title, in its client area.
WORKBENCH_FILE_MENU = (18, 10)


if "mnemonics" in sections:
    focus_workbench()
    x, y, _, _ = window(0)
    move(x + CLIENT_LEFT + WORKBENCH_FILE_MENU[0], y + CLIENT_TOP + WORKBENCH_FILE_MENU[1])
    click()
    shot("mnemonics-mouse-open")
    keys("esc")
    move(600, 700)
    keys("f10")
    shot("mnemonics-keyboard-open")
    keys("esc")


# Settings > Displays: Shift+Tab from Light reaches its tab; Tab on to
# Scale Up (past Apply, Revert, Make Primary, Scale Down); each Enter is one
# 25% step; back to Apply.
def set_scale(steps):
    launch("settings")
    keys("shift-tab", "ret")
    keys(*(["tab"] * 5))
    keys(*(["ret"] * steps))
    keys(*(["shift-tab"] * 4), "ret")
    time.sleep(3)


if "density125" in sections:
    set_scale(1)
    shot("density125-settings")
    launch("text")
    launch("files")
    time.sleep(2)
    move(SCREEN_W - 40, SCREEN_H // 2)
    shot("density125-taskbar")
    keys("meta_l", "down", "right")
    shot("density125-apps-menu")
    keys("esc", "esc")


def audit_round(prefix):
    # Alt+Tab raises the next window (Desktop's cycleFocus): every window
    # comes to the top once, whatever covers whose title.
    count = len(SURFACE.findall(log_text()))
    move(SCREEN_W - 40, SCREEN_H // 2)
    for index in range(count):
        keys("alt-tab")
        shot(f"{prefix}window-{index:02d}")
    keys("meta_l", "down", "right")
    shot(f"{prefix}apps-menu")
    keys("esc", "esc")
    move(SCREEN_W - 40, SCREEN_H // 2)
    keys("meta_l-n")
    shot(f"{prefix}deskbar-panel")
    keys("esc")


if "audit" in sections:
    for name in ("files", "text", "devices", "inspector", "logs", "trace"):
        launch(name)
    time.sleep(2)
    audit_round("audit-light-")
    # Dark: as in the dark section (Settings opens on Appearance, Light
    # focused: Tab to Dark, choose it, Tab on to Apply).
    launch("settings")
    keys("tab", "spc")
    keys(*(["tab"] * 8), "ret")
    time.sleep(1.5)
    audit_round("audit-dark-")

print("desktop-polish-shots: done", flush=True)
