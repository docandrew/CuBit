#!/usr/bin/env python3
"""Native QEMU browser regression; called only by SERVO_BROWSER_CHECK=1.

This injects real keyboard input through QEMU, then waits for actual Servo
navigation/title callbacks. The controlled page changes its title on DOM input.
No direct engine commands, forged Desktop messages or JavaScript evaluation.
"""
import pathlib
import json
import os
import socket
import sys
import time
from check_resize_pixels import compare, pixels

monitor, serial, capture = map(pathlib.Path, sys.argv[1:4])
started = time.monotonic()
timeline = capture.with_suffix(".timeline.jsonl")
closing = False
cursor_x, cursor_y = 80, 80
settings_uses = 0

def record(event, **details):
    with timeline.open("a") as output:
        output.write(json.dumps({"seconds": round(time.monotonic() - started, 3),
                                 "event": event, **details}) + "\n")

record("fixture-start", pid=os.getpid())


def text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def fail(message):
    record("failure", message=message)
    try:
        command(f'screendump "{capture}"')
        command("quit")  # End only this failed, fixture-owned VM.
    except OSError:
        pass
    raise RuntimeError(message)


def wait(marker, after=0, seconds=30):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        log = text()
        if "USER-MEMORY-FAULT:" in log or "CUBITSHELL: panic" in log or "CUBITSHELL: FAIL" in log:
            fail("native fault recorded; aborting browser input fixture")
        if not closing and "CUBITSHELL: closed" in log:
            fail("browser closed before stability interval completed")
        if log.count(marker) > after:
            record("callback", marker=marker)
            return
        time.sleep(0.05)
    fail(f"missing native callback: {marker!r} after occurrence {after}")


def command(line):
    def prompt(channel):
        response = bytearray()
        while not response.endswith(b"(qemu) "):
            chunk = channel.recv(4096)
            if not chunk:
                raise ConnectionError("QEMU monitor closed before command completion")
            response.extend(chunk)
            if len(response) > 65536:
                raise ConnectionError("oversized QEMU monitor response")
        return response

    with socket.socket(socket.AF_UNIX) as channel:
        channel.settimeout(5)
        channel.connect(str(monitor))
        prompt(channel)
        channel.sendall((line + "\n").encode())
        # The banner, echoed characters and final prompt arrive separately.
        # Only the final prompt confirms completion (including screendump).
        if line != "quit":
            prompt(channel)


def key(name):
    command(f"sendkey {name} 10")
    # Functional TCG gate: allow a full software frame between characters.
    # This deliberately does not establish overload or hardware latency.
    time.sleep(0.3)


def type_text(value):
    names = {":": "shift-semicolon", "/": "slash", ".": "dot", "-": "minus"}
    for char in value:
        key(names.get(char, char))


def move(dx, dy):
    global cursor_x, cursor_y
    while dx or dy:
        sx, sy = max(-60, min(60, dx)), max(-60, min(60, dy))
        command(f"mouse_move {sx} {sy}")
        dx, dy = dx - sx, dy - sy
        cursor_x, cursor_y = cursor_x + sx, cursor_y + sy
        time.sleep(0.15)


def navigate(path):
    marker = f"CUBITSHELL-BROWSER: url {path}"
    previous = text().count(marker)
    loaded = text().count(f"CUBITSHELL-BROWSER: loaded-path {path}")
    key("ctrl-l")
    type_text(f"http://10.0.2.2:18470{path}")
    key("ret")
    wait(marker, previous)
    wait(f"CUBITSHELL-BROWSER: loaded-path {path}", loaded)


def move_to(x, y):
    move(x - cursor_x, y - cursor_y)


def expect(marker, action):
    previous = text().count(marker)
    action()
    wait(marker, previous)


def click_input():
    # Unit-scale fixture: page origin(102,216), input at page(24,136).
    move_to(180, 364)
    command("mouse_button 1")
    time.sleep(0.2)
    command("mouse_button 0")


def click_at(x, y):
    move_to(x, y)
    command("mouse_button 1")
    time.sleep(0.2)
    command("mouse_button 0")


def resize(dx, dy):
    command("mouse_button 1")
    time.sleep(0.2)
    move(dx, dy)
    command("mouse_button 0")


def history(action, path, title):
    previous = text().count(f"CUBITSHELL-BROWSER: url {path}")
    traversed = text().count("CUBITSHELL-BROWSER: history traversal complete")
    action()
    wait(f"CUBITSHELL-BROWSER: url {path}", previous)
    wait("CUBITSHELL-BROWSER: history traversal complete", traversed)
    # Retained history need not load again. Require active DOM handling.
    expect(f"CUBITSHELL-BROWSER: title {title}", lambda: key("esc"))


def open_settings(keyboard=False):
    if keyboard or settings_uses % 2 == 0:
        key("alt-e")
        key("s")
    else:
        click_at(172, 124)  # Edit menu title.
        time.sleep(0.5)
        command(f'screendump "{capture.with_name(capture.stem + "-edit-menu.ppm")}"')
        click_at(200, 190)  # Settings below Select address and a separator.


def menu_new_tab():
    click_at(124, 124)  # File.
    time.sleep(0.5)
    command(f'screendump "{capture.with_name(capture.stem + "-file-menu.ppm")}"')
    click_at(160, 150)  # New tab.


def menu_reload():
    key("alt-v")
    key("r")


def toggle_layout():
    global settings_uses
    settings_uses += 1
    # Settings is modal: background shortcuts must not create a tab.
    expect("CUBITSHELL-BROWSER: settings opened", open_settings)
    before = text().count("CUBITSHELL-BROWSER: tab new")
    key("ctrl-t")
    click_at(124, 192)  # Background new-tab button must also be blocked.
    click_at(352, 396)
    time.sleep(0.5)
    if text().count("CUBITSHELL-BROWSER: tab new") != before:
        fail("Settings leaked Ctrl+T to background chrome")
    command(f'screendump "{capture.with_name(capture.stem + "-settings.ppm")}"')
    if settings_uses % 2:
        key("esc")
    else:
        key("tab")
        key("ret")  # Native Done button via keyboard focus.
    record("settings-modal-pass")


def cycle(number):
    record("cycle-start", cycle=number)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserPointerFocus", click_input)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserTyped:abc", lambda: type_text("abc"))
    expect("CUBITSHELL-BROWSER: settings opened", lambda: open_settings(keyboard=True))
    click_at(628, 472)  # Mouse Done after the keyboard mnemonic must not eat a key.
    time.sleep(0.5)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserTyped:abcd", lambda: type_text("d"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserTyped:abc", lambda: key("backspace"))
    record("menu-modal-keyboard-handoff-pass")
    clicks = text().count("CUBITSHELL-BROWSER: title CuBitBrowserPointerFocus")
    key("f10")
    key("right")
    click_at(180, 364)  # Dismiss the menu over the page input; no click-through.
    time.sleep(0.5)
    if text().count("CUBITSHELL-BROWSER: title CuBitBrowserPointerFocus") != clicks:
        fail("menu outside dismissal clicked through to page")
    record("menubar-input-pass")
    expect("CUBITSHELL-BROWSER: title CuBitBrowserB", lambda: navigate("/browser-b"))
    history(lambda: click_at(132, 156), "/browser-a", "CuBitBrowserRestoredA")
    history(lambda: click_at(210, 156), "/browser-b", "CuBitBrowserRestoredB")
    expect("CUBITSHELL-BROWSER: loaded-path /browser-b", menu_reload)
    move_to(180, 364)
    scrolled = text().count("CUBITSHELL-BROWSER: title CuBitBrowserScrolled")
    # QEMU ui/ui-hmp-cmds.c maps negative dz to WHEEL_DOWN (one notch,
    # independent of magnitude). Verify the page sees positive DOM deltaY.
    expect("CUBITSHELL-BROWSER: title CuBitBrowserWheel:1",
           lambda: command("mouse_move 0 0 -1"))
    wait("CUBITSHELL-BROWSER: title CuBitBrowserScrolled", scrolled)
    # UI.App.Open sets 800x600 as the minimum client size. Enlarge
    # within the 1024x768 output work area, then restore for the next cycle.
    move_to(900, 714)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserResize:854x482", lambda: resize(60, 12))
    move_to(958, 724)  # inside the new border, whose exclusive edge is (960,726)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserResize:800x472", lambda: resize(-58, -10))
    expect("CUBITSHELL-BROWSER: tab new 2", lambda: menu_new_tab() if number % 2 else key("ctrl-t"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserPointerFocus", click_input)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserTyped:abc", lambda: type_text("abc"))
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: key("ctrl-shift-tab"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserRestoredB", lambda: key("esc"))
    expect("CUBITSHELL-BROWSER: tab select 2", lambda: click_at(440, 192))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserRetained:abc", lambda: key("f2"))
    # Same tabs in a side rail. Require active page interaction after remapping.
    expect("CUBITSHELL-BROWSER: title CuBitBrowserResize:608x512", toggle_layout)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserPointerFocus", lambda: click_at(372, 324))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserTyped:abcd", lambda: type_text("d"))
    vertical_capture = capture.with_name(capture.stem + "-vertical.ppm")
    command(f'screendump "{vertical_capture}"')
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: click_at(155, 228))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserRestoredB", lambda: key("esc"))
    expect("CUBITSHELL-BROWSER: tab select 2", lambda: click_at(155, 264))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserRetained:abcd", lambda: key("f2"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserResize:800x472", toggle_layout)
    if number % 2:
        # Closing the inactive tab's child button must not select its parent.
        expect("CUBITSHELL-BROWSER: tab select 1", lambda: click_at(160, 192))
        select_two_before = text().count("CUBITSHELL-BROWSER: tab select 2")
    expect("CUBITSHELL-BROWSER: tab close 2", lambda: click_at(574, 192) if number % 2 else key("ctrl-w"))
    if number % 2 and text().count("CUBITSHELL-BROWSER: tab select 2") != select_two_before:
        fail("close child also selected its inactive parent tab")
    wait("CUBITSHELL-BROWSER: tab parked 2", number - 1)
    record("idle-start", cycle=number)
    time.sleep(3)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserRestoredB", lambda: key("esc"))
    command(f'screendump "{capture}"')
    checked, changed = compare(baseline_pixels, pixels(capture))
    if changed:
        fail(f"resize left {changed}/{checked} stale scanout pixels")
    record("resize-pixels-pass", cycle=number, checked=checked)
    record("cycle-complete", cycle=number)


def extended_features():
    record("features-start")
    def ping_original():
        # Servo suppresses unchanged titles. Produce a fresh DOM title before
        # asking Escape to restore it; an already-restored title is no oracle.
        move_to(400, 400)
        expect("CUBITSHELL-BROWSER: title CuBitBrowserWheel:1",
               lambda: command("mouse_move 0 0 -1"))
        expect("CUBITSHELL-BROWSER: title CuBitBrowserRestoredB", lambda: key("esc"))
    # More than eight live views, with actual horizontal and vertical overflow.
    for index in range(2, 17):
        expect(f"CUBITSHELL-BROWSER: tab new {index}", lambda: key("ctrl-t"))
    command(f'screendump "{capture.with_name(capture.stem + "-overflow.ppm")}"')
    expect("CUBITSHELL-BROWSER: tab select 15", lambda: click_at(850, 192))
    expect("CUBITSHELL-BROWSER: tab select 16", lambda: click_at(884, 192))
    expect("CUBITSHELL-BROWSER: viewport 608x512", toggle_layout)
    expect("CUBITSHELL-BROWSER: tab select 15", lambda: click_at(230, 192))
    expect("CUBITSHELL-BROWSER: tab select 16", lambda: click_at(260, 192))
    expect("CUBITSHELL-BROWSER: viewport 800x472", toggle_layout)
    for index in range(16, 1, -1):
        if index < 16:
            expect(f"CUBITSHELL-BROWSER: tab select {index}", lambda: key("ctrl-shift-tab"))
        parked = text().count(f"CUBITSHELL-BROWSER: tab parked {index}")
        expect(f"CUBITSHELL-BROWSER: tab close {index}", lambda: key("ctrl-w"))
        wait(f"CUBITSHELL-BROWSER: tab parked {index}", parked)
    ping_original()
    record("tabs-overflow-pass", live_tabs=16)
    # Exercise capacity, isolated close, and reuse of a retired window slot.
    for count in range(2, 5):
        ready = text().count("CUBITSHELL-BROWSER: window ready")
        expect("CUBITSHELL-BROWSER: window opened", lambda: key("ctrl-n"))
        wait("CUBITSHELL-BROWSER: window ready", ready)
    command(f'screendump "{capture.with_name(capture.stem + "-windows.ppm")}"')
    expect("CUBITSHELL-BROWSER: window limit", lambda: key("ctrl-n"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    parked = text().count("CUBITSHELL-BROWSER: tab parked 1")
    expect("CUBITSHELL-BROWSER: settings opened", lambda: open_settings(keyboard=True))
    expect("CUBITSHELL-BROWSER: window closed", lambda: click_at(944, 149))
    # The original window's exposed title bar is above the staggered siblings.
    click_at(420, 98)
    ping_original()
    wait("CUBITSHELL-BROWSER: tab parked 1", parked)
    ready = text().count("CUBITSHELL-BROWSER: window ready")
    expect("CUBITSHELL-BROWSER: window opened", lambda: key("ctrl-n"))
    wait("CUBITSHELL-BROWSER: window ready", ready)
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    expect("CUBITSHELL-BROWSER: window closed", lambda: key("ctrl-shift-w"))
    # Title-bar close targets each exposed sibling directly, without relying
    # on border clicks to establish keyboard focus.
    expect("CUBITSHELL-BROWSER: window closed", lambda: click_at(926, 131))
    expect("CUBITSHELL-BROWSER: window closed", lambda: click_at(912, 113))
    click_at(420, 98)
    ping_original()
    record("windows-isolation-reuse-pass", windows=4)


wait("CUBITSHELL: PASS", seconds=120)
wait("CUBITSHELL-BROWSER: fonts PASS")
wait("CUBITSHELL-BROWSER: aligned allocator PASS cycles=32")
wait("CUBITSHELL-BROWSER: frame cancel PASS cycles=2")
wait("ui-app: protected frame published")
# Failed previous fixtures may have left a saved vertical preference.
if "CUBITSHELL-BROWSER: initial viewport 608x512" in text():
    expect("CUBITSHELL-BROWSER: viewport 800x472", toggle_layout)
alive_start = time.monotonic()
stability_seconds = int(os.environ.get("SERVO_BROWSER_STABILITY_SECONDS", "180"))
assert 0 <= stability_seconds <= 3600
record("browser-alive-start", required_seconds=stability_seconds)
baseline = capture.with_name(capture.stem + "-before-resize.ppm")
command(f'screendump "{baseline}"')
baseline_pixels = pixels(baseline)
cycles = 0
while cycles == 0 or time.monotonic() - alive_start < stability_seconds:
    cycles += 1
    cycle(cycles)
alive_seconds = time.monotonic() - alive_start
record("browser-alive-complete", seconds_observed=round(alive_seconds, 3), cycles=cycles)
if os.environ.get("SERVO_BROWSER_FEATURES") == "1":
    extended_features()
# Leave the saved preference vertical, then reopen through the launcher.
expect("CUBITSHELL-BROWSER: title CuBitBrowserResize:608x512", toggle_layout)
closing = True
key("ctrl-w")
wait("CUBITSHELL: closed")
record("closed", cycles=cycles)
previous_pass = text().count("CUBITSHELL: PASS")
previous_viewport = text().count("CUBITSHELL-BROWSER: initial viewport 608x512")
key("meta_l")
for item in ["down", "down", "down", "down", "ret"]:
    key(item)
wait("CUBITSHELL: PASS", previous_pass, seconds=120)
wait("CUBITSHELL-BROWSER: initial viewport 608x512", previous_viewport)
record("preference-reopen-pass")
previous_close = text().count("CUBITSHELL: closed")
key("ctrl-w")
wait("CUBITSHELL: closed", previous_close)
record("reopened-browser-closed")
print(f"SERVO-BROWSER-INPUT: PASS navigation editing pointer-focus DOM-input history reload scroll resize close cycles={cycles} browser_alive_seconds={alive_seconds:.3f}", flush=True)
