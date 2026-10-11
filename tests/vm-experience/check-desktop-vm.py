#!/usr/bin/env python3
"""QEMU VM experience, end to end (headless test desktop-vm).

Drives the real devices of `make -C kernel run-desktop` (virtio-vga plus a
virtio-tablet) from the host, without a GTK window:
  * absolute pointer: QMP input-send-event with abs axes reaches virtio-input,
    Desktop and the virtio-gpu host cursor plane. QEMU's own
    virtio_gpu_update_cursor trace must place the cursor at exactly the
    injected host pixel (minus the arrow's hotspot), and clicks must arrive
    at exactly that pixel (Desktop's "absolute buttons" line);
  * dynamic resolution: a VNC client's SetDesktopSize is what a resized GTK
    window does (both call dpy_set_ui_info), so it drives virtio-gpu's
    VIRTIO_GPU_EVENT_DISPLAY, display.svc, and Desktop's re-layout. The
    scanout must take the new mode (or the fitted one), and absolute input
    must stay exact at the new size.
With CUBIT_VM_GTK=1 (tests/vm-experience/gtk-check.sh: QEMU's GTK display
on a private Xvfb), it first drives the real GTK window: the host pointer
moves and clicks in it and the window is resized (xdotool), exercising
GTK's own absolute input and resize (ui/gtk.c) rather than QMP and VNC.
Arguments: serial-log qmp-socket timeout-seconds vnc-socket trace-log.
Quits QEMU when done; prints VM-EXPERIENCE: PASS or raises."""
import json
import os
import re
import socket
import struct
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AXIS_LAST = 0x7FFF                  # QEMU INPUT_EVENT_ABS_MAX = Absolute_Pointer.Axis_Last
TASKBAR_H = 36                      # desktop main.adb: the arrow is shown over the taskbar
MINIMUM_MODE = (800, 600)           # virtio-gpu MINIMUM_MODE


def require(condition, explanation):
    if not condition:
        raise RuntimeError(explanation)


def arrow_hotspot():
    text = (ROOT / "userspace/services/desktop/desktop_cursors.ads").read_text()
    m = re.search(r"Arrow => \(Width => \d+, Height => \d+, Hotspot_X => (\d+), Hotspot_Y => (\d+)", text)
    require(m is not None, "arrow cursor metadata not found")
    return int(m.group(1)), int(m.group(2))


def host_position(pixel, extent):
    """ui/input.c qemu_input_scale_axis(pixel, 0, extent, 0, AXIS_LAST)."""
    return pixel * AXIS_LAST // extent


serial_path, qmp_path, timeout, vnc_path, trace_path = sys.argv[1:6]
deadline = time.monotonic() + float(timeout) - 5
serial = Path(serial_path)
trace = Path(trace_path)
hot_x, hot_y = arrow_hotspot()


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(marker, what=None, after=0):
    while True:
        text = log_text()
        index = text.find(marker, after)
        if index >= 0:
            return index + len(marker)
        require(time.monotonic() < deadline, f"timed out waiting for {what or marker!r}")
        time.sleep(0.1)


def cursor_events():
    events = []
    if not trace.exists():
        return events
    for line in trace.read_text(errors="replace").splitlines():
        if "virtio_gpu_update_cursor" not in line:
            continue
        m = re.search(r"scanout (\d+), x (-?\d+), y (-?\d+), (\w+), res (0x[0-9a-f]+|\d+)",
                      line)
        if m:
            events.append((int(m.group(1)), int(m.group(2)), int(m.group(3)), m.group(4),
                           int(m.group(5), 0)))
    return events


while not Path(qmp_path).exists():
    require(time.monotonic() < deadline, "QMP socket deadline")
    time.sleep(0.05)

qmp = socket.socket(socket.AF_UNIX)
qmp.settimeout(5)
qmp.connect(qmp_path)
stream = qmp.makefile("rwb")
require("QMP" in json.loads(stream.readline()), "QMP greeting")


def command(name, arguments=None):
    request = {"execute": name}
    if arguments is not None:
        request["arguments"] = arguments
    stream.write(json.dumps(request).encode() + b"\n")
    stream.flush()
    while True:
        response = json.loads(stream.readline())
        require("error" not in response, str(response))
        if "return" in response:
            return response["return"]


command("qmp_capabilities")


def screen_size():
    path = serial.with_suffix(".vm-screen.ppm")
    command("screendump", {"filename": str(path)})
    data = path.read_bytes()
    fields = data.split(maxsplit=3)
    require(fields[0] == b"P6", "screendump is not a PPM")
    return int(fields[1]), int(fields[2])


def move(px, py, size):
    width, height = size
    command("input-send-event", {"events": [
        {"type": "abs", "data": {"axis": "x", "value": host_position(px, width)}},
        {"type": "abs", "data": {"axis": "y", "value": host_position(py, height)}}]})


def button(down):
    command("input-send-event", {"events": [
        {"type": "btn", "data": {"down": down, "button": "left"}}]})


def expect_cursor_at(px, py, what):
    want = (px - hot_x, py - hot_y)
    while True:
        events = cursor_events()
        if events and events[-1][1:3] == want and events[-1][4] != 0:
            return
        require(time.monotonic() < deadline,
                f"{what}: host cursor not at {want} (last traced {events[-1] if events else None})")
        time.sleep(0.05)


def check_pointer(size, label):
    """Absolute motion lands on exactly the injected pixel, on the host plane."""
    width, height = size
    row = height - TASKBAR_H // 3
    for px in (40, width // 2, width - 1 - 40):
        move(px, row, size)
        expect_cursor_at(px, row, f"{label} move to {px},{row}")
    # A click at an exact pixel; Desktop reports where it applied it.
    px = width // 2 + 17
    move(px, row, size)
    expect_cursor_at(px, row, f"{label} click position")
    mark = len(log_text())
    button(True)
    wait_for(f"desktop: absolute buttons 1 at {px},{row}", f"{label} press at {px},{row}", mark)
    button(False)
    wait_for(f"desktop: absolute buttons 0 at {px},{row}", f"{label} release at {px},{row}", mark)
    print(f"vm-experience: {label}: host cursor and clicks exact at {width}x{height}", flush=True)


def set_desktop_size(width, height):
    """What a resized GTK window does: RFB SetDesktopSize -> dpy_set_ui_info."""
    def recv(s, n):
        data = b""
        while len(data) < n:
            chunk = s.recv(n - len(data))
            require(chunk, "VNC connection closed")
            data += chunk
        return data
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.settimeout(5)
        s.connect(vnc_path)
        recv(s, 12)
        s.sendall(b"RFB 003.008\n")
        count = recv(s, 1)[0]
        require(1 in recv(s, count), "VNC offers no None security")
        s.sendall(b"\x01")
        require(struct.unpack(">I", recv(s, 4))[0] == 0, "VNC security failed")
        s.sendall(b"\x01")                      # shared
        recv(s, 4 + 16)                         # size and pixel format
        recv(s, struct.unpack(">I", recv(s, 4))[0])
        # SetDesktopSize (251): width, height, one screen covering it.
        s.sendall(struct.pack(">BxHHBx", 251, width, height, 1) +
                  struct.pack(">IHHHHI", 0, 0, 0, width, height, 0))
        time.sleep(0.5)


def xdotool(*args):
    return subprocess.run(["xdotool", *map(str, args)], capture_output=True, text=True,
                          check=True).stdout


def gtk_window():
    while True:
        found = subprocess.run(["xdotool", "search", "--onlyvisible", "--name", "QEMU"],
                               capture_output=True, text=True).stdout.split()
        if found:
            return found[0]
        require(time.monotonic() < deadline, "GTK window deadline")
        time.sleep(0.2)


def last_cursor_after(count):
    while True:
        events = cursor_events()
        if len(events) > count and events[-1][4] != 0:
            time.sleep(0.3)        # let a burst settle
            return cursor_events()[-1]
        require(time.monotonic() < deadline, "no host cursor update from GTK motion")
        time.sleep(0.05)


def gtk_phase():
    """The person's view, minus eyes: GTK window, host pointer, resize."""
    window = gtk_window()
    mice = command("query-mice")
    current = [m for m in mice if m.get("current")]
    require(current and current[0].get("absolute"),
            f"GTK's current pointer device is not absolute (no tablet): {mice}")
    print(f"vm-experience: gtk: current pointer {current[0]['name']!r} absolute "
          f"(no grab needed)", flush=True)
    # Resize the window: GTK offers its new size (ui_info), Desktop follows.
    mark = len(log_text())
    xdotool("windowsize", window, 1300, 900)
    wait_for("desktop: output 0 re-laid out", "GTK window resize re-layout", mark)
    wait_for("desktop: active outputs= 1", "outputs reopened after GTK resize", mark)
    time.sleep(1.5)
    size = screen_size()
    line = re.findall(r"virtio-gpu: display change head 0 preferred (\d+)x(\d+)", log_text()[mark:])
    require(line, "GTK resize did not reach virtio-gpu")
    preferred = tuple(map(int, line[-1]))
    require(size == preferred, f"guest mode {size} is not the window's {preferred}")
    print(f"vm-experience: gtk: window resized, guest mode follows at {size[0]}x{size[1]}",
          flush=True)
    # Host pointer moves inside the window: at zoom 1:1 the guest cursor
    # moves by exactly the same pixels, on the host cursor plane.
    points = [(150, 200), (650, 520), (1100, 300)]
    seen = []
    for x, y in points:
        before = len(cursor_events())
        xdotool("mousemove", "--window", window, x, y)
        event = last_cursor_after(before)
        seen.append((event[1] + hot_x, event[2] + hot_y))
    offsets = {(x - gx, y - gy) for (x, y), (gx, gy) in zip(points, seen)}
    require(len(offsets) == 1, f"guest pointer drifts from the host pointer: {list(zip(points, seen))}")
    (off_x, off_y), = offsets
    require(off_x == 0 and 0 <= off_y < 100,
            f"unexpected drawing-area origin {off_x},{off_y} in the GTK window")
    print(f"vm-experience: gtk: host pointer and guest cursor coincide "
          f"(drawing area at window 0,{off_y})", flush=True)
    # A click with the host pointer lands at the host pointer.
    x, y = 640, 400
    xdotool("mousemove", "--window", window, x, y)
    time.sleep(0.5)
    mark = len(log_text())
    xdotool("click", 1)
    wait_for(f"desktop: absolute buttons 1 at {x - off_x},{y - off_y}", "GTK click press", mark)
    wait_for(f"desktop: absolute buttons 0 at {x - off_x},{y - off_y}", "GTK click release", mark)
    print(f"vm-experience: gtk: click landed at the host pointer", flush=True)
    return window, off_y


def check_drag(size, label):
    """The hardware pointer follows a window drag (title bar): every step of
    the drag must reach the host cursor plane while Desktop repaints the
    moved window (regression: the repaint path never moved the plane)."""
    # Open the CCL workbench (first Apps entry: Meta, then Return), as
    # check-dual-desktop.py does; the run installs ccl-workbench.app.
    mark = len(log_text())
    wait_for("desktop: menu refresh ready", f"{label}: Apps menu ready")
    move(20, 300, size)                 # off the taskbar: no hover selection
    time.sleep(0.3)
    for attempt in range(3):
        command("send-key", {"keys": [{"type": "qcode", "data": "meta_l"}]})
        time.sleep(0.5)
        command("send-key", {"keys": [{"type": "qcode", "data": "ret"}]})
        until = time.monotonic() + 5
        while "desktop: launch submitted" not in log_text()[mark:] and time.monotonic() < until:
            time.sleep(0.1)
        if "desktop: launch submitted" in log_text()[mark:]:
            break
        command("send-key", {"keys": [{"type": "qcode", "data": "esc"}]})
        time.sleep(0.5)
    wait_for("ccl-workbench: first frame presented", f"{label}: a window to drag", mark)
    time.sleep(0.5)
    windows = [tuple(map(int, m)) for m in re.findall(
        r"desktop: surface\s+(\d+) at\s+(\d+),\s+(\d+) size\s+(\d+) x\s+(\d+)", log_text())]
    width, height = size
    # Ordinary windows only (not a full-screen shell surface).
    windows = [w for w in windows if w[3] < width and w[4] < height - TASKBAR_H]
    require(windows, f"{label}: no window to drag (no 'desktop: surface ... at' line)")
    _, wx, wy, ww, wh = windows[-1]
    grab = (min(wx + ww // 2, width - 60), wy + 8)
    require(grab[1] < height - TASKBAR_H, f"{label}: window title {grab} not on screen")
    move(*grab, size)
    expect_cursor_at(*grab, f"{label} over the title bar")
    mark = len(log_text())
    button(True)
    wait_for("desktop: absolute buttons 1 at", f"{label} press on the title", mark)
    steps = [(grab[0] + 25 * i, grab[1] + 15 * i) for i in range(1, 9)]
    for x, y in steps:
        move(x, y, size)
        expect_cursor_at(x, y, f"{label} drag step to {x},{y}")
    button(False)
    wait_for("desktop: absolute buttons 0 at", f"{label} release", mark)
    require(re.search(r"desktop: ptr hit-down \d+ \d+ 1\b", log_text()[mark:]) is not None or
            "desktop: ptr hit-down" not in log_text()[mark:],
            f"{label}: the press did not start a window move")
    print(f"vm-experience: {label}: host cursor followed {len(steps)} drag steps "
          f"from {grab[0]},{grab[1]}", flush=True)


GTK_WINDOW = None   # (window, drawing-area top) once the GTK phase ran


def resize(requested, expected, label):
    mark = len(log_text())
    if GTK_WINDOW:
        # The GTK window owns the size: its next configure would undo a
        # VNC request. Resize the window so its drawing area is requested.
        window, top = GTK_WINDOW
        xdotool("windowsize", window, requested[0], requested[1] + top)
    else:
        set_desktop_size(*requested)
    wait_for(f"desktop: output 0 re-laid out {expected[0]}x{expected[1]}",
             f"{label}: Desktop re-layout at {expected}", mark)
    wait_for("desktop: active outputs= 1", f"{label}: outputs reopened", mark)
    time.sleep(1.0)
    size = screen_size()
    require(size == expected, f"{label}: scanout is {size}, expected {expected}")
    print(f"vm-experience: {label}: host asked {requested[0]}x{requested[1]}, "
          f"scanout {size[0]}x{size[1]}", flush=True)
    return size


try:
    wait_for("virtio-input: tablet ready", "virtio-input driver")
    wait_for("desktop: internal shell active", "Desktop session")
    wait_for("desktop: asynchronous presentation active", "Desktop presenting")
    time.sleep(2.0)
    if os.environ.get("CUBIT_VM_GTK") == "1":
        GTK_WINDOW = gtk_phase()
        size = resize((1024, 768), (1024, 768), "back to the boot mode")
    size = screen_size()
    require(size == (1024, 768), f"boot scanout {size}")

    # The first absolute report switches the pointer to the host plane.
    move(size[0] // 2, size[1] - TASKBAR_H // 3, size)
    wait_for("desktop: absolute pointer source on output 0", "absolute source")
    wait_for("desktop: pointer absolute (host-pointer cursor planes eligible: yes)",
             "pointer on the host plane")
    check_pointer(size, "boot")
    check_drag(size, "boot drag")

    size = resize((1280, 800), (1280, 800), "grow")
    check_pointer(size, "grown")
    size = resize((640, 480), MINIMUM_MODE, "shrink below minimum")
    check_pointer(size, "minimum")
    size = resize((1024, 768), (1024, 768), "restore")
    check_pointer(size, "restored")
    text = log_text()
    require("desktop: output mode watch unavailable" not in text, "mode watch unavailable")
    require("display: output mode change failed" not in text, "display mode change failed")
    print("VM-EXPERIENCE: PASS", flush=True)
finally:
    try:
        command("quit")
    except Exception:
        pass
