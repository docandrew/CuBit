"""Exercise the native Desktop through PS/2 input; inspect both real scanouts.

This deliberately does not paint host-side stand-ins or inspect private guest
memory. Timing waits allow asynchronous presentation to settle, not benchmark it.
"""
import json
from pathlib import Path
import socket
import sys
import time


def require(condition, explanation):
    if not condition:
        raise RuntimeError(explanation)


serial, socket_path, timeout = sys.argv[1:]
deadline = time.monotonic() + float(timeout)
log = Path(serial)
while not Path(socket_path).exists():
    require(time.monotonic() < deadline, "QMP socket deadline")
    time.sleep(0.05)

with socket.socket(socket.AF_UNIX) as connection:
    connection.settimeout(3)
    connection.connect(socket_path)
    stream = connection.makefile("rwb")
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

    def hmp(text):
        result = command("human-monitor-command", {"command-line": text})
        require(not result.strip(), f"input rejected: {result}")

    def capture(phase):
        time.sleep(0.8)
        images = []
        for head in range(2):
            path = log.with_suffix(f".{phase}-head-{head}.ppm")
            command("screendump", {"device": "multi-gpu", "head": head,
                                   "filename": str(path), "format": "ppm"})
            magic, dimensions, maximum, pixels = path.read_bytes().split(b"\n", 3)
            require((magic, dimensions, maximum) == (b"P6", b"1024 768", b"255"),
                    "unexpected scanout geometry/format")
            require(len(pixels) == 1024 * 768 * 3, "truncated capture")
            images.append(pixels)
        return images

    def region(image, x, y, w, h):
        return b"".join(image[(row * 1024 + x) * 3:
                              (row * 1024 + x + w) * 3]
                        for row in range(y, y + h))

    pointer = [80, 80]

    def move(x, y):
        while pointer != [x, y]:
            dx = max(-70, min(70, x - pointer[0]))
            dy = max(-70, min(70, y - pointer[1]))
            hmp(f"mouse_move {dx} {dy}")
            pointer[0] += dx
            pointer[1] += dy
            time.sleep(0.06)

    def click():
        hmp("mouse_button 1")
        time.sleep(0.07)
        hmp("mouse_button 0")

    command("qmp_capabilities")
    while True:
        text = log.read_text(errors="replace") if log.exists() else ""
        if ("desktop: active outputs= 2 primary= 0" in text and
                "desktop: asynchronous frame released" in text and
                "ps2: consumer registered" in text):
            break
        require(time.monotonic() < deadline, "native Desktop startup deadline")
        time.sleep(0.05)

    # Park the cursor outside window-restoration comparison regions.
    move(20, 700)
    baseline = capture("baseline")
    require(region(baseline[0], 100, 120, 800, 500) ==
            region(baseline[1], 100, 120, 800, 500), "per-output wallpaper differs")
    require(region(baseline[0], 0, 738, 1024, 30) !=
            region(baseline[1], 0, 738, 1024, 30), "taskbar missing on primary")
    hmp("sendkey meta_l")
    time.sleep(0.3)
    # CCL Workbench is the first Apps entry; exercise an actual native client.
    hmp("sendkey ret")
    while "ccl-workbench: native window ready" not in log.read_text(errors="replace"):
        require(time.monotonic() < deadline, "Workbench launch deadline")
        time.sleep(0.05)
    original = capture("opened")
    require(region(original[0], 100, 120, 550, 260) !=
            region(baseline[0], 100, 120, 550, 260), "Workbench did not open")
    move(180, 90)
    hmp("mouse_button 1")
    move(880, 90)
    spanning = capture("spanning")
    # Window moved 700 pixels; check both halves of its client interior.
    require(region(original[0], 110, 130, 90, 200) ==
            region(spanning[0], 810, 130, 90, 200), "primary window fragment wrong")
    require(region(original[0], 350, 130, 300, 200) ==
            region(spanning[1], 26, 130, 300, 200), "secondary window fragment wrong")
    move(1180, 90)
    hmp("mouse_button 0")
    secondary = capture("secondary")
    require(region(secondary[0], 80, 60, 944, 670) ==
            region(baseline[0], 80, 60, 944, 670), "old window/cursor paint not restored")
    require(region(original[0], 110, 130, 500, 200) ==
            region(secondary[1], 86, 130, 500, 200), "window not fully on secondary")
    # New double-click sequence after the drag's release has aged out.
    time.sleep(0.6)
    click()
    time.sleep(0.09)
    click()
    maximized = capture("maximized")
    require(region(maximized[0], 100, 120, 800, 500) ==
            region(baseline[0], 100, 120, 800, 500), "maximize changed primary")
    require(region(maximized[1], 10, 700, 900, 50) !=
            region(baseline[1], 10, 700, 900, 50), "maximize did not fill secondary")
    move(2027, 12)
    click()
    move(80, 80)
    closed = capture("closed")
    require(closed[1] == baseline[1], "secondary cursor/window artifacts after close")
    print("PASS native Desktop: primary taskbar, split drag, per-monitor maximize, "
          "wallpaper/cursor restoration", flush=True)
