"""Native idle-client wake and solid-pixel oracle; QEMU functional test only."""
import json
from pathlib import Path
import re
import socket
import sys
import time
from dpi_pixels import coverage, find_rectangle

serial, socket_path, duration = sys.argv[1:]
log = Path(serial)
deadline = time.monotonic() + float(duration)


def require(ok, reason):
    if not ok:
        raise RuntimeError(reason)


def text():
    value = log.read_text(errors="replace") if log.exists() else ""
    require(not re.search(r"DPI-CLIENT: FAIL|Unhandled Exception|PANIC|USER EXCEPTION", value),
            "native fixture fault")
    return value


def until(predicate, reason):
    while not predicate():
        require(time.monotonic() < deadline, reason)
        time.sleep(0.05)


until(lambda: Path(socket_path).exists(), "QMP unavailable")
with socket.socket(socket.AF_UNIX) as connection:
    connection.settimeout(5)
    connection.connect(socket_path)
    stream = connection.makefile("rwb")
    require("QMP" in json.loads(stream.readline()), "QMP greeting")

    def command(name, arguments=None):
        item = {"execute": name}
        if arguments is not None:
            item["arguments"] = arguments
        stream.write(json.dumps(item).encode() + b"\n")
        stream.flush()
        while True:
            response = json.loads(stream.readline())
            require("error" not in response, str(response))
            if "return" in response:
                return response["return"]

    command("qmp_capabilities")

    def hmp(line):
        require(not command("human-monitor-command", {"command-line": line}).strip(), line)

    pointer = [80, 80]

    def move(x, y):
        while pointer != [x, y]:
            dx, dy = [max(-60, min(60, a - b)) for a, b in zip((x, y), pointer)]
            hmp(f"mouse_move {dx} {dy}")
            pointer[0] += dx
            pointer[1] += dy
            time.sleep(0.06)

    def click(x, y):
        move(x, y)
        hmp("mouse_button 1")
        time.sleep(0.09)
        hmp("mouse_button 0")
        time.sleep(0.2)

    def drag(x1, y1, x2, y2):
        move(x1, y1)
        hmp("mouse_button 1")
        move(x2, y2)
        hmp("mouse_button 0")

    def pixels(head, rgb, width, height, phase, expected_origin=None):
        # Wait for an actual displayed solid rectangle of the required size.
        # A stale lower-density client cannot synthesize the new scale's color.
        path = log.with_suffix(f".{phase}.ppm")
        located = None
        def matches():
            nonlocal located
            command("screendump", {"device": "multi-gpu", "head": head,
                                   "filename": str(path), "format": "ppm"})
            magic, dimensions, maximum, data = path.read_bytes().split(b"\n", 3)
            w, h = map(int, dimensions.split())
            require(magic == b"P6" and maximum == b"255" and len(data) == w*h*3,
                    "invalid screenshot")
            located = find_rectangle(data, w, h, rgb, width, height)
            return located is not None and (expected_origin is None or located == expected_origin)
        until(matches, f"missing {phase} native pixels")
        return located

    def density(n, d, after):
        pattern = rf"DPI-CLIENT: paint=\s*\d+ density=\s*{n}/\s*{d} logical=\s*320x\s*234"
        until(lambda: re.search(pattern, text()[after:]) is not None,
              f"idle client did not repaint at {n}/{d}")

    until(lambda: "DPI-CLIENT: ready" in text() and
          "ui-app: protected frame published" in text(), "initial client publication")
    pixels(0, (176, 32, 64), 320, 234, "initial")
    drag(180, 90, 1204, 90)
    move(80, 600)  # Park the cursor and its shadow outside client pixels.
    source_origin = pixels(1, (176, 32, 64), 320, 234, "secondary-unit")
    # Open compositor Settings on the primary, away from the idle client.
    hmp("sendkey meta_l"); time.sleep(0.3)
    hmp("sendkey up"); time.sleep(0.12)
    hmp("sendkey up"); time.sleep(0.12)
    hmp("sendkey ret"); time.sleep(1)
    click(154, 180)  # Displays tab.
    # Diagram geometry matches the existing 1024+1280 mixed-output fixture.
    click(625, 264)  # Secondary output, well inside its diagram rectangle.
    for n, d, rgb in [(5, 4, (32,176,64)), (3, 2, (32,64,176))]:
        start = len(text())
        click(721, 420)  # Increase scale.
        click(648, 451)  # Apply.
        density(n, d, start)
        x, w = coverage(source_origin[0], 320, n, d)
        y, h = coverage(source_origin[1], 234, n, d)
        pixels(1, rgb, w, h, f"scale-{n}-{d}", (x, y))
        count = text().count("DPI-CLIENT: paint=")
        time.sleep(0.7)
        require(text().count("DPI-CLIENT: paint=") == count,
                "idle client kept painting without input")
    start = len(text())
    drag(1204, 90, 180, 90)
    move(80, 600)
    density(1, 1, start)
    pixels(0, (176,32,64), 320,234, "returned-unit")
    hmp("sendkey esc")
    until(lambda: "DPI-CLIENT: closed" in text(), "client failed to close")
    print("PASS native idle DPI: configure wake, 125/150% pixels, no idle paints, return to unit", flush=True)
