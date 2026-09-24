#!/usr/bin/env python3
"""Real guest UI/input regression; screenshots come from QEMU's scanout."""
import json
from pathlib import Path
import socket
import sys
import time

serial, socket_path, timeout = sys.argv[1:]
log = Path(serial)
deadline = time.monotonic() + float(timeout)


def require(condition, reason):
    if not condition:
        raise RuntimeError(reason)


def wait_for(predicate, reason):
    while not predicate():
        require(time.monotonic() < deadline, reason)
        time.sleep(0.05)


wait_for(lambda: Path(socket_path).exists(), "missing QMP socket")
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
            reply = json.loads(stream.readline())
            require("error" not in reply, str(reply))
            if "return" in reply:
                return reply["return"]

    def hmp(text):
        result = command("human-monitor-command", {"command-line": text})
        require(not result.strip(), str(result))

    def key(name):
        hmp("sendkey " + name)
        time.sleep(0.15)

    pointer = [80, 80]

    def move(x, y):
        while pointer != [x, y]:
            dx = max(-70, min(70, x - pointer[0]))
            dy = max(-70, min(70, y - pointer[1]))
            hmp(f"mouse_move {dx} {dy}")
            pointer[0] += dx
            pointer[1] += dy
            time.sleep(0.06)

    def click(x, y):
        move(x, y)
        hmp("mouse_button 1")
        time.sleep(0.1)
        hmp("mouse_button 0")
        move(80, 80)

    def capture(name):
        time.sleep(0.6)
        filename = log.with_suffix(f".{name}.ppm")
        command("screendump", {"filename": str(filename)})
        magic, dimensions, maximum, pixels = filename.read_bytes().split(b"\n", 3)
        require((magic, dimensions, maximum) == (b"P6", b"1024 768", b"255"), "scanout dimensions")
        command("screendump", {"filename": str(filename.with_suffix(".png")), "format": "png"})
        return pixels

    def region(pixels, x, y, width, height):
        return b"".join(pixels[(row * 1024 + x) * 3:(row * 1024 + x + width) * 3]
                        for row in range(y, y + height))

    command("qmp_capabilities")
    wait_for(lambda: log.exists() and "ps2: consumer registered, entering event loop" in log.read_text(errors="replace"),
             "Desktop input ready")
    time.sleep(1)
    # Default Apps selection is Workbench; Up wraps to the new last entry.
    key("meta_l")
    key("up")
    key("ret")
    wait_for(lambda: "config-inspector: native window ready" in log.read_text(errors="replace"),
             "Config Inspector did not launch from Apps")
    initial = capture("initial")
    key("home")
    key("left")
    collapsed = capture("collapsed")
    require(region(initial, 105, 190, 235, 330) != region(collapsed, 105, 190, 235, 330), "collapse did not repaint descendants")
    key("right")
    expanded = capture("expanded")
    require(region(initial, 105, 190, 235, 330) == region(expanded, 105, 190, 235, 330), "expand did not restore tree")
    for _ in range(4):
        key("down")
    selected = capture("selected")
    require(region(selected, 415, 185, 430, 240) != region(initial, 415, 185, 430, 240), "selection did not display a live value")
    count = log.read_text(errors="replace").count("config-inspector: snapshot ready")
    key("f5")
    wait_for(lambda: log.read_text(errors="replace").count("config-inspector: snapshot ready") > count,
             "refresh did not complete")
    refreshed = capture("refreshed")
    require(region(selected, 105, 180, 740, 330) == region(refreshed, 105, 180, 740, 330), "refresh lost selection or expansion")
    key("end")
    scrolled = capture("scrolled")
    require(region(scrolled, 120, 210, 245, 390) != region(initial, 120, 210, 245, 390),
            "End did not reveal the last tree entry")
    key("home")
    home = capture("home")
    require(region(home, 120, 210, 245, 390) == region(initial, 120, 210, 245, 390),
            "Home did not restore the top of the tree")
    # Real PS/2 mouse input: disclosure is distinct from selecting the label.
    click(129, 205)
    mouse_collapsed = capture("mouse-collapsed")
    require(region(mouse_collapsed, 120, 220, 245, 380) != region(home, 120, 220, 245, 380),
            "mouse disclosure did not collapse Machine")
    click(129, 205)
    click(250, 301)
    mouse_selected = capture("mouse-selected")
    require(region(mouse_selected, 415, 185, 430, 240) == region(selected, 415, 185, 430, 240),
            "mouse selection did not read the same live setting")
    count = log.read_text(errors="replace").count("config-inspector: snapshot ready")
    click(153, 140)
    wait_for(lambda: log.read_text(errors="replace").count("config-inspector: snapshot ready") > count,
             "Refresh button did not activate")
    capture("mouse-refreshed")
    print("PASS Config Inspector: Apps launch, tree navigation, live values, keyboard/mouse disclosure and refresh", flush=True)
