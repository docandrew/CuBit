"""Observe actual CuBit-rendered pixels on two QEMU scanout heads.

No synthetic painting: the guest fixture sends ordinary authorized display IPC.
QMP only observes scanout. This is not a desktop layout or hardware latency test.
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
            line = stream.readline()
            require(line, "QMP disconnected")
            response = json.loads(line)
            require("error" not in response, str(response))
            if "return" in response:
                return response["return"]

    command("qmp_capabilities")
    # Two heads start with different contents; only head 1 changes afterward.
    # Releasing head 0's lease/grant must not release head 1 or change its pixels.
    for phase, second in (("a", (32, 80, 224)),
                          ("b", (48, 208, 80)),
                          ("c", (224, 192, 48))):
        marker = f"DISPLAY-DUAL: phase-{phase}"
        while marker not in (log.read_text(errors="replace") if log.exists() else ""):
            require(time.monotonic() < deadline, f"guest phase {phase} deadline")
            time.sleep(0.02)
        for head, color in enumerate(((224, 48, 32), second)):
            image = log.with_suffix(f".phase-{phase}-head-{head}.ppm")
            command("screendump", {"device": "multi-gpu", "head": head,
                                   "filename": str(image), "format": "ppm"})
            # QEMU emits this simple PPM header; fail closed on other formats.
            magic, dimensions, maximum, pixels = image.read_bytes().split(b"\n", 3)
            require((magic, dimensions, maximum) == (b"P6", b"1024 768", b"255"),
                    f"unexpected head {head} geometry/format")
            require(len(pixels) == 1024 * 768 * 3, "truncated scanout capture")
            for y in range(2, 30):
                for x in range(2, 30):
                    offset = (y * 1024 + x) * 3
                    require(tuple(pixels[offset:offset + 3]) == color,
                            f"wrong pixels phase={phase} head={head} at {x},{y}")
            print(f"PASS native scanout phase={phase} head={head} RGB={color}", flush=True)
