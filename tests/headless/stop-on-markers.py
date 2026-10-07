#!/usr/bin/env python3
"""Ends a headless QEMU run once the guest prints one of the given markers.

Usage: stop-on-markers.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS MARKER...

Polls the serial log for any MARKER (or a kernel panic), then asks QEMU to
quit over QMP, so a case takes as long as the guest's work rather than its
whole timeout (as tests/sched-latency/stop-on-done.py does for one case).
Without a marker it does nothing: run.sh's timeout and marker checks decide
the result. Always exits 0.
"""
import json
from pathlib import Path
import socket
import sys
import time

PANIC = b"CUBIT KERNEL PANIC"
POLL_SECONDS = 0.5
SETTLE_SECONDS = 0.5  # let the serial file catch up before quitting

serial, socket_path, timeout, *markers = sys.argv[1:]
ends = [m.encode() for m in markers] + [PANIC]
deadline = time.monotonic() + float(timeout)


def finished():
    try:
        data = Path(serial).read_bytes()
    except OSError:
        return False
    return any(end in data for end in ends)


while not finished():
    if time.monotonic() >= deadline:
        sys.exit(0)
    time.sleep(POLL_SECONDS)
time.sleep(SETTLE_SECONDS)
try:
    with socket.socket(socket.AF_UNIX) as connection:
        connection.settimeout(3)
        connection.connect(socket_path)
        stream = connection.makefile("rwb")
        json.loads(stream.readline())  # greeting
        for command in ("qmp_capabilities", "quit"):
            stream.write(json.dumps({"execute": command}).encode() + b"\n")
            stream.flush()
            stream.readline()
except (OSError, ValueError) as error:
    print(f"stop-on-markers: could not quit QEMU: {error}", file=sys.stderr)
sys.exit(0)
