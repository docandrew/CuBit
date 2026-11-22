#!/usr/bin/env python3
"""Ends the bench-latency QEMU run once the guest prints its final marker.

Usage: stop-on-done.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS

Polls the serial log for "sched-latency: done" (or a FAIL line, or a
kernel panic), then asks QEMU to quit over QMP, so the case takes as long as
the benchmark rather than its whole timeout. Without the marker it does nothing: run.sh's timeout and
marker check decide the result. Always exits 0.
"""
import json
from pathlib import Path
import socket
import sys
import time

DONE = b"sched-latency: done"
FAIL = b"sched-latency: FAIL"
PANIC = b"CUBIT KERNEL PANIC"
POLL_SECONDS = 0.5
SETTLE_SECONDS = 0.5  # let the serial file catch up before quitting

serial, socket_path, timeout = sys.argv[1:]
deadline = time.monotonic() + float(timeout)


def finished():
    try:
        data = Path(serial).read_bytes()
    except OSError:
        return False
    return DONE in data or FAIL in data or PANIC in data


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
    print(f"stop-on-done: could not quit QEMU: {error}", file=sys.stderr)
sys.exit(0)
