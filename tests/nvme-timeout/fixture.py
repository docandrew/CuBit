#!/usr/bin/env python3
"""BUG-003 headless fixture: make an NVMe read outlive nvme.drv's deadline.

Usage: fixture.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS MODE RESULT_FILE

Once nvme.drv is serving, throttle the NVMe drive's reads through QMP
(block_set_io_throttle) to THROTTLE_BYTES_PER_SECOND. QEMU's leaky bucket
lets the first read through and holds every later one for at least
(previous read's bytes) / rate, so the next read outlives the driver's
30 s I/O deadline. Then, by MODE:

  recover: lift the throttle once the driver has started recovery (an abort
           settled or a controller reset). The re-issued read must succeed
           and the boot must complete (run.sh checks its markers).
  stuck:   keep the throttle until the driver fails the request with a
           typed outcome ("outcome=timed-out"), then lift it: the next reads
           must succeed (check.py checks the order).

Writes one line per step to RESULT_FILE ("fixture: ..."); exits 1 if a step
never happened before TIMEOUT_SECONDS.
"""
import json
from pathlib import Path
import socket
import sys
import time

POLL_SECONDS = 0.2
# Any read of 300 bytes or more holds the next one past 30 s.
THROTTLE_BYTES_PER_SECOND = 10
DRIVE = "nvme0"

READY = b"NVMe Driver: Ready, entering message loop."
TIMED_OUT = b"NVMe: command timed out:"
RECOVERING = (b"NVMe: resetting controller", b"NVMe: recovered without a reset")
FAILED_TYPED = b"outcome=timed-out"
PANIC = b"CUBIT KERNEL PANIC"

serial, socket_path, timeout, mode, result_path = sys.argv[1:]
if mode not in ("recover", "stuck"):
    sys.exit(f"fixture: unknown mode {mode}")
deadline = time.monotonic() + float(timeout)
result = open(result_path, "w", buffering=1)


def note(text):
    result.write(f"fixture: {text}\n")


def wait_for(*markers):
    while time.monotonic() < deadline:
        try:
            data = Path(serial).read_bytes()
        except OSError:
            data = b""
        if PANIC in data:
            return False
        if any(marker in data for marker in markers):
            return True
        time.sleep(POLL_SECONDS)
    return False


class Monitor:
    def __init__(self, path):
        for _ in range(100):
            try:
                self.connection = socket.socket(socket.AF_UNIX)
                self.connection.connect(path)
                break
            except OSError:
                self.connection.close()
                time.sleep(0.1)
        else:
            raise OSError(f"cannot connect to {path}")
        self.stream = self.connection.makefile("rwb")
        json.loads(self.stream.readline())  # greeting
        self.execute("qmp_capabilities")

    def execute(self, command, **arguments):
        message = {"execute": command}
        if arguments:
            message["arguments"] = arguments
        self.stream.write(json.dumps(message).encode() + b"\n")
        self.stream.flush()
        while True:
            reply = json.loads(self.stream.readline())
            if "event" in reply:
                continue
            if "error" in reply:
                raise RuntimeError(f"{command}: {reply['error']}")
            return reply.get("return")

    def throttle(self, read_bytes_per_second):
        self.execute("block_set_io_throttle", device=DRIVE,
                     bps=0, bps_rd=read_bytes_per_second, bps_wr=0,
                     iops=0, iops_rd=0, iops_wr=0)


def main():
    if not wait_for(READY):
        note("nvme.drv never became ready")
        return 1
    monitor = Monitor(socket_path)
    monitor.throttle(THROTTLE_BYTES_PER_SECOND)
    note(f"reads throttled to {THROTTLE_BYTES_PER_SECOND} B/s")
    if not wait_for(TIMED_OUT):
        note("no command timed out")
        monitor.throttle(0)
        return 1
    note("a command timed out")
    if mode == "recover":
        ok = wait_for(*RECOVERING)
        note("recovery started" if ok else "recovery never started")
    else:
        ok = wait_for(FAILED_TYPED)
        note("the request failed with a typed outcome" if ok
             else "no typed failure")
    monitor.throttle(0)
    note("throttle lifted")
    return 0 if ok else 1


try:
    status = main()
except (OSError, ValueError, RuntimeError) as error:
    note(f"error: {error}")
    status = 1
note(f"done status={status}")
sys.exit(status)
