#!/usr/bin/env python3
"""LAT-002: key presses at swept phases of a 60 Hz refresh, through QEMU's
HMP monitor (QEMU i8042 -> IRQ -> ps2.drv -> Desktop -> the focused app).

Each press is sent PERIOD_US * STRIDE_PERIODS + PERIOD_US / PHASES after
the previous one, so consecutive presses land one eighth of a refresh
later in the guest's refresh interval: any run of PHASES presses covers
every phase once. The guest measures each press's phase itself (Desktop's
"desktop: latency phase=" lines); the host only spaces them. Keys
alternate between a digit and Backspace so the Calculator's entry, and so
each frame's damage, stays small.

usage: display-latency-keys.py MONITOR_SOCKET [COUNT]
"""
import socket
import sys
import time

PERIOD_US = 16_667
PHASES = 8
# Whole refreshes between presses: room for the press, its release and
# the frame, so presses never queue behind each other.
STRIDE_PERIODS = 6
HOLD_MS = 10
KEYS = ("1", "backspace")


def main() -> int:
    if len(sys.argv) not in (2, 3):
        print(__doc__, file=sys.stderr)
        return 2
    count = int(sys.argv[2]) if len(sys.argv) == 3 else 96
    interval = (PERIOD_US * STRIDE_PERIODS + PERIOD_US // PHASES) / 1e6
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as monitor:
        monitor.connect(sys.argv[1])
        monitor.setblocking(False)
        start = time.monotonic()
        for index in range(count):
            due = start + index * interval
            while True:
                left = due - time.monotonic()
                if left <= 0:
                    break
                time.sleep(min(left, 0.002))
            key = KEYS[index % len(KEYS)]
            monitor.sendall(("sendkey %s %d\n" % (key, HOLD_MS)).encode())
            try:
                while monitor.recv(4096):
                    pass
            except BlockingIOError:
                pass
    print("display-latency-keys: sent %d presses, interval %.3f ms" % (count, interval * 1e3))
    return 0


if __name__ == "__main__":
    sys.exit(main())
