#!/usr/bin/env python3
"""Observe live compute-control markers against the host monotonic clock.

This catches guest time dilation, not microsecond clock accuracy. Serial
delivery/polling adds uncertainty; a 12-second interval allows a 5% gate.
"""
import argparse
import json
from pathlib import Path
import time
from report import fields


def validate(starts, finishes):
    if len(starts) != 2 or set(starts) != set(finishes):
        return dict(valid=False, workers=[])
    workers = []
    for pid, (guest_start, host_start) in starts.items():
        guest_end, host_end = finishes[pid]
        guest_ms = guest_end - guest_start
        host_ms = (host_end - host_start) / 1_000_000
        ratio = guest_ms / host_ms if host_ms > 0 else 0
        workers.append(dict(pid=pid, guest_ms=guest_ms, host_ms=round(host_ms, 3),
                            ratio=ratio, valid=guest_ms >= 12_000 and 0.95 <= ratio <= 1.05))
    return dict(valid=all(w["valid"] for w in workers), workers=workers)


def observe(path, timeout):
    starts, finishes = {}, {}
    deadline = time.monotonic() + timeout
    stream = None
    pending = ""
    try:
        while time.monotonic() < deadline:
            if stream is None:
                try:
                    stream = path.open(errors="replace")
                except FileNotFoundError:
                    time.sleep(0.01)
                    continue
            pending += stream.read()
            while "\n" in pending:
                line, pending = pending.split("\n", 1)
                stamp = time.monotonic_ns()
                if line.startswith("CPU-CONTROL: START "):
                    f = fields(line)
                    if "pid" not in f or "start_ms" not in f or f["pid"] in starts:
                        raise ValueError("duplicate or invalid start marker")
                    starts[f["pid"]] = (f["start_ms"], stamp)
                elif line.startswith("CPU-CONTROL: COMPLETE "):
                    f = fields(line)
                    if "pid" not in f or "finish_ms" not in f or f["pid"] in finishes:
                        raise ValueError("duplicate or invalid completion marker")
                    finishes[f["pid"]] = (f["finish_ms"], stamp)
            if len(finishes) >= 2:
                return validate(starts, finishes)
            time.sleep(0.01)
        raise ValueError("timed out waiting for both compute-control workers")
    finally:
        if stream:
            stream.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=Path)
    parser.add_argument("--timeout", type=float, default=40)
    args = parser.parse_args()
    try:
        result = observe(args.serial, args.timeout)
    except ValueError as error:
        result = dict(valid=False, error=str(error))
    print(json.dumps(result, indent=2))
    raise SystemExit(0 if result["valid"] else 1)
