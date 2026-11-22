#!/usr/bin/env python3
"""Correlate bounded guest input-tail samples with a frozen local CPU trace.

A gap without guest events is NOT evidence of host descheduling by itself.
The guest cannot distinguish host pauses from interrupt-masked kernel work.
"""
import argparse
import json
import re
from pathlib import Path

EVENT = re.compile(
    r"LATENCY-TRACE: tsc=\s*(\d+) pid=\s*(\d+) event=(\w+)"
    r" a=\s*(\d+) b=\s*(\d+)"
)
TAIL = re.compile(
    r"INPUT-TAIL: scenario=(\w+) started=\s*(\d+)"
    r" published=\s*(\d+) finished=\s*(\d+)"
)


def report(text):
    calibration = re.search(r"TIMING: calibration ticks_per_ms=\s*(\d+)", text)
    if calibration is None or int(calibration[1]) == 0:
        raise ValueError("missing positive benchmark calibration")
    rate = int(calibration[1])
    events = [dict(zip(("tsc", "pid", "event", "a", "b"),
                       (int(m[1]), int(m[2]), m[3], int(m[4]), int(m[5]))))
              for m in EVENT.finditer(text)]
    if any(b["tsc"] < a["tsc"] for a, b in zip(events, events[1:])):
        raise ValueError("trace timestamps reversed or multiple CPU dumps")
    gaps = []
    for before, after in zip(events, events[1:]):
        duration_us = (after["tsc"] - before["tsc"]) * 1000 / rate
        if duration_us >= 200:
            gaps.append({"duration_us": duration_us,
                         "before": before, "after": after})
    # Blocking syscall spans include time when other processes execute; do
    # not call their entire wall duration interrupt-disabled execution time.
    entered = {}
    syscalls = []
    for event in events:
        key = (event["pid"], event["a"])
        if event["event"] == "syscall_enter":
            entered[key] = event
        elif event["event"] == "syscall_return" and key in entered:
            start = entered.pop(key)
            duration_us = (event["tsc"] - start["tsc"]) * 1000 / rate
            if duration_us >= 200:
                syscalls.append({"pid": event["pid"], "syscall": event["a"],
                                 "started": start["tsc"], "finished": event["tsc"],
                                 "wall_duration_us": duration_us})
    samples = []
    for m in TAIL.finditer(text):
        start, published, finished = map(int, m.group(2, 3, 4))
        if not start <= published <= finished:
            raise ValueError("input timestamps reversed")
        overlaps = [g for g in gaps if g["before"]["tsc"] < finished
                    and g["after"]["tsc"] > start]
        samples.append({"scenario": m[1], "started": start,
                        "finished": finished,
                        "duration_us": (finished - start) * 1000 / rate,
                        "publish_us": (published - start) * 1000 / rate,
                        "fully_covered_by_trace": bool(events) and
                        events[0]["tsc"] <= start <= finished <= events[-1]["tsc"],
                        "overlapping_event_gaps": overlaps})
    return {"ticks_per_ms": rate, "trace_events": len(events),
            "samples": samples,
            "timer_lateness_us": [e["a"] * 1000 / rate for e in events
                                  if e["event"] == "timer_late"],
            "largest_event_gaps": sorted(gaps, key=lambda g: g["duration_us"],
                                         reverse=True)[:8],
            "long_syscall_spans": syscalls,
            "scope": "local guest CPU only; gaps do not establish host-vs-guest cause"}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=Path)
    args = parser.parse_args()
    print(json.dumps(report(args.serial.read_text(errors="replace")), indent=2))
