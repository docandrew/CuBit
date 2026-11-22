#!/usr/bin/env python3
"""Tabulates sched-latency lines from one or more serial logs or outputs.

Usage: summarize.py LOG [LOG...]

Prints a Markdown table with one row per (workload, load): the median over
the runs that reported that pair of p50, p99, max, missed and bg_rate, and
the range of p99 across runs.
"""
import re
import statistics
import sys

LINE = re.compile(r"sched-latency: workload=(\S+) load=(\S+) samples=(\d+) p50_us=(\S+)"
                  r" p99_us=(\S+) max_us=(\S+) missed=(\d+) bg_rate=(\S+) bg_unit=(\S+)")

pairs = {}
order = []
for path in sys.argv[1:]:
    with open(path, errors="replace") as log:
        for match in LINE.finditer(log.read()):
            key = match.group(1), match.group(2)
            if key not in pairs:
                pairs[key] = []
                order.append(key)
            pairs[key].append([float(match.group(i)) for i in range(3, 9)] + [match.group(9)])

print("| workload | load | runs | p50 µs | p99 µs (range) | max µs | missed | bg_rate |")
print("|---|---|---:|---:|---:|---:|---:|---:|")
for key in sorted(order, key=lambda k: (order.index((k[0], "none")) if (k[0], "none") in order else 0, order.index(k))):
    rows = pairs[key]
    med = [statistics.median(r[i] for r in rows) for i in range(6)]
    p99s = [r[2] for r in rows]
    unit = rows[0][6]
    rate = "-" if unit == "-" else f"{med[5]:,.0f} {unit}"
    spread = f" ({min(p99s):,.0f}–{max(p99s):,.0f})" if len(rows) > 1 else ""
    print(f"| {key[0]} | {key[1]} | {len(rows)} | {med[1]:,.1f} | {med[2]:,.1f}{spread} |"
          f" {med[3]:,.1f} | {med[4]:.0f} | {rate} |")
