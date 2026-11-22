#!/usr/bin/env python3
"""Summarize PENNY-ALLOC lines of a run-breakdown.py --alloc-timing output.

    alloc_report.py OUT

Per page, platform and step kind: allocations of 1 MiB or more and the time
spent in them (allocate + free + reallocate), summed over a run's steps;
the median over runs with the range."""
import pathlib
import re
import statistics
import sys
from collections import defaultdict

LINE = re.compile(r"PENNY-ALLOC: kind=(\S+) step=\d+ allocations=(\d+) allocate_us=(\d+) "
                  r"frees=(\d+) free_us=(\d+) reallocations=(\d+) reallocate_us=(\d+)")


def main():
    out = pathlib.Path(sys.argv[1])
    runs = defaultdict(lambda: defaultdict(lambda: [0, 0]))
    for directory in sorted(p for p in out.iterdir() if p.is_dir() and re.search(r"-\d+$", p.name)):
        page, platform, run = directory.name.rsplit("-", 2)
        log = directory / ("serial.log" if platform == "cubit" else "stderr.log")
        if not log.exists():
            continue
        for match in LINE.finditer(log.read_text(errors="replace")):
            kind = match.group(1)
            counts = runs[(page, platform, kind)][run]
            counts[0] += int(match.group(2)) + int(match.group(4)) + int(match.group(6))
            counts[1] += int(match.group(3)) + int(match.group(5)) + int(match.group(7))
    print("| page | platform | step kind | large allocations (median, range) | time us (median, range) |")
    print("|---|---|---|---|---|")
    for (page, platform, kind), by_run in sorted(runs.items()):
        calls = [c for c, _ in by_run.values()]
        times = [t for _, t in by_run.values()]
        print(f"| {page} | {platform} | {kind} | {statistics.median(calls):.0f} ({min(calls)}-{max(calls)}) "
              f"| {statistics.median(times):.0f} ({min(times)}-{max(times)}) |")


if __name__ == "__main__":
    main()
