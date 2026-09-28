#!/usr/bin/env python3
"""Validate exact native Config samples, then summarize; never accept partial runs."""
import argparse
import math
import pathlib
import re
import statistics

PHASES = ("cached-get", "committed-set", "overlap-get", "overlap-set")
SAMPLES = 64


def parse(text):
    assert "TEST: FAIL" not in text
    assert text.count("TEST: PASS config-objects-benchmark") == 1
    headers = re.findall(
        r"CONFIG-BENCH: start samples=\s*(\d+) ticks_per_ms=\s*(\d+) final_revision=\s*(\d+)", text)
    assert len(headers) == 1
    count, rate, revision = map(int, headers[0])
    assert count == SAMPLES and rate > 0 and revision == 129
    rows = {phase: [] for phase in PHASES}
    seen = []
    before = None
    for line in text.splitlines():
        if not line.startswith("CONFIG-BENCH:"):
            continue
        if line.startswith("CONFIG-BENCH: start"):
            assert re.fullmatch(
                r"CONFIG-BENCH: start samples=\s*\d+ ticks_per_ms=\s*\d+ final_revision=\s*\d+", line)
            continue
        match = re.fullmatch(
            r"CONFIG-BENCH: sample phase=(\S+) index=\s*(\d+) ticks=\s*(\d+)", line)
        if match:
            phase, index, ticks = match.groups()
            assert phase in rows and int(ticks) > 0
            assert int(index) == len(rows[phase]) + 1
            rows[phase].append(int(ticks) * 1000 / rate)
            seen.append(phase)
        else:
            match = re.fullmatch(r"CONFIG-BENCH: reads_before_write_reply=\s*(\d+)", line)
            assert match and before is None, line
            before = int(match[1])
    assert seen == [phase for phase in PHASES for _ in range(SAMPLES)]
    assert before is not None and 0 <= before <= SAMPLES
    return rows, before


def quantile(values, percent):
    return sorted(values)[math.ceil(len(values) * percent / 100) - 1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="+", type=pathlib.Path)
    args = parser.parse_args()
    runs = [parse(path.read_text()) for path in args.logs]
    print("Native public Config IPC (microseconds; median of run-level percentiles)")
    print("| Operation | p50 | p95 | p99 |")
    print("|---|---:|---:|---:|")
    for phase in PHASES:
        values = [statistics.median(quantile(rows[phase], p) for rows, _ in runs)
                  for p in (50, 95, 99)]
        print(f"| {phase} | " + " | ".join(f"{value:.2f}" for value in values) + " |")
    print("Reads received before write reply, per run: " +
          ", ".join(f"{before}/{SAMPLES}" for _, before in runs))
    print("64 samples/phase: p99 is the maximum. Not a latency guarantee.")
    print("Calibrated guest TSC; cross-vCPU agreement assumed. No CPU pinning implied.")
    print("Write measures service-acknowledged Turso commit, not ext2 power-cut atomicity.")
    print("Overlap is client-observed outstanding requests, not proof of server ordering.")
    print("Each log requires a separate SQLite/WAL/ext2 oracle in the headless runner.")


if __name__ == "__main__":
    main()
