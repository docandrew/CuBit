#!/usr/bin/env python3
"""Validate native benchmark evidence before presenting timings (not a CI limit)."""
import argparse
import pathlib
import re
import statistics

OPERATIONS = ("read-sequential", "read-random", "overwrite", "write-vectored", "write-then-flush")
EXPECTED = {(op, size, depth) for op in OPERATIONS for size in (4096, 65536)
            for depth in ((1,) if op == "write-then-flush" else (1, 8))}
ROW = re.compile(
    r"^MEASURE backend=cubit-native operation=(\S+) bytes=(\d+) depth=(\d+) n=(\d+) "
    r"p50_ns=(\d+) p95_ns=(\d+) p99_ns=(\d+) max_ns=(\d+) timed_ns=(\d+) peak_deferred=(\d+)$"
)


def capacity(text):
    # Retain the measured historical baseline format for A/B reports, not an
    # old application ABI. Never aggregate different buffer sizes into one run.
    starts = [line for line in text.splitlines() if line.startswith("TURSO-BENCH: START ")]
    if len(starts) != 1:
        raise ValueError("missing/duplicate benchmark start")
    if "transport=serial-4k-grant " in starts[0]:
        return 4096
    match = re.search(r"transport=serial-grant transfer_bytes=(\d+) ", starts[0])
    if match is None or int(match[1]) == 0 or int(match[1]) % 4096 != 0:
        raise ValueError("invalid transfer capacity")
    return int(match[1])


def parse(text):
    capacity(text)
    lines = text.splitlines()
    starts = [line for line in lines if line.startswith("TURSO-BENCH: START ")]
    if len(starts) != 1 or lines.count("TURSO-BENCH: PASS") != 1:
        raise ValueError("missing/duplicate benchmark boundaries")
    calibration = re.search(r"clock=calibrated-tsc ticks_per_ms=(\d+)", starts[0])
    if calibration is None or int(calibration[1]) == 0:
        raise ValueError("invalid calibration")
    if "TURSO-NATIVE: Ada typed worker publication PASS (filesystem)" not in lines:
        raise ValueError("native persistence regression did not finish")
    rows = {}
    for line in lines:
        if not line.startswith("MEASURE "):
            continue
        match = ROW.fullmatch(line)
        if match is None:
            raise ValueError("malformed measurement")
        op = match[1]
        size, depth, n, p50, p95, p99, maximum, elapsed, deferred = map(int, match.groups()[1:])
        key = op, size, depth
        if key not in EXPECTED or key in rows:
            raise ValueError("unexpected/duplicate phase")
        if n != 64 or not (0 < p50 <= p95 <= p99 == maximum) or elapsed <= 0 or deferred != 0:
            raise ValueError("invalid sample count/quantiles/timing or non-baseline concurrency")
        rows[key] = (p50, p95, p99, elapsed, n * size * 1e9 / elapsed / 1048576)
    if rows.keys() != EXPECTED:
        raise ValueError("incomplete phase set")
    return rows


def vector_layout(text):
    start = next(line for line in text.splitlines() if line.startswith("TURSO-BENCH: START "))
    match = re.search(r"vector_layout=(\S+)", start)
    layout = match[1] if match else "segmented"
    if layout not in ("segmented", "packed"):
        raise ValueError("unknown vector layout")
    return layout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=pathlib.Path, nargs="+")
    args = parser.parse_args()
    texts = [path.read_text() for path in args.serial]
    capacities = {capacity(text) for text in texts}
    if len(capacities) != 1:
        raise ValueError("cannot aggregate different transfer capacities")
    runs = [parse(text) for text in texts]
    layouts = {vector_layout(text) for text in texts}
    if len(layouts) != 1:
        raise ValueError("cannot aggregate different vector layouts")
    print(f"Native CuBit/Turso File adapter; calibrated guest TSC; serial {capacities.pop()}-byte grant path.")
    print(f"Vector layout: {layouts.pop()}.")
    print(f"{len(runs)} run(s); medians of run-level percentiles, not pooled percentiles.")
    print("No Linux comparison, hardware latency guarantee, or power-cut claim.\n")
    print("| Operation | Bytes | Batch | p50 µs | p95 µs | p99 µs | Active MiB/s |")
    print("|---|---:|---:|---:|---:|---:|---:|")
    for key in sorted(EXPECTED):
        p50, p95, p99, _, bandwidth = (
            statistics.median(run[key][index] for run in runs) for index in range(5)
        )
        op, size, depth = key
        print(f"| {op} | {size} | {depth} | {p50 / 1000:.2f} | {p95 / 1000:.2f} | {p99 / 1000:.2f} | {bandwidth:.2f} |")


if __name__ == "__main__":
    main()
