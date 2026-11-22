#!/usr/bin/env python3
"""Strict v2 benchmark log reader. No partial-run or pooled-percentile claims."""
from pathlib import Path
import statistics
import sys

RAW = {"read-sequential", "read-random", "overwrite", "write-vectored"}
EXPECTED = {(op, size, depth) for op in RAW for size in (4096, 65536) for depth in (1, 8, 32)}
EXPECTED |= {("write-then-flush", size, 1) for size in (4096, 65536)}
EXPECTED |= {("config-commit-full-sync", 0, 1), ("config-read-warm", 0, 1)}

def fields(line):
    result = {}
    for field in line.split()[1:]:
        key, value = field.split("=", 1)
        if key in result:
            raise ValueError(f"duplicate field {key}")
        result[key] = value
    return result

def parse(text):
    headers = [fields(s) for s in text.splitlines() if s.startswith("BENCH version=")]
    if len(headers) != 1:
        raise ValueError("expected one benchmark header")
    header = headers[0]
    required = dict(version="2", platform="linux", engine="0.8.0-pre.12", build="O2", storage="buffered", cache="warm", synchronization="FULL", raw_buffers="heap-unregistered", batches="fixed")
    if any(header.get(k) != v for k, v in required.items()):
        raise ValueError("incompatible benchmark configuration")
    backend = header["backend"]
    if backend not in ("syscall", "io-uring"):
        raise ValueError("unknown backend")
    lines = text.splitlines()
    if lines[-1:] != [f"BENCH PASS backend={backend}"] or sum(s.startswith("BENCH PASS") for s in lines) != 1:
        raise ValueError("missing final success marker")
    rows = {}
    for line in lines:
        if not line.startswith("MEASURE "):
            continue
        row = fields(line)
        if row.pop("backend") != backend:
            raise ValueError("mixed backends")
        operation = row.pop("operation")
        deferred = row.pop("peak_deferred")
        row = {k: int(v) for k, v in row.items()}
        if set(row) != {"bytes", "depth", "n", "p50_ns", "p95_ns", "p99_ns", "max_ns", "timed_ns"} or any(v < 0 for v in row.values()):
            raise ValueError("invalid numeric fields")
        key = operation, row["bytes"], row["depth"]
        if key not in EXPECTED or key in rows:
            raise ValueError("unknown or duplicate measurement")
        count = 5000 if operation == "config-read-warm" else 1000 if operation == "config-commit-full-sync" else 1024
        if row["n"] != count or row["timed_ns"] <= 0:
            raise ValueError("invalid sample count or duration")
        if not 0 <= row["p50_ns"] <= row["p95_ns"] <= row["p99_ns"] <= row["max_ns"] <= row["timed_ns"]:
            raise ValueError("inconsistent latency distribution")
        if row["bytes"] == 0:
            if deferred != "na":
                raise ValueError("SQL queue depth is not instrumented")
        elif not 0 <= int(deferred) <= row["depth"]:
            raise ValueError("invalid deferred count")
        row["peak_deferred"] = deferred
        rows[key] = row
    if set(rows) != EXPECTED:
        raise ValueError("incomplete phase matrix")
    return backend, rows

def main(paths):
    groups = {}
    for path in paths:
        backend, rows = parse(Path(path).read_text())
        for key, row in rows.items():
            groups.setdefault((backend, *key), []).append(row)
    print("# Linux-hosted Turso baseline\n")
    print("Medians across runs, with run-to-run p99 range; not pooled percentiles or a CuBit comparison.\n")
    print("| Backend | Operation | Bytes | Batch depth | Runs | p50 µs | p99 µs (range) | Active-phase MiB/s |")
    print("|---|---|---:|---:|---:|---:|---:|---:|")
    for (backend, operation, size, depth), rows in sorted(groups.items()):
        p50 = statistics.median(r["p50_ns"] for r in rows)/1000
        p99s = [r["p99_ns"]/1000 for r in rows]
        rate = "—" if not size else f'{statistics.median(r["n"]*size*1e9/r["timed_ns"]/1048576 for r in rows):.1f}'
        print(f"| {backend} | {operation} | {size} | {depth} | {len(rows)} | {p50:.2f} | {statistics.median(p99s):.2f} ({min(p99s):.2f}–{max(p99s):.2f}) | {rate} |")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("usage: summarize-benchmarks.py COMPLETE_LOG...")
    main(sys.argv[1:])
