#!/usr/bin/env python3
"""Validate native diagnostic timings AND its closed database independently."""
import argparse
import importlib.util
import math
import pathlib
import re
import sqlite3
import subprocess
import tempfile

spec = importlib.util.spec_from_file_location("storage_check", pathlib.Path(__file__).with_name("check-storage-disk.py"))
storage_check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(storage_check)

OPERATIONS = ("OpenExisting", "OpenCreate", "Close", "Read", "Write", "Size", "Resize", "Flush")


def parse(text):
    assert text.count("TURSO-SQL: PASS") == 1 and "TEST: FAIL" not in text
    assert "STORAGE: filesystem grant retired" in text
    header = re.findall(r"^TURSO-SQL: start samples=129 ticks_per_ms=(\d+)$", text, re.M)
    assert len(header) == 1 and int(header[0]) > 0
    rate = int(header[0])
    samples, operations = [], {}
    for line in text.splitlines():
        if not line.startswith("TURSO-SQL:"):
            continue
        if line == "TURSO-SQL: PASS" or line == f"TURSO-SQL: start samples=129 ticks_per_ms={rate}":
            continue
        match = re.fullmatch(
            r"TURSO-SQL: sample revision=(\d+) ticks=(\d+) io_ticks=(\d+) reads=(\d+) read_bytes=(\d+) writes=(\d+) write_bytes=(\d+) vectors=(\d+) flushes=(\d+)", line)
        if match:
            values = tuple(map(int, match.groups()))
            revision, ticks, io, reads, read_bytes, writes, write_bytes, vectors, flushes = values
            assert revision == len(samples) + 1
            assert 0 < io <= ticks and writes > 0 and write_bytes > 0 and flushes > 0
            assert vectors <= writes and (reads > 0 or read_bytes == 0)
            samples.append(values)
        else:
            match = re.fullmatch(r"TURSO-SQL: operation=(\w+) calls=(\d+) ticks=(\d+) bytes=(\d+)", line)
            assert match, line
            op = match[1]
            assert op in OPERATIONS and op not in operations
            operations[op] = tuple(map(int, match.groups()[1:]))
    assert len(samples) == 129 and tuple(operations) == OPERATIONS
    assert sum(row[2] for row in samples) == sum(op[1] for op in operations.values())
    for op, calls_index, bytes_index in [("Read", 3, 4), ("Write", 5, 6)]:
        assert operations[op][0] == sum(row[calls_index] for row in samples)
        assert operations[op][2] == sum(row[bytes_index] for row in samples)
    assert operations["Flush"][0] == sum(row[8] for row in samples)
    assert all(operations[op][2] == 0 for op in OPERATIONS if op not in ("Read", "Write"))
    return rate, samples, operations


def report(rate, samples, operations):
    elapsed = sum(row[1] for row in samples)
    io = sum(row[2] for row in samples)
    times = sorted(row[1] * 1000 / rate for row in samples)
    print("Native Store-level profile; 129 commits; not public Config IPC timing.")
    print(f"Commit p50={times[64]:.2f} us p99={times[math.ceil(129 * .99)-1]:.2f} us")
    print(f"Inside filesystem transport: {io / elapsed:.2%} of total commit time.")
    print("| Operation | Calls | Bytes | Total ms |")
    print("|---|---:|---:|---:|")
    for op, (calls, ticks, size) in operations.items():
        print(f"| {op} | {calls} | {size} | {ticks / rate:.3f} |")
    print("Includes scheduler/wait time. Outside-transport time includes SQL, allocator, adapter and instrumentation.")
    print("Not a device latency, public IPC, Linux comparison, or power-cut guarantee.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=pathlib.Path)
    parser.add_argument("disk", type=pathlib.Path)
    args = parser.parse_args()
    measurements = parse(args.serial.read_text())
    subprocess.run(["e2fsck", "-fn", str(args.disk)], check=True)
    with tempfile.TemporaryDirectory(prefix="cubit-sql-profile.") as directory:
        database = pathlib.Path(directory) / "transactions.sqlite"
        subprocess.run(["debugfs", "-R", f"dump /turso-native/transactions.sqlite {database}", str(args.disk)],
                       check=True, capture_output=True, text=True)
        assert database.is_file() and database.stat().st_size > 0
        # Probe closes/checkpoints this DB before printing PASS. An old or
        # incomplete main file fails the exact129revision check (no WAL copied).
        with sqlite3.connect(f"file:{database}?mode=ro", uri=True) as db:
            storage_check.check_tables(db, objects=True, benchmark=True)
    report(*measurements)
    print("TURSO-SQL: independent exact129revision SQLite/ext2 PASS")


if __name__ == "__main__":
    main()
