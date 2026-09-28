#!/usr/bin/env python3
"""Report opt-in NVMe wait attribution, not a production latency benchmark."""
import argparse
import importlib.util
import pathlib
import re

spec = importlib.util.spec_from_file_location("sql_profile", pathlib.Path(__file__).with_name("check-sql-profile.py"))
sql_profile = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sql_profile)

KINDS = ("read", "write", "flush")


def parse(text):
    rate, _, _ = sql_profile.parse(text)
    rows = []
    for line in text.splitlines():
        if not line.startswith("NVME-WAIT:"):
            continue
        match = re.fullmatch(
            r"NVME-WAIT: interval=\s*(\d+) kind=(read|write|flush) commands=\s*(\d+) ticks=\s*(\d+) slow=\s*(\d+) sleeps=\s*(\d+)", line)
        assert match, line
        interval, kind, commands, ticks, slow, sleeps = match.groups()
        interval, commands, ticks, slow, sleeps = map(int, (interval, commands, ticks, slow, sleeps))
        assert interval == len(rows) // 3 + 1 and kind == KINDS[len(rows) % 3]
        assert 0 <= slow <= commands and 0 <= sleeps <= 1000 * slow
        assert (commands == 0) == (ticks == 0)
        assert kind != "flush" or commands == 64
        rows.append((interval, kind, commands, ticks, slow, sleeps))
    assert rows and len(rows) % 3 == 0
    return rate, rows


def report(rate, rows):
    print("NVMe diagnostic intervals: 64 flush commands each; incomplete tail omitted.")
    print("| Interval | Command | Count | Wait ms | Spin exhausted | Sleep calls |")
    print("|---|---|---:|---:|---:|---:|")
    for interval, kind, commands, ticks, slow, sleeps in rows:
        print(f"| {interval} | {kind} | {commands} | {ticks / rate:.3f} | {slow} | {sleeps} |")
    print("Counts include boot and other I/O outside the timed SQL commit loop.")
    print("Wait time includes guest descheduling, not just hardware completion.")
    print("Diagnostic serial output perturbs runs. Do not subtract these totals from SQL timings or claim a production latency bound.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=pathlib.Path)
    args = parser.parse_args()
    report(*parse(args.serial.read_text()))
