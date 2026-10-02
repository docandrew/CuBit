"""Validate native stress-source recovery and aggregate Desktop IPC budgets."""
from pathlib import Path
import re
import sys


def check(text):
    expected = re.findall(r"input-stress: resync reports\s+(\d+)", text)
    if len(expected) != 1 or int(expected[0]) < 1:
        raise ValueError("missing or ambiguous publisher resynchronization count")
    fields = ("source_gap", "source_reject", "present_req", "input_req")
    totals = dict.fromkeys(fields, 0)
    rows = re.findall(r"desktop: stats ([^\n]*)", text)
    if not rows:
        raise ValueError("missing Desktop telemetry")
    for row in rows:
        for field in fields:
            values = re.findall(rf"\b{field}=(\d+)\b", row)
            if len(values) != 1:
                raise ValueError(f"missing or ambiguous {field} in Desktop telemetry")
            totals[field] += int(values[0])
    if totals["source_gap"] != int(expected[0]) or totals["source_reject"] != 0:
        raise ValueError(f"input recovery mismatch: expected={expected[0]}, observed={totals}")
    if totals["present_req"] > 20 or totals["input_req"] > 160:
        raise ValueError(f"excessive Workbench IPC: {totals}")
    return totals


if __name__ == "__main__":
    try:
        print(f"input-stream: PASS {check(Path(sys.argv[1]).read_text())}")
    except (OSError, ValueError) as error:
        raise SystemExit(f"input-stream: FAIL {error}")
