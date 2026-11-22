"""Correlate one Desktop incarnation's software frame completions, not photons."""
import json
from pathlib import Path
import sys


def fields(line, marker, expected):
    parts = line.split(marker, 1)[1].split()
    pairs = [p.split("=") for p in parts]
    if any(len(p) != 2 or not p[1].isdigit() for p in pairs):
        raise ValueError("malformed frame trace")
    values = {k: int(v) for k, v in pairs}
    if len(values) != len(parts) or values.keys() != expected:
        raise ValueError("duplicate, missing or unknown trace field")
    if any(v >= 2**64 - 1 for v in values.values()):
        raise ValueError("unavailable or out-of-range trace value")
    return values


def check(text):
    rows, pending, frames, previous = [], 0, set(), {}
    batches = 0
    for line in text.splitlines():
        if "COMPOSITOR-FRAME:" in line:
            row = fields(line, "COMPOSITOR-FRAME:", {"output", "session", "frame", "submit_us", "complete_us"})
            if row["output"] > 1 or not row["session"] or not row["frame"]:
                raise ValueError("invalid output or identity")
            if row["frame"] in frames:
                raise ValueError("repeated frame identity")
            frames.add(row["frame"])
            if row["complete_us"] < row["submit_us"]:
                raise ValueError("reversed clock")
            old = previous.get(row["output"])
            if old and (row["frame"] <= old["frame"] or row["submit_us"] < old["complete_us"]):
                raise ValueError("overlapping or stale per-output submission")
            previous[row["output"]] = row
            rows.append(row)
            pending += 1
            if pending > 64:
                raise ValueError("trace batch exceeds storage bound")
        elif "COMPOSITOR-FRAME-STATS:" in line:
            row = fields(line, "COMPOSITOR-FRAME-STATS:", {"count", "invalid", "dropped"})
            if row["count"] != pending or row["invalid"] or row["dropped"]:
                raise ValueError("incomplete, invalid or saturated trace batch")
            pending = 0
            batches += 1
    if not rows or pending:
        raise ValueError("missing or unterminated trace batch")
    return {"scope": "Desktop submission to observed validated Display completion; not input or photon latency",
            "units": "microseconds", "batches": batches, "records": rows,
            "max_submit_to_completion_us": max(r["complete_us"] - r["submit_us"] for r in rows)}


if __name__ == "__main__":
    try:
        print(json.dumps(check(Path(sys.argv[1]).read_text()), indent=2))
    except (ValueError, OSError) as error:
        raise SystemExit(f"frame trace rejected: {error}")
