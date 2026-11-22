"""Validate opt-in Desktop software-duration evidence, never physical latency."""
import json
import re
import sys
from pathlib import Path
STAGES = {"input_dispatch", "request_dispatch", "scene_draw", "submit_call", "submit_to_completion"}
FIELDS = {"count", "min_us", "max_us", "p50_upper_us", "p99_upper_us", "invalid", "dropped"}

def check(text):
    windows = {stage: [] for stage in STAGES}
    for line in text.splitlines():
        if "COMPOSITOR-TIMING:" not in line:
            continue
        raw = line.split("COMPOSITOR-TIMING:", 1)[1].strip()
        parts = raw.split()
        if len(parts) != 8 or any(not re.fullmatch(r"[a-z0-9_]+=[a-z0-9_]+", p) for p in parts):
            raise ValueError("malformed timing record")
        fields = dict(p.split("=") for p in parts)
        stage = fields.pop("stage", None)
        if stage not in STAGES or fields.keys() != FIELDS:
            raise ValueError("unknown stage, duplicate or missing field")
        if any(not value.isdigit() for value in fields.values()):
            raise ValueError("non-numeric timing sample")
        row = {key: int(value) for key, value in fields.items()}
        if any(value > 2**64-1 for value in row.values()) or row["count"] > 1_000_000:
            raise ValueError("out-of-range timing sample")
        if row["invalid"] or row["dropped"]:
            raise ValueError("clock samples invalid or histogram saturated")
        if not row["count"] or not (row["min_us"] <= row["max_us"] and
            row["min_us"] <= row["p50_upper_us"] <= row["p99_upper_us"]):
            raise ValueError("inconsistent histogram bounds")
        windows[stage].append(row)
    if any(not rows for rows in windows.values()):
        raise ValueError("missing stage evidence")
    return {"units": "microseconds", "scope": "instrumented software wall durations, not photon latency",
            "stages": {stage: {"samples": sum(r["count"] for r in rows),
                "max_us": max(r["max_us"] for r in rows), "windows": rows}
                for stage, rows in sorted(windows.items())}}

if __name__ == "__main__":
    try:
        print(json.dumps(check(Path(sys.argv[1]).read_text()), indent=2))
    except (ValueError, OSError) as error:
        raise SystemExit(f"timing evidence rejected: {error}")
