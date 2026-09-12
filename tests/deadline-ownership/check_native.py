#!/usr/bin/env python3
"""Validate the isolated pre-scheduler timer probe, not an input latency SLA."""
import argparse
import json
import re
from pathlib import Path


def report(text, cpus):
    rows = [{k: int(v) for k, v in re.findall(r"(\w+)=\s*(\d+)", line)}
            for line in text.splitlines() if line.startswith("DEADLINE-TIMER:")]
    required = {"cpu", "requested_us", "expired", "cancelled", "stale", "early_vectors",
                "failures", "p99_late_le_us", "max_late_ticks", "ticks_per_us"}
    valid = (cpus in range(1, 5) and len(rows) == cpus and
             {r.get("cpu") for r in rows} == set(range(cpus)) and all(
                 set(r) == required and r["requested_us"] == 200 and
                 r["expired"] == 256 and r["cancelled"] == 128 and r["stale"] == 128 and
                 r["early_vectors"] > 0 and r["failures"] == 0 and
                 1 <= r["ticks_per_us"] <= 1_000_000 and r["p99_late_le_us"] >= 0
                 for r in rows))
    return {"valid": valid, "cpus": rows,
            "scope": "Boot-only 200us one-shot expiry lateness; not loaded scheduling or input latency."}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=Path)
    parser.add_argument("--cpus", type=int, required=True)
    args = parser.parse_args()
    result = report(args.serial.read_text(errors="replace"), args.cpus)
    print(json.dumps(result, indent=2))
    if not result["valid"]:
        raise SystemExit("missing, duplicate, or failed deadline timer probe")
