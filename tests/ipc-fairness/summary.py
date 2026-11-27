#!/usr/bin/env python3
"""Summarize one bench-fairness serial log: every client's FAIRNESS line,
and for the case's small client (big-small: label 2; otherwise the worst
one-thread client) its p99. Usage: summary.py SERIAL_LOG [EXPECTED_CLIENTS].
Exit 1 when a client failed or fewer than EXPECTED_CLIENTS reported."""
import re, statistics, sys
line_re = re.compile(r"FAIRNESS: client (.*)$")
clients = []
failed = False
for raw in open(sys.argv[1], errors="replace"):
    if "FAIRNESS: FAIL" in raw:
        print(raw.strip()); failed = True
    m = line_re.search(raw)
    if m:
        fields = dict(kv.split("=", 1) for kv in m.group(1).split())
        clients.append({k: (int(v) if v.isdigit() else v) for k, v in fields.items()})
expected = int(sys.argv[2]) if len(sys.argv) > 2 else len(clients)
clients.sort(key=lambda c: c["label"])
for c in clients:
    print("FAIRNESS-CLIENT " + " ".join(f"{k}={v}" for k, v in c.items()))
if clients:
    total = sum(c["calls"] for c in clients)
    p99s = [c["p99_us"] for c in clients if isinstance(c["p99_us"], int)]
    small = next((c for c in clients if c["label"] == 2 and c["threads"] == 1), None) \
        if any(c["threads"] > 1 for c in clients) else \
        max(clients, key=lambda c: c["p99_us"] if isinstance(c["p99_us"], int) else -1)
    share = small["calls"] / total if total else 0
    print(f"FAIRNESS-SUMMARY clients={len(clients)} calls={total} "
          f"p99_us_median={statistics.median(p99s) if p99s else '?'} "
          f"p99_us_worst={max(p99s) if p99s else '?'} "
          f"small_label={small['label']} small_p50_us={small['p50_us']} "
          f"small_p99_us={small['p99_us']} small_max_us={small['max_us']} "
          f"small_call_share={share:.3f}")
if failed or len(clients) < expected:
    print(f"FAIRNESS-SUMMARY FAIL: {len(clients)} of {expected} clients reported")
    sys.exit(1)
