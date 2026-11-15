#!/usr/bin/env python3
"""LAT-002: summarize Desktop's input-to-photon report lines from a serial
log (Desktop_Latency_Report), merged over every report interval: the
refreshes-late histogram, refreshes late by phase, per-stage quantiles
against their budgets (printed, not enforced) and the per-path frame costs.

Quantiles are the worst interval's histogram bucket upper bounds (the
merged maximum is exact). Prints DISPLAY-LATENCY: PASS when at least one
press was followed to its latch with timing, FAIL otherwise.

usage: display-latency-summary.py SERIAL_LOG
"""
import re
import sys

FIELD = re.compile(r"(\S+)=(\S+)")


def fields(text):
    return dict(FIELD.findall(text))


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    totals = {}
    late = [0] * 5
    phases = {}
    stages = {}
    paths = {}
    inputs_px = {"frames": 0, "px": 0, "max": 0}
    grid = "none"
    period = "0"
    with open(sys.argv[1], "rb") as log:
        for raw in log:
            line = raw.decode("ascii", "replace")
            at = line.find("desktop: latency ")
            if at < 0:
                continue
            body = line[at + len("desktop: latency "):].strip()
            item = fields(body)
            if body.startswith("inputs="):
                for key in ("inputs", "shown", "unshown", "evicted", "untimed", "dropped_frames",
                            "frames_timed", "vblanks", "interval_us"):
                    totals[key] = totals.get(key, 0) + int(item.get(key, "0"))
                late_part = body[body.find("refreshes_late"):]
                for index, key in enumerate(("0", "1", "2", "3", "4+")):
                    match = re.search(r"\b%s=(\d+)" % re.escape(key), late_part)
                    late[index] += int(match.group(1)) if match else 0
                grid = item.get("grid", grid)
                period = item.get("period_us", period)
            elif body.startswith("phase="):
                slot = phases.setdefault(item["phase"], [0, 0, 0, 0])
                for index, key in enumerate(("n", "late0", "late1", "late2+")):
                    slot[index] += int(item.get(key, "0"))
            elif body.startswith("stage="):
                name = item["stage"]
                slot = stages.setdefault(name, {"n": 0, "p50": 0, "p90": 0, "p99": 0, "max": 0,
                                                "budget_us": item.get("budget_us", "none")})
                slot["n"] += int(item["n"])
                for key in ("p50", "p90", "p99", "max"):
                    slot[key] = max(slot[key], int(item[key]))
            elif body.startswith("input_frames="):
                frames = int(item["input_frames"])
                inputs_px["frames"] += frames
                inputs_px["px"] += int(item["px_avg"]) * frames
                inputs_px["max"] = max(inputs_px["max"], int(item["px_max"]))
            elif body.startswith("path="):
                name = item["path"]
                parts = body.split(" render ")
                render = fields(parts[1].split(" submit_to_ready ")[0]) if len(parts) > 1 else {}
                ready = fields(body.split(" submit_to_ready ")[1]) if " submit_to_ready " in body else {}
                slot = paths.setdefault(name, {"frames": 0, "px": 0, "render_p50": 0, "render_p99": 0,
                                               "render_max": 0, "ready_p50": 0, "ready_p99": 0})
                frames = int(item["frames"])
                slot["px"] += int(item["px_avg"]) * frames
                slot["frames"] += frames
                slot["render_p50"] = max(slot["render_p50"], int(render.get("p50", "0")))
                slot["render_p99"] = max(slot["render_p99"], int(render.get("p99", "0")))
                slot["render_max"] = max(slot["render_max"], int(render.get("max", "0")))
                slot["ready_p50"] = max(slot["ready_p50"], int(ready.get("p50", "0")))
                slot["ready_p99"] = max(slot["ready_p99"], int(ready.get("p99", "0")))
    print("DISPLAY-LATENCY: grid=%s period_us=%s %s" % (
        grid, period, " ".join("%s=%d" % kv for kv in sorted(totals.items()))))
    print("DISPLAY-LATENCY: refreshes late 0=%d 1=%d 2=%d 3=%d 4+=%d" % tuple(late))
    for name in sorted(phases, key=lambda p: int(p.split("/")[0])):
        n, l0, l1, l2 = phases[name]
        print("DISPLAY-LATENCY: phase %s n=%d late0=%d late1=%d late2+=%d" % (name, n, l0, l1, l2))
    for name, slot in stages.items():
        budget = slot["budget_us"]
        verdict = ""
        if budget != "none":
            verdict = " p99 %s budget" % ("within" if slot["p99"] <= int(budget) else "OVER")
        print("DISPLAY-LATENCY: stage %-22s n=%d p50<=%d p90<=%d p99<=%d max=%d budget_us=%s%s" % (
            name, slot["n"], slot["p50"], slot["p90"], slot["p99"], slot["max"], budget, verdict))
    for name, slot in paths.items():
        print("DISPLAY-LATENCY: path %s frames=%d px_avg=%d render p50<=%d p99<=%d max=%d "
              "submit_to_ready p50<=%d p99<=%d" % (
                  name, slot["frames"], slot["px"] // max(slot["frames"], 1), slot["render_p50"],
                  slot["render_p99"], slot["render_max"], slot["ready_p50"], slot["ready_p99"]))
    if inputs_px["frames"]:
        print("DISPLAY-LATENCY: input frames=%d px_avg=%d px_max=%d" % (
            inputs_px["frames"], inputs_px["px"] // inputs_px["frames"], inputs_px["max"]))
    timed = sum(late)
    if timed == 0:
        print("DISPLAY-LATENCY: FAIL no press followed to a timed latch")
        return 1
    print("DISPLAY-LATENCY: PASS presses_timed=%d (budgets printed, not enforced)" % timed)
    return 0


if __name__ == "__main__":
    sys.exit(main())
