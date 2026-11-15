#!/usr/bin/env python3
"""The headless metricsview test's checks (tests/headless/run.sh): from the
serial log, the Metrics app read Desktop's live latency series through
metrics.svc, read its own series back, and kept its refreshes and input
handling measured. Prints a summary; exits 1 on a failure.

Usage: check_metricsview.py SERIAL_LOG
"""
import re
import sys
from pathlib import Path

data = Path(sys.argv[1]).read_bytes().decode("latin-1")
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


TRACKS = re.compile(r"metricsview: tracks=(\d+)/(\d+) columns=(\d+) input_to_ready samples=(\d+) "
                    r"newest_p99_us=(\d+) queries=(\d+) query_mean_us=(\d+) query_max_us=(\d+) "
                    r"refresh_last_us=(\d+) refresh_max_us=(\d+) refreshes=(\d+) "
                    r"input_to_render_last_us=(\d+) input_to_render_max_us=(\d+) inputs=(\d+) "
                    r"input_cost_last_us=(\d+) input_cost_max_us=(\d+)")
PUSH = re.compile(r"push=(on|off) pushed=(\d+) data_thread=(on|off) data_steps=(\d+) data_last_us=(\d+) "
                  r"data_max_us=(\d+)")
OWN = re.compile(r"metricsview: own series (metricsview\.\w+) n=(\d+) p50_us=(\d+) p99_us=(\d+) max_us=(\d+)")
STAGE = re.compile(r"desktop: latency stage=input_to_ready n=(\d+) p50=(\d+) p90=(\d+) p99=(\d+) max=(\d+)")

reports = [tuple(int(g) for g in m.groups()) for m in TRACKS.finditer(data)]
own_all = {}
for m in OWN.finditer(data):
    own_all[m.group(1)] = tuple(int(g) for g in m.groups()[1:])
own = [own_all["metricsview.input_to_render_us"]] if "metricsview.input_to_render_us" in own_all else []
stages = [tuple(int(g) for g in m.groups()) for m in STAGE.finditer(data)]
check(len(reports) >= 3, f"periodic reports ({len(reports)})")
if reports:
    last = reports[-1]
    resolved, total, columns, samples = last[0], last[1], last[2], last[3]
    check(resolved >= total - 2, f"series resolved: {resolved} of {total} (frames.dropped and its kin appear "
                                 f"only once something drops)")
    check(max(r[3] for r in reports) > 0, "Desktop's input_to_ready samples drawn from the store")
    check(columns > 0, f"columns with data: {columns}")
    check(last[5] > 0, f"store queries: {last[5]}")
    check(last[13] > 0, f"inputs measured to the end of their render: {last[13]}")
    print(f"metricsview: query mean {last[6]} us, max {last[7]} us over {last[5]} calls; "
          f"refresh last {last[8]} us, max {last[9]} us over {last[10]} refreshes; "
          f"input-to-rendered last {last[11]} us, max {last[12]} us over {last[13]} inputs; "
          f"the app's own work per input last {last[14]} us, max {last[15]} us")
pushes = PUSH.findall(data)
if pushes:
    push_on, pushed, thread_on, steps, data_last, data_max = pushes[-1]
    check(push_on == "on" and int(pushed) > 0, f"metrics.svc pushes samples: push={push_on} pushed={pushed}")
    check(thread_on == "on" and int(steps) > 0, f"the data thread steps: {thread_on}, {steps} steps")
    print(f"metricsview: data thread last step {data_last} us, slowest {data_max} us")
else:
    check(False, "push and data-thread fields in the reports")
check(bool(own) and own[-1][0] > 0, "the app's own input_to_render_us series read back from metrics.svc")
for name, (n, p50, p99, top) in sorted(own_all.items()):
    print(f"metricsview: own {name} n={n} p50={p50} us p99={p99} us max={top} us (read back from metrics.svc)")
# The editor's keys end in Esc, which saves the dashboard into the user's
# Dashboards folder (made at the first save) on the boot's NVMe volume.
LOADED = re.compile(r"metricsview: dashboard (from \S+|built-in \(\S+: \w+\))")
SAVE = re.compile(r"metricsview: (saved|not saved) (\S+) bytes=(\d+) result=(\w+)")
check(bool(LOADED.search(data)), "the saved dashboard looked for at launch")
saves = SAVE.findall(data)
check(any(v == "saved" for v, _, _, _ in saves), "an edit ended by Esc saves the dashboard")
for verdict, name, size, result in saves:
    print(f"metricsview: {verdict} {name} ({size} bytes, {result})")
# MET-002's menus, picker and dashboards folder (the keys the headless run
# sends after the editor's): each picked panel's CCL, the notices with the
# hash of the dashboard shown, and the menu and picker inputs measured.
PICKED = re.compile(r"metricsview: panel (\d+) now (\(Panel .*\))")
NOTICE = re.compile(r"metricsview: notice: (.*) \| file=(\S*) source_fnv=(\d+)")
picked = PICKED.findall(data)
check(len(picked) >= 3, f"the picker changed panels ({len(picked)} picks)")
for panel, source in picked[:3]:
    print(f"metricsview: picked panel {panel}: {source[:160]}")
check(any("Metric_Statistic.P99" in s and "Panel_Kind.Stat" in s for _, s in picked),
      "a new panel re-sourced and shown as a number")
notices = NOTICE.findall(data)
for text, name, fnv in notices:
    print(f"metricsview: notice {text!r} (file {name}, fnv {fnv})")
saved = [fnv for text, name, fnv in notices if text.startswith("Saved as headless-copy.ccl")]
opened = [fnv for text, name, fnv in notices if text.startswith("Opened headless-copy.ccl")]
check(bool(saved) and bool(opened) and saved[-1] == opened[-1],
      "Save As then Open gives the same dashboard (equal CCL source hashes)")
refused = [(text, fnv) for text, name, fnv in notices if text.startswith("broken.ccl: ")]
check(bool(refused) and "not a Dashboard" in refused[-1][0] and bool(opened) and refused[-1][1] == opened[-1],
      "a file that does not check is refused with the checker's message; the dashboard stays")
for kind in ("menu", "picker"):
    name = f"metricsview.input_to_render_{kind}_us"
    check(name in own_all and own_all[name][0] > 0, f"{kind} inputs measured to their render ({name})")
if stages:
    print("desktop: input_to_ready (Desktop's reports, all inputs): "
          + "; ".join(f"n={s[0]} p50={s[1]} p99={s[3]} max={s[4]}" for s in stages[-3:]))
sys.exit(1 if failures else 0)
