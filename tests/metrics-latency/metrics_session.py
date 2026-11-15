#!/usr/bin/env python3
"""The headless metrics-latency test's host side (tests/headless/run.sh;
MET-002). The guest runs the display-latency fixture (Desktop, the
Calculator, key presses at swept refresh phases through QEMU's i8042) with
metrics.svc and ccl-remote. Desktop publishes LAT-002's samples through its
com.cubit-os.stdmetric outlet; this driver pairs with ccl-remote like a LAN
host and reads the series back through CCL (metrics.series, metrics.range,
metrics.window), checking them against Desktop's own "desktop: latency"
report lines on the serial log: the same ledger, two paths.

Usage: metrics_session.py SERIAL_LOG MONITOR_SOCKET TIMEOUT_SECONDS PORT PRESSES
Prints PASS/FAIL lines and the store's view; quits QEMU; exits 1 on any failure.
"""
import re
import socket
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "cubit-remote"))
import cubit_remote as cr  # noqa: E402

serial, monitor, timeout, port, presses = (sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4]),
                                           int(sys.argv[5]))
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
REPORT = re.compile(rb"desktop: latency inputs=\d+ shown=(\d+)")
STAGE = re.compile(rb"desktop: latency stage=(\w+) n=(\d+) p50=\d+ p90=\d+ p99=\d+ max=(\d+)")
#  Seconds kept for reading the store and quitting before run.sh's timeout.
QUERY_RESERVE_S = 40
#  Desktop's report interval (Desktop_Latency_Report), and one more.
SETTLE_S = 6
STAGES = ("driver_to_intake", "intake_to_delivery", "delivery_to_publish", "publish_to_compose",
          "compose_to_submit", "submit_to_ready", "ready_to_latch_slack", "latch_to_observed",
          "input_to_ready", "input_to_latch")
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


def serial_bytes():
    try:
        return Path(serial).read_bytes()
    except OSError:
        return b""


def shown_so_far(data):
    return sum(int(m.group(1)) for m in REPORT.finditer(data))


def stage_totals(data):
    """Per stage: the samples Desktop's report lines counted, and the maximum."""
    totals = {}
    for m in STAGE.finditer(data):
        name = m.group(1).decode()
        count, high = totals.get(name, (0, 0))
        totals[name] = (count + int(m.group(2)), max(high, int(m.group(3))))
    return totals


def quit_qemu():
    try:
        with socket.socket(socket.AF_UNIX) as connection:
            connection.settimeout(3)
            connection.connect(monitor)
            connection.sendall(b"quit\n")
    except OSError as error:
        print(f"metrics-latency: could not quit QEMU: {error}", file=sys.stderr)


def window(host, name, source=""):
    ok, text = cr.evaluate(host, cr.window_expression(name, source, 1, 0))
    return (cr.window_stats(text) if ok else None), text


def main():
    code = None
    while time.monotonic() < deadline:
        data = serial_bytes()
        found = CODE.search(data)
        if found and LISTENING in data:
            code = found.group(1).decode()
            break
        if b"CUBIT KERNEL PANIC" in data:
            break
        time.sleep(0.5)
    check(code is not None, "the guest showed a pairing code and listens")
    if code is None:
        return
    host = cr.Host("guest", "127.0.0.1", port, cr.decode_token_text(code))
    for _ in range(20):
        try:
            cr.hello(host)
            break
        except (OSError, cr.RemoteError):
            time.sleep(0.5)
    else:
        check(False, "hello with the pairing token")
        return

    # Every press shown (Desktop's own count), then one more report.
    while time.monotonic() < deadline - QUERY_RESERVE_S and shown_so_far(serial_bytes()) < presses:
        time.sleep(0.5)
    time.sleep(SETTLE_S)
    data = serial_bytes()
    shown = shown_so_far(data)
    stages = stage_totals(data)
    check(shown > 0, "Desktop's report lines show %d of %d presses" % (shown, presses))

    ok, text = cr.evaluate_whole(host, cr.series_expression("desktop.latency.*", ""))
    listed = cr.parse_list(text) if ok else None
    rows = [dict(zip(cr.SERIES_FIELDS, cr.parse_fields(e)[1])) for e in listed[1]] if listed else []
    names = {row["name"] for row in rows}
    # A stage with no sample (latch_to_observed has none without a hardware
    # latch) sends no Describe, so its series never appears.
    sampled = [s for s in STAGES if stages.get(s, (0, 0))[0] > 0]
    check(ok and all("desktop.latency." + s in names for s in sampled) and
          all("desktop.latency." + s not in names for s in STAGES if s not in sampled) and
          "desktop.latency.refreshes_late" in names,
          "metrics.series lists every sampled stage (%d of %d) and refreshes_late (%d series)"
          % (len(sampled), len(STAGES), len(rows)))
    check(all(row["source"] == "com.cubit.desktop" and row["program"] == "desktop" for row in rows),
          "the series carry Desktop's identity from its launcher (source, program labels)")
    check(all(row["shed"] == 0 and row["evicted"] == 0 for row in rows), "no sample shed or evicted")
    print(cr.render_series(text), flush=True)

    # Each stage: the store's count and maximum against the report lines'.
    for stage in sampled:
        name = "desktop.latency." + stage
        stats, text = window(host, name)
        expected = stages.get(stage, (0, 0))
        check(stats is not None and stats["count"] == expected[0] and stats["max"] == expected[1],
              "%s: store n=%s max=%s, report lines n=%d max=%d" %
              (name, stats and stats["count"], stats and stats["max"], expected[0], expected[1]))
        print("%-44s %s" % (name, cr.render_window(text)), flush=True)

    counted, _ = window(host, "desktop.latency.shown")
    check(counted is not None and counted["sum"] == shown,
          "desktop.latency.shown sums to the report lines' shown (%s, %d)" % (counted and counted["sum"], shown))
    late, _ = window(host, "desktop.latency.refreshes_late")
    check(late is not None and late["count"] > 0, "refreshes late: one sample per timed press")

    # The raw samples and the 1 s windows of input_to_ready agree with the window.
    ready, _ = window(host, "desktop.latency.input_to_ready")
    ok, text = cr.evaluate_whole(host, cr.range_expression("desktop.latency.input_to_ready", "", 1, 0, 0))
    items = cr.parse_list(text) if ok else None
    points = [e for e in items[1] if e.startswith("(Metric_Item.Point")] if items else []
    check(ready is not None and len(points) == ready["count"],
          "metrics.range: %d raw points, the window counts %s" % (len(points), ready and ready["count"]))
    ok, text = cr.evaluate_whole(host, cr.range_expression("desktop.latency.input_to_ready", "", 1, 0, 1000000))
    items = cr.parse_list(text) if ok else None
    rollups = [cr.parse_fields(e)[1][0][1] for e in items[1] if e.startswith("(Metric_Item.Rollup")] if items else []
    check(ready is not None and sum(r[1] for r in rollups) == ready["count"],
          "metrics.range step 1 s: %d windows hold %d samples" % (len(rollups), sum(r[1] for r in rollups)))
    print(cr.render_range(text), flush=True)

    # Frames against refreshes, and the per-path frame series.
    ok, text = cr.evaluate_whole(host, cr.series_expression("desktop.frames.*", ""))
    frames = cr.parse_list(text) if ok else None
    check(frames is not None and len(frames[1]) > 0, "desktop.frames.* series exist")
    print(cr.render_series(text), flush=True)


try:
    main()
finally:
    quit_qemu()
print("metrics-latency: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
