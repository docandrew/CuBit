#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Key presses beside CPU-bound peers reach the app within the input limit.

display-latency with CUBIT_LATENCY_LOAD (bench-input-load.app peers) and
CUBIT_LATENCY_INPUT (ps2: the i8042; usb: QEMU's usb-kbd on xhci.drv).
Desktop's LAT-002 report gives, per interval, the quantiles of two hops of
each press: driver_to_intake (the driver's acquisition to Desktop's intake)
and intake_to_delivery (to the app's receipt). The input limit (as
bench-input's, INPUT_LOADED_MISSES_PERCENT = 10): in every interval the
90th percentiles of the two hops sum to under 1 ms, so at most about one
press in ten takes 1 ms or more from its driver to the app. The driver
named must be the one that published the presses.

usage: check_latency.py SERIAL_LOG ps2|usb
"""
import re
import sys

LIMIT_US = 1000
MINIMUM_PRESSES = 48
STAGE = re.compile(r"desktop: latency stage=(\w+) n=(\d+) p50=(\d+) p90=(\d+) p99=(\d+) max=(\d+)")


def main() -> int:
    if len(sys.argv) != 3 or sys.argv[2] not in ("ps2", "usb"):
        print(__doc__, file=sys.stderr)
        return 2
    text = open(sys.argv[1], "rb").read().decode("ascii", "replace")
    intervals = []
    current = {}
    for line in text.splitlines():
        m = STAGE.search(line)
        if not m:
            continue
        name = m.group(1)
        if name in current:
            intervals.append(current)
            current = {}
        current[name] = (int(m.group(2)), int(m.group(4)), int(m.group(6)))
    if current:
        intervals.append(current)
    presses, worst, failed = 0, 0, False
    for index, item in enumerate(intervals, 1):
        if "driver_to_intake" not in item or "intake_to_delivery" not in item:
            continue
        n, p90a, maxa = item["driver_to_intake"]
        _, p90b, maxb = item["intake_to_delivery"]
        presses += n
        total = p90a + p90b
        worst = max(worst, total)
        print("INPUT-URGENCY-LATENCY: interval=%d presses=%d driver_to_intake_p90=%d "
              "intake_to_delivery_p90=%d sum_p90=%d max=%d limit=%d" % (
                  index, n, p90a, p90b, total, maxa + maxb, LIMIT_US))
        failed = failed or total >= LIMIT_US
    published = (re.findall(r"xhci: .* keys-published=(\d+)", text) if sys.argv[2] == "usb"
                 else ["1"] if "ps2: " in text else [])
    if not published or max(int(p) for p in published) == 0:
        print("INPUT-URGENCY-LATENCY: FAIL %s published no key presses" % sys.argv[2])
        return 1
    if presses < MINIMUM_PRESSES:
        print("INPUT-URGENCY-LATENCY: FAIL only %d presses timed" % presses)
        return 1
    if failed:
        print("INPUT-URGENCY-LATENCY: FAIL input=%s worst_sum_p90=%d us (limit %d)" % (
            sys.argv[2], worst, LIMIT_US))
        return 1
    print("INPUT-URGENCY-LATENCY: PASS input=%s presses=%d worst_sum_p90=%d us (limit %d)" % (
        sys.argv[2], presses, worst, LIMIT_US))
    return 0


if __name__ == "__main__":
    sys.exit(main())
