#!/usr/bin/env python3
"""devmgr's driver stage of a shutdown, from a serial log (PWR-003).

Every driver and service devmgr started must end on its Stop well inside
its grace: none FORCED, NOT_ENDED or EXITED_FAILING, and each within
Maximum_Stop_Ms. Prints each one's stop latency (milliseconds from devmgr's
Stops to the exit devmgr took).

With --services, procmgr's units (apps and services) too: each must be
STOPPED_CLEAN (or ENDED_EARLY, KEPT) within Maximum_Stop_Ms, except those
named with --fixture (a test fixture that ignores Stop or quiesces slowly on
purpose).

Ordering: devmgr's "drivers stopped" report comes before procmgr's own
report, power.svc's S5 request and the stand-in's S5 entry. A log that
ends before the report (the kernel console ring still held it at S5,
LOG-009) is accepted only with --lossy-tail, and only when none of those
later lines is present: then the tail was lost, not reordered.

Usage: check_driver_stops.py [--lossy-tail] [--maximum-stop-ms N]
                             [--services [--fixture NAME]...] SERIAL_LOG
Exit 0: pass; 1: a driver missed Stop, too slow, or misordered; 2: usage.
"""
import argparse
import re
import sys

#  "Well under" devmgr's grace (power.svc's driver_grace_ms, 1000 ms in the
#  shutdown images): a driver that answers Stop ends in a few ms in QEMU.
Maximum_Stop_Ms = 100

PREFIX = "devmgr: shutdown: "
STOPPING = re.compile(r"devmgr: shutdown: stopping (\S+) \(grace (\d+) ms\)")
ENDED = re.compile(
    r"devmgr: shutdown: (\S+) (STOPPED_CLEAN|EXITED_FAILING|FORCED \(ended after its grace\))"
    r"(?: after (\d+) ms)?")
MISSED = re.compile(r"devmgr: shutdown: (\S+) (did not end within its grace|NOT_ENDED|could not be ended)")
REPORT = re.compile(
    r"devmgr: shutdown: drivers stopped(?: in (\d+) ms: (\d+) clean, (\d+) failing, (\d+) forced, (\d+) not ended)?")
UNIT = re.compile(
    r"procmgr: shutdown: (\S+) pid\d+ (STOPPED_CLEAN|EXITED_FAILING|ENDED_EARLY|FORCED|NOT_ENDED|KEPT)"
    r"(?: after (\d+) ms)? \((app|service|power tail), grace (\d+) ms")
#  What may only follow the report: procmgr's own, then power.svc's S5
#  request, then the firmware step (the stand-in's line; acpi.svc prints
#  none before S5).
LATER = ("procmgr: shutdown: stopped in", "power: procmgr stopped the system",
         "power: asking acpi.svc to enter S5", "shutdown-standin: entering S5")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--lossy-tail", action="store_true")
    parser.add_argument("--maximum-stop-ms", type=int, default=Maximum_Stop_Ms)
    parser.add_argument("--services", action="store_true")
    parser.add_argument("--fixture", action="append", default=[])
    parser.add_argument("serial")
    args = parser.parse_args()
    with open(args.serial, "rb") as f:
        lines = f.read().decode("utf-8", "replace").split("\n")

    asked = {}       # name -> grace
    ended = {}       # name -> (outcome, ms or None)
    missed = {}      # name -> what
    report_at = None
    report = None
    later_at = {}
    units = {}       # procmgr's: name -> (outcome, ms or None, stage, grace)
    failures = []
    for number, line in enumerate(lines, 1):
        if m := STOPPING.search(line):
            asked[m.group(1)] = int(m.group(2))
        elif m := ENDED.search(line):
            ended[m.group(1)] = (m.group(2).split()[0], None if m.group(3) is None else int(m.group(3)))
        elif m := MISSED.search(line):
            missed.setdefault(m.group(1), m.group(2))
        elif m := UNIT.search(line):
            units.setdefault(m.group(1), (m.group(2), None if m.group(3) is None else int(m.group(3)),
                                          m.group(4), int(m.group(5))))
        elif (m := REPORT.search(line)) and report_at is None:
            report_at = number
            report = m
        for marker in LATER:
            if marker in line and marker not in later_at:
                later_at[marker] = number

    if not asked:
        print("driver-stop: devmgr stopped no drivers (no 'devmgr: shutdown: stopping' line)", file=sys.stderr)
        return 1

    # Ordering first: it decides whether a missing report is lag or a bug.
    if report_at is None:
        present = [marker for marker in LATER if marker in later_at]
        if present:
            failures.append("ORDER: '%s' without devmgr's 'drivers stopped' before it" % present[0])
        elif args.lossy_tail:
            print("driver-stop: the log ends before devmgr's report and nothing later is present: "
                  "the console tail was lost at S5 (LOG-009), not reordered")
        else:
            failures.append("devmgr's 'drivers stopped' report is missing")
    else:
        for marker, at in later_at.items():
            if at < report_at:
                failures.append("ORDER: '%s' (line %d) before devmgr's report (line %d)" % (marker, at, report_at))

    width = max(len(name) for name in asked)
    print("driver-stop: %d asked to stop" % len(asked))
    for name, grace in asked.items():
        outcome, ms = ended.get(name, (None, None))
        if outcome is None:
            if name in missed:
                text = "MISSED (%s)" % missed[name]
                failures.append("%s: %s" % (name, missed[name]))
            elif report_at is None and args.lossy_tail:
                text = "(its line was in the lost tail)"
            else:
                text = "NO OUTCOME"
                failures.append("%s: no outcome reported" % name)
        else:
            text = outcome + ("" if ms is None else " %5d ms" % ms)
            if outcome != "STOPPED_CLEAN":
                failures.append("%s: %s (grace %d ms)" % (name, outcome, grace))
            elif ms is None:
                pass   # an older devmgr: no latency printed
            elif ms > args.maximum_stop_ms:
                failures.append("%s: stopped in %d ms, over %d ms" % (name, ms, args.maximum_stop_ms))
            if name in missed and outcome == "FORCED":
                text += " (did not end within its grace)"
        print("  %-*s  %s" % (width, name, text))
    if report is not None and report.group(1) is not None:
        print("driver-stop: stage %s ms: %s clean, %s failing, %s forced, %s not ended"
              % report.group(1, 2, 3, 4, 5))
        if int(report.group(4)) or int(report.group(5)) or int(report.group(3)):
            failures.append("devmgr's report: %s failing, %s forced, %s not ended" % report.group(3, 4, 5))

    if args.services:
        if not units:
            failures.append("procmgr stopped no units (no 'procmgr: shutdown: NAME pid' outcome line)")
        else:
            width = max(len(name) for name in units)
            print("driver-stop: procmgr's units")
        for name, (outcome, ms, stage, grace) in units.items():
            allowed = name in args.fixture
            print("  %-*s  %-13s %s  (%s, grace %d ms%s)" % (
                width, name, outcome, "" if ms is None else "%5d ms" % ms, stage, grace,
                ", a fixture: not judged" if allowed else ""))
            if allowed:
                continue
            if outcome not in ("STOPPED_CLEAN", "ENDED_EARLY", "KEPT"):
                failures.append("procmgr's %s: %s (grace %d ms)" % (name, outcome, grace))
            elif ms is not None and ms > args.maximum_stop_ms:
                failures.append("procmgr's %s: stopped in %d ms, over %d ms" % (name, ms, args.maximum_stop_ms))

    for failure in failures:
        print("driver-stop: FAIL " + failure, file=sys.stderr)
    if failures:
        return 1
    print("driver-stop: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
