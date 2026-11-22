#!/usr/bin/env python3
"""LOG-005: check a CCL_CONSOLE_DEMO=log-burst serial log.

check_serial.py SERIAL_LOG

log-burst-probe writes 1000 lines ("log-burst: line N") at startup, then
4000 more; after each phase the console evaluates
(logs.recent "log-burst-probe"). Each result is a List<LogItem>: records,
typed gaps, the probe's tally. Checked, for each:

- every item is the probe's (one source);
- the tally balances: published = retained + not_retained, and published
  plus what was shed (over budget, by the publisher's ring) is every line
  written so far; the ring's shed count is the one the probe printed;
- phase 1 (under the per-source ceiling of logs.retention, system.ccl):
  nothing was given up (not_retained 0);
- phase 2 (over it): exactly the ceiling is retained, the rest counted as
  not retained;
- the result itself: the first record shown is line 1 (the boot window is
  kept), records are in order, and between two records shown every missing
  line is accounted for by the typed gaps between them (not retained, not
  shown), exactly, when the ring shed nothing; shown records and gap counts
  add up to the tally's published count.
"""
import re
import sys

PER_SOURCE_CEILING = 4096   # system.ccl: (setting "logs.retention" ...)
PHASES = (1000, 5000)       # userspace/apps/log-burst-probe: lines after each phase
REPL = "ccl-console: REPL completed: List<LogItem>: "
PHASE_DONE = re.compile(r"log-burst-probe: phase (\d) done, (\d+) lines, (\d+) shed by its ring")
ENTRY = re.compile(r'\A\(LogItem\.Logged \(LogEntry (\d+) Severity\.(\w+) (\d+) "((?:[^"\\]|\\.)*)"\)\)\Z', re.S)
GAP = re.compile(r'\A\(LogItem\.Gap \(LogGap \d+ (\d+) (\d+) GapReason\.(\w+) "[^"]*"\)\)\Z')
TALLY = re.compile(r'\A\(LogItem\.Tally \(LogTally (\d+) (\d+) (\d+) (\d+) (\d+) (\d+) "[^"]*"\)\)\Z')
LINE = re.compile(r'\Alog-burst: line (\d+)\Z')


def fail(message):
    print(f"log-burst: FAIL {message}", file=sys.stderr)
    sys.exit(1)


def elements(body):
    """The top-level parenthesized elements of a list literal's body."""
    items, depth, start, quoted, escaped = [], 0, None, False, False
    for i, c in enumerate(body):
        if quoted:
            if escaped:
                escaped = False
            elif c == "\\":
                escaped = True
            elif c == '"':
                quoted = False
        elif c == '"':
            quoted = True
        elif c == "(":
            if depth == 0:
                start = i
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                items.append(body[start:i + 1])
    return items


def check(phase, result, lines, shed):
    if not (result.startswith("[") and result.endswith("]")):
        fail(f"phase {phase}: not a whole list: {result[:120]}")
    source, records, gaps_since, tally, shown = None, None, 0, None, 0
    last = 0
    gap_total = 0
    for item in elements(result[1:-1]):
        entry, gap, counts = ENTRY.match(item), GAP.match(item), TALLY.match(item)
        if entry:
            _, _, who, text = entry.groups()
            number = LINE.match(text)
            if not number:
                fail(f"phase {phase}: not a probe line: {text}")
            number = int(number.group(1))
            if source is None:
                source = who
                if number != 1:
                    fail(f"phase {phase}: the first record shown is line {number}, not line 1 (boot window)")
            if who != source:
                fail(f"phase {phase}: a record of another source {who}")
            if number <= last:
                fail(f"phase {phase}: line {number} after line {last}")
            if shed == 0 and number - last - 1 != gaps_since:
                fail(f"phase {phase}: lines {last + 1}..{number - 1} missing, gaps between say {gaps_since}")
            last, gaps_since, shown = number, 0, shown + 1
        elif gap:
            who, count, reason = gap.groups()
            if reason not in ("Not_Retained", "Not_Shown"):
                fail(f"phase {phase}: a gap of {reason}")
            if source is not None and who != source:
                fail(f"phase {phase}: a gap of another source {who}")
            gaps_since += int(count)
            gap_total += int(count)
        elif counts:
            tally = tuple(map(int, counts.groups()))
        else:
            fail(f"phase {phase}: not a LogItem: {item[:120]}")
    if tally is None:
        fail(f"phase {phase}: no tally")
    who, published, retained, not_retained, over_budget, publisher_full = tally
    if str(who) != source:
        fail(f"phase {phase}: the tally is of source {who}, the records of {source}")
    if published != retained + not_retained:
        fail(f"phase {phase}: published {published} != retained {retained} + not retained {not_retained}")
    if published + over_budget + publisher_full != lines:
        fail(f"phase {phase}: {published} published + {over_budget} over budget + {publisher_full} "
             f"ring-shed != {lines} lines written")
    if publisher_full != shed:
        fail(f"phase {phase}: logstore counted {publisher_full} shed by the ring, the probe {shed}")
    if lines <= PER_SOURCE_CEILING and not_retained != 0:
        fail(f"phase {phase}: {not_retained} given up under the ceiling")
    if published > PER_SOURCE_CEILING and (retained != PER_SOURCE_CEILING
                                           or not_retained != published - PER_SOURCE_CEILING):
        fail(f"phase {phase}: retained {retained}, not retained {not_retained}, ceiling {PER_SOURCE_CEILING}")
    if shown + gap_total != published:
        fail(f"phase {phase}: {shown} shown + {gap_total} in gaps != {published} published")
    if shed == 0 and last != lines:
        fail(f"phase {phase}: the last record shown is line {last}, not {lines}")
    print(f"log-burst: phase {phase}: {published} published = {retained} retained + {not_retained} not retained; "
          f"{shown} shown + {gap_total} in typed gaps; shed {over_budget} over budget, {publisher_full} by the ring")


def main():
    if len(sys.argv) != 2:
        fail("usage: check_serial.py SERIAL_LOG")
    text = open(sys.argv[1], "rb").read().decode("latin-1")
    done = {int(m.group(1)): (int(m.group(2)), int(m.group(3))) for m in PHASE_DONE.finditer(text)}
    results = [line[line.index(REPL) + len(REPL):].rstrip("\r") for line in text.splitlines() if REPL in line]
    if len(results) != len(PHASES):
        fail(f"{len(results)} logs.recent results, expected {len(PHASES)}")
    for phase, (expected_lines, result) in enumerate(zip(PHASES, results), start=1):
        if phase not in done or done[phase][0] != expected_lines:
            fail(f"phase {phase}: the probe did not report {expected_lines} lines")
        check(phase, result, expected_lines, done[phase][1])
    print("log-burst: PASS")


if __name__ == "__main__":
    main()
