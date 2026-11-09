#!/usr/bin/env python3
"""BUG-003 headless check: a read past nvme.drv's deadline is recovered.

Usage: check.py SERIAL_LOG FIXTURE_RESULT MODE

recover: a command timed out, recovery ran (abort settled, or a controller
         reset re-created the queues), the read was re-issued, and no
         request failed: the boot completed (run.sh checks its markers).
stuck:   a command timed out on every pass and the request failed with the
         typed outcome "timed-out" after the bounded attempts, the client
         got the error reply, and later reads succeeded: boot markers come
         after the failure.

Before the fix the driver latched on the first timeout ("NVMe: I/O
completion lost or foreign", then "NVMe: Read failed." for every read).
"""
from pathlib import Path
import re
import sys

serial, fixture_result, mode = sys.argv[1:]
text = Path(serial).read_bytes().decode("utf-8", "replace")
lines = text.splitlines()
problems = []


def first(pattern, after=-1):
    for index, line in enumerate(lines):
        if index > after and pattern in line:
            return index
    return None


try:
    fixture = Path(fixture_result).read_text()
except OSError:
    fixture = ""
if "done status=0" not in fixture:
    problems.append(f"fixture did not finish its steps: {fixture.strip()!r}")

if "NVMe: I/O completion lost or foreign" in text:
    problems.append("the driver latched (old 'completion lost or foreign')")

timed_out = first("NVMe: command timed out:")
if timed_out is None:
    problems.append("no command timed out")
else:
    recovered = None
    for marker in ("NVMe: controller reset done", "NVMe: recovered without a reset"):
        found = first(marker, timed_out)
        if found is not None and (recovered is None or found < recovered):
            recovered = found
    if recovered is None:
        problems.append("no recovery after the timeout")
    if mode == "recover":
        if recovered is not None and first("NVMe: re-issuing", recovered - 1) is None:
            problems.append("the lost read was not re-issued")
        failure = first("NVMe: read failed:")
        if failure is not None:
            problems.append(f"a request failed: {lines[failure].strip()}")
    else:
        failure = first("outcome=timed-out", timed_out)
        if failure is None:
            problems.append("the stuck request never failed with a typed outcome")
        else:
            if not re.search(r"attempts= 2\b", lines[failure]):
                problems.append(f"unexpected attempt count: {lines[failure].strip()}")
            later = [i for i, line in enumerate(lines)
                     if i > failure and ("shell: cwd=@nvme:0/" in line
                                         or "procmgr: ready" in line
                                         or "ps2: consumer registered" in line)]
            if not later:
                problems.append("no boot progress after the failed request")

if problems:
    for problem in problems:
        print(f"nvme-{mode}: {problem}", file=sys.stderr)
    sys.exit(1)
print(f"nvme-timeout ({mode}): PASS")
