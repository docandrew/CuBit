#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""test262 on CuBit against its Linux reference (WEB-006 phase a).

    compare.py SERIAL_LOG [REFERENCE_PER_FILE_JSON [TOLERANCE]]

SERIAL_LOG is a ladybird-test262 run's serial log: the launcher's line for
every result that is not a pass ("ladybird-test262: <result> <test>") and
its totals. REFERENCE_PER_FILE_JSON is libjs-test262's --per-file output for
the same build hosted on Linux (cubit-ladybird scripts/test262-reference.sh).
With a reference, the run passes when CuBit's pass count is within
TOLERANCE (a fraction of the tests; default 0.001) of the reference's, and
the tests whose results differ are listed.
"""
import json
import re
import sys
from pathlib import Path

PREFIX = "@nvme:0/test262/"
REFERENCE_NAMES = {
    "passed": "PASSED", "failed": "FAILED", "skipped": "SKIPPED",
    "metadata_error": "METADATA_ERROR", "harness_error": "HARNESS_ERROR",
    "timeout": "TIMEOUT_ERROR", "assert_fail": "PROCESS_ERROR",
    "process_error": "PROCESS_ERROR", "todo_error": "TODO_ERROR",
}


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    text = Path(sys.argv[1]).read_text(errors="replace")
    totals = re.search(r"ladybird-test262: tests (\d+), passed (\d+), failed (\d+)", text)
    if not totals or "LADYBIRD-TEST262: DONE" not in text:
        print("compare: the run did not finish", file=sys.stderr)
        return 1
    total, passed = int(totals.group(1)), int(totals.group(2))
    print(f"CuBit: {passed} of {total} passed ({100 * passed / total:.3f}%)")
    print(totals.group(0))
    cubit = {}
    for result, test in re.findall(r"ladybird-test262: (\w+) (@nvme:0/test262/\S+)", text):
        cubit[test.removeprefix(PREFIX)] = REFERENCE_NAMES.get(result, "FAILED")
    if len(sys.argv) < 3:
        return 0
    reference = json.loads(Path(sys.argv[2]).read_text())["results"]
    tolerance = float(sys.argv[3]) if len(sys.argv) > 3 else 0.001
    reference_passed = sum(1 for r in reference.values() if r == "PASSED")
    print(f"Linux: {reference_passed} of {len(reference)} passed ({100 * reference_passed / len(reference):.3f}%)")
    differing = sorted(t for t in set(reference) | set(cubit)
                       if cubit.get(t, "PASSED") != reference.get(t, "missing"))
    for test in differing:
        print(f"  {test}: CuBit {cubit.get(test, 'PASSED')}, Linux {reference.get(test, 'missing')}")
    gap = abs(passed - reference_passed) / max(len(reference), 1)
    verdict = total == len(reference) and gap <= tolerance
    print(f"{len(differing)} tests differ; pass counts differ by {100 * gap:.3f}% of the tests "
          f"(tolerance {100 * tolerance:.3f}%): {'PASS' if verdict else 'FAIL'}")
    return 0 if verdict else 1


if __name__ == "__main__":
    sys.exit(main())
