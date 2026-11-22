#!/usr/bin/env python3
"""Check repeated unpublished rollback and PID reuse, not just boot liveness."""
import pathlib
import re
import sys

log = pathlib.Path(sys.argv[1]).read_text(errors="replace")
reclaimed = re.findall(r"unpublished process reclaimed PID\s+(\d+)", log)
assert len(reclaimed) == 8, f"expected eight partial-load rollbacks, saw {len(reclaimed)}"
assert len(set(reclaimed)) == 1, f"construction leaked/reordered PID slots: {reclaimed}"
# ProcessName is 16 bytes; the filename is currently printed as capability-test.
successful = re.search(r"Loaded module capability-test\.\s+w/ process ID\s+(\d+)", log)
assert successful, "successful follow-up ELF launch missing"
assert successful[1] == reclaimed[-1], "valid launch did not reuse reclaimed PID"
assert "capability-test: all tests passed" in log
print("headless: eight partial-load rollbacks and successful PID reuse verified")
