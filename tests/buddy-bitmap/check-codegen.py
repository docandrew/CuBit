#!/usr/bin/env python3
"""Check the actual assertion-free x86-64 kernel object, not the host harness."""
import re
import subprocess
import sys
from pathlib import Path

object_file = Path(sys.argv[1] if len(sys.argv) > 1 else "kernel/build/buddy_bitmap.o")
symbols = subprocess.check_output(["nm", str(object_file)], text=True)
for helper in ("prove_", "consistent", "pair_span", "limit_bit"):
    if re.search(r"\bbuddy_bitmap__" + helper, symbols):
        raise SystemExit(f"FAIL: ghost helper emitted in kernel object: {helper}")

listing = subprocess.check_output(
    ["objdump", "-d", "--disassemble=buddy_bitmap__locate", str(object_file)], text=True
)
if "<buddy_bitmap__locate>:" not in listing:
    raise SystemExit("FAIL: kernel lookup symbol missing")
if not re.search(r"\bshr[qwl]?\b", listing):
    raise SystemExit("FAIL: lookup no longer uses a right shift")
if re.search(r"\b(?:i?div|call)[qwl]?\b", listing):
    raise SystemExit("FAIL: lookup contains division or a subprogram call")
print("PASS kernel codegen: ghost helpers absent; lookup uses shift with no divide/call")
