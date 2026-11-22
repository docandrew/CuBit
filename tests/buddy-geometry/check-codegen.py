#!/usr/bin/env python3
"""Inspect the production kernel object, with runtime assertions disabled."""
import re
import subprocess

path = "kernel/build/buddy_geometry.o"
symbols = subprocess.check_output(["nm", path], text=True)
if re.search(r"\bbuddy_geometry__(?:prove_|valid\b)", symbols):
    raise SystemExit("FAIL: geometry Ghost code emitted in the kernel")
undefined = subprocess.check_output(["nm", "-u", path], text=True).strip()
if undefined:
    raise SystemExit(f"FAIL: unexpected geometry runtime dependencies: {undefined}")
listing = subprocess.check_output(
    ["objdump", "-d", "--disassemble=buddy_geometry__split", path], text=True
)
if "<buddy_geometry__split>:" not in listing:
    raise SystemExit("FAIL: production split symbol missing")
if re.search(r"\b(?:i?div|call)[qwl]?\b", listing):
    raise SystemExit("FAIL: split contains division or helper calls")
print("PASS geometry codegen: no Ghost/runtime dependencies; split has no divide/call")
