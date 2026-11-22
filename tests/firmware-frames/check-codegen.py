#!/usr/bin/env python3
"""Focused production proof-erasure check; not a performance benchmark."""
import subprocess

obj = "kernel/build/firmware_frames.o"
symbols = subprocess.check_output(["nm", obj], text=True)
for name in ("whole_pages", "touched_pages", "largest_block", "classify"):
    if f"firmware_frames__{name}" not in symbols:
        raise SystemExit(f"FAIL: missing production {name}")
for forbidden in ("__prove_", "__postconditions", "__assert"):
    if forbidden in symbols:
        raise SystemExit(f"FAIL: proof/check symbol present: {forbidden}")
undefined = subprocess.check_output(["nm", "-u", obj], text=True)
if undefined.strip():
    raise SystemExit(f"FAIL: review new external dependencies: {undefined}")
print("PASS firmware admission codegen: production helpers present; "
      "no Ghost/assertion symbols or external dependencies")
