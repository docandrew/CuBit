#!/usr/bin/env python3
"""Pinned x86-64 codegen checks, not an allocator latency benchmark."""
import re
import subprocess
from pathlib import Path

obj = "kernel/build/bootallocator.o"


def disassemble(symbol):
    output = subprocess.check_output(
        ["objdump", "-dr", f"--disassemble={symbol}", obj], text=True
    )
    if f"<{symbol}>:" not in output:
        raise SystemExit(f"FAIL: missing {symbol}")
    return output


symbols = subprocess.check_output(["nm", "-S", obj], text=True)
state = re.search(r"^\w+\s+(\w+)\s+\w\s+bootallocator__reservations$", symbols, re.M)
if not state:
    raise SystemExit("FAIL: missing reservation state")
# Pinned representation: packed frame bits followed by a 32-bit high-water PFN.
state_bytes = int(state[1], 16)
if state_bytes != 2052:
    raise SystemExit("FAIL: review changed boot bitmap/high-water representation")
bitmap_bytes = state_bytes - 4
last_frame = bitmap_bytes * 8 - 1
query = disassemble("bootallocator__isfree")
reserve = disassemble("bootallocator__frames__reserve")
highest = disassemble("bootallocator__highestpfnallocated")
for name, output in (("isFree", query), ("Reserve", reserve)):
    if not re.search(rf"\bcmp\s+\$0x{last_frame:x},", output):
        raise SystemExit(f"FAIL: {name} inclusive bound disagrees with bitmap")
    if not re.search(r"\bsar\s+\$0x3,", output) or not re.search(r"\band\s+\$0x7,", output):
        raise SystemExit(f"FAIL: {name} no longer uses direct packed-bit indexing")
if not re.search(rf"0x{bitmap_bytes:x}\(%rdi\)", reserve):
    raise SystemExit("FAIL: high-water write not immediately after packed bitmap")
if "bootallocator__reservations+0x7fc" not in highest:
    raise SystemExit("FAIL: high-water reader does not match the reservation field")
if re.search(r"\b(call\w*|i?div\w*|push\w*|rep\w*)\s", reserve):
    raise SystemExit("FAIL: reservation gained calls, division, stack saves or a bulk copy")
if re.search(r"\bsub\s+.*%rsp", reserve):
    raise SystemExit("FAIL: reservation gained a stack allocation")
usage = Path("kernel/build/bootallocator.su").read_text()
if not re.search(r":Reserve\s+8\s+static$", usage, re.M):
    raise SystemExit("FAIL: reservation no longer has the minimal leaf stack usage")
if re.search(r"(?i)(reservation_properties|__postconditions|__assert|__prove_)", symbols):
    raise SystemExit("FAIL: proof/check symbols entered production allocator")
print(f"PASS boot reservation codegen: {bitmap_bytes}-byte packed bitmap; "
      f"inclusive PFN {last_frame}; leaf reservation, no calls/division/copies/proof code")
