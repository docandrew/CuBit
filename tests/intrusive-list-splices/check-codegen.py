#!/usr/bin/env python3
"""Check the actual optimized x86-64 kernel; this is not a latency benchmark."""
import re
import subprocess

obj = "kernel/build/buddyallocator.o"
symbols = subprocess.check_output(["nm", obj], text=True)
if re.search(r"\bbuddyallocator__list_splices", symbols):
    raise SystemExit("FAIL: splice helpers or Ghost proofs emitted out of line")
listing = subprocess.check_output(
    ["objdump", "-d", "--disassemble=buddyallocator__addtofreelist", obj], text=True
)
if "<buddyallocator__addtofreelist>:" not in listing:
    raise SystemExit("FAIL: production insertion symbol missing")
instructions = [line for line in listing.splitlines() if re.match(
    r"\s*[0-9a-f]+:\s+(?:[0-9a-f]{2}\s)+\s*\w", line)]
calls = [line for line in instructions if re.search(r"\bcall\b", line)]
if len(calls) != 1 or "<buddyallocator__moveblock>" not in calls[0]:
    raise SystemExit("FAIL: insertion acquired additional calls")
# Baseline captured before the splice extraction with the pinned Nix compiler.
# This is deliberately toolchain-specific; review it when updating the compiler.
if len(instructions) > 32:
    raise SystemExit(f"FAIL: insertion grew beyond baseline: {len(instructions)} instructions")
if not re.search(r"\baddl\s+\$0x1,", listing):
    raise SystemExit("FAIL: count no longer increments directly in memory")
kernel_symbols = subprocess.check_output(["nm", "kernel/cubit_kernel"], text=True)
if re.search(r"buddy_list_refinement|refinement_checks", kernel_symbols):
    raise SystemExit("FAIL: Ghost list-refinement harness entered the kernel")
if "free_block_set__" in kernel_symbols:
    raise SystemExit("FAIL: experimental free-set tree entered the kernel")
print(f"PASS splice codegen: {len(instructions)} instructions, original call count, in-place count update; no Ghost/tree code")
