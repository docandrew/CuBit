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
# KERN-014 slice 1: the links live in a table, not in the block, so the
# insertion calls the descriptor transition and then the three link-table
# stores (setPrev twice, setNext once). The relocation names the callees.
relocated = subprocess.check_output(
    ["objdump", "-dr", "--disassemble=buddyallocator__addtofreelist", obj], text=True
)
callees = re.findall(r"R_X86_64_PLT32\s+buddyallocator__(\w+)-0x4", relocated)
direct = re.findall(r"call\s+[0-9a-f]+ <buddyallocator__(\w+)>", relocated)
if sorted(callees + direct) != sorted(["moveblock", "setprev", "setprev", "setnext"]):
    raise SystemExit(f"FAIL: insertion calls changed: {sorted(callees + direct)}")
# Baseline captured with the pinned Nix compiler (32 before KERN-014's link
# table; 24 after). Deliberately toolchain-specific; review it when updating
# the compiler.
if len(instructions) > 32:
    raise SystemExit(f"FAIL: insertion grew beyond baseline: {len(instructions)} instructions")
if not re.search(r"\baddl\s+\$0x1,", listing):
    raise SystemExit("FAIL: count no longer increments directly in memory")
kernel_symbols = subprocess.check_output(["nm", "kernel/cubit_kernel"], text=True)
if re.search(r"buddy_list_refinement|refinement_checks", kernel_symbols):
    raise SystemExit("FAIL: Ghost list-refinement harness entered the kernel")
if "free_block_set__" in kernel_symbols:
    raise SystemExit("FAIL: experimental free-set tree entered the kernel")
print(f"PASS splice codegen: {len(instructions)} instructions, moveblock plus three link-table stores, in-place count update; no Ghost/tree code")
