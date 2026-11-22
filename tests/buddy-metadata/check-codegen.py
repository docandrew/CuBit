#!/usr/bin/env python3
"""Pinned x86 kernel codegen check; not an allocator latency benchmark."""
import re
import subprocess

obj = "kernel/build/buddyallocator.o"
symbols = subprocess.check_output(["nm", "-S", obj], text=True)
entry = re.search(r"^\w+\s+(\w+)\s+\w\s+buddyallocator__descriptoraddress$", symbols, re.M)
if not entry:
    raise SystemExit("FAIL: missing production descriptorAddress")
# Before extracting metadata arithmetic, this routine was 0x1b1 bytes with
# this pinned toolchain (including existing admission/error paths).
if int(entry[1], 16) > 0x1B1:
    raise SystemExit("FAIL: descriptorAddress grew beyond its pre-extraction baseline")
listing = subprocess.check_output(
    ["objdump", "-d", "--disassemble=buddyallocator__descriptoraddress", obj], text=True
)
if not re.search(r"\blea\s+\(%r\w+,%r\w+,2\),%r\w+", listing):
    raise SystemExit("FAIL: descriptor slot calculation lost its indexed LEA")
undefined = subprocess.check_output(["nm", "-u", obj], text=True)
if "buddy_metadata__" in undefined:
    raise SystemExit("FAIL: metadata arithmetic acquired an out-of-line call or load")
core = subprocess.check_output(["nm", "kernel/build/buddy_metadata.o"], text=True)
if re.search(r"\b[Tt]\b|buddy_metadata__(?:prove_|fits_at)", core):
    raise SystemExit("FAIL: metadata helpers or Ghost predicates emitted kernel code")
print("PASS metadata codegen: descriptorAddress within baseline size, indexed LEA, no helper/Ghost code")
