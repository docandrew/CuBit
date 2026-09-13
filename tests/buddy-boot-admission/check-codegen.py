#!/usr/bin/env python3
"""Pinned x86-64 codegen regression, not a performance or whole-kernel proof."""
import subprocess
import sys

# The boot adapter now uses the proved packed-Boolean reservation ADT rather
# than hand-written word/bit getters. Keep checking the real storage/bounds.
subprocess.run([sys.executable, "tests/boot-frame-allocator/check-codegen.py"], check=True)

undefined = subprocess.check_output(["nm", "-u", "kernel/build/bootallocator.o"], text=True)
if "buddy_boot_admission__" in undefined:
    raise SystemExit("FAIL: boot index/admission helpers were not inlined")
core_symbols = subprocess.check_output(
    ["nm", "kernel/build/buddy_boot_admission.o"], text=True
)
if "buddy_boot_admission__prove_" in core_symbols:
    raise SystemExit("FAIL: Ghost boot-admission proofs entered the kernel")
print("PASS boot admission codegen: no admission helper/Ghost calls")
