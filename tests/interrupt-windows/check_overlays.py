#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""No per-CPU overlay where an interrupt window may move the thread.

An interrupt window (Interrupt_Windows) lets a system call be preempted
and resume on another CPU. Per-CPU data read through an overlay taken
before the window then describes the old CPU, which runs another thread:
the bundle 8 panic ("Spinlock release without CPU ownership") was SBRK
locking one process's address space through Syscall.handler's overlay and
unlocking another's. So the system call handlers (syscall*.adb), which
reach window-opening work through calls, and every subprogram that opens
a window must read per-CPU state live (PerCPUData's functions), never
through an overlay at `PerCPUData.getPerCPUDataAddr`.

Usage: check_overlays.py [KERNEL_SRC_DIR]. Exit 1 on an overlay.
"""
import re
import sys
from pathlib import Path

src = Path(sys.argv[1] if len(sys.argv) > 1 else Path(__file__).resolve().parents[2] / "kernel/src")
overlay = re.compile(r"getPerCPUDataAddr")
subprogram = re.compile(r"^\s*(?:overriding\s+)?(?:procedure|function)\s+(\w+)")


def enclosing(lines, index):
    """The nearest subprogram declared above line index (good enough here:
    the kernel declares no subprogram inside a subprogram that opens a
    window)."""
    for i in range(index, -1, -1):
        m = subprogram.match(lines[i].split("--", 1)[0])
        if m:
            return m.group(1)
    return None


failed = False
for unit in sorted(src.glob("*.adb")):
    if unit.name == "interrupt_windows.adb":
        continue
    lines = unit.read_text(errors="replace").splitlines()
    code = [line.split("--", 1)[0] for line in lines]
    # Subprograms that open windows themselves.
    opening = {enclosing(lines, i) for i, c in enumerate(code) if "Interrupt_Windows." in c and
               ("Open" in c or "Point" in c)}
    for i, c in enumerate(code):
        if not overlay.search(c):
            continue
        owner = enclosing(lines, i)
        # A system call handler reaches window-opening work (spawn, sbrk,
        # owned memory) through calls; a window opener can move itself.
        if unit.name.startswith("syscall") or owner in opening:
            print(f"INTERRUPT-WINDOWS: FAIL per-CPU overlay in {unit.name}:{i + 1} "
                  f"({owner}): {lines[i].strip()}")
            failed = True
if failed:
    sys.exit(1)
print("INTERRUPT-WINDOWS: PASS no per-CPU overlay in system call handlers or window openers")
