#!/usr/bin/env python3
"""Every constant of CuBit.Filesystem_Queues (Ada) has the same value in
cubit_fs_queue.h (C), and the C header has no extra ones."""
import re, sys
from pathlib import Path
root = Path(__file__).resolve().parents[2]
ada = (root / "userspace/runtime/gnat/cubit-filesystem_queues.ads").read_text()
c = (root / "userspace/c/cubit_fs_queue.h").read_text()
def value(text):
    return int(text.replace("_", "").replace("16#", "0x").rstrip("#"), 0)
ada_constants = {}
for name, text in re.findall(r"^\s+(\w+)\s*:\s*constant\s*:=\s*([\w#]+)\s*;", ada, re.M):
    if name in ("Word_Bits", "Long_Bits"):
        continue
    c_name = name.upper() if name.startswith("OP_") else "FS_" + name.upper()
    ada_constants[c_name] = value(text)
c_constants = {n: int(v, 0) for n, v in re.findall(r"^\s+((?:FS|OP_FS)_\w+)\s*=\s*(\w+),", c, re.M)}
bad = [f"{n}: Ada {v}, C {c_constants.get(n)}" for n, v in ada_constants.items() if c_constants.get(n) != v]
bad += [f"{n}: only in C" for n in c_constants if n not in ada_constants]
for line in bad:
    print("FAIL:", line)
print(f"fs-queue-layout: {'PASS' if not bad else 'FAIL'} ({len(ada_constants)} constants)")
sys.exit(1 if bad else 0)
