#!/usr/bin/env python3
"""Focused release-code proof erasure and stack checks."""
from pathlib import Path
import subprocess

obj = "kernel/build/multiboot_entry.o"
symbols = subprocess.check_output(["nm", obj], text=True)
for name in ("admit_address", "snapshot"):
    assert "multiboot_entry__" + name in symbols, name
for name in ("__assert", "__postconditions", "__prove_", "secondary_stack"):
    assert name not in symbols, name
undefined = subprocess.check_output(["nm", "-u", obj], text=True)
assert not undefined.strip(), undefined
for unit, routine in (("multiboot_entry", "Admit_Address"),
                      ("multiboot_entry", "Snapshot"),
                      ("multiboot", "Read_Information")):
    rows = [line.split("\t") for line in
            Path(f"kernel/build/{unit}.su").read_text().splitlines()
            if line.split("\t")[0].endswith(":" + routine)]
    assert len(rows) == 1 and int(rows[0][1]) <= 2048, rows
    print(f"PASS {routine}: {rows[0][1]} bytes per-function stack")
print("PASS entry codegen: no external, runtime assertion or proof dependencies")
