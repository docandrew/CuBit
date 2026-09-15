#!/usr/bin/env python3
"""Check proof erasure and boot-workspace stack discipline, not throughput."""
from pathlib import Path
import subprocess


def nm(unit, *options):
    return subprocess.check_output(
        ["nm", *options, f"kernel/build/{unit}.o"], text=True)


symbols = nm("multiboot_memory_map")
for name in ("parse", "next_entry"):
    assert f"multiboot_memory_map__{name}" in symbols, name
for forbidden in ("__prove_", "__postconditions", "__assert", "secondary_stack"):
    assert forbidden not in symbols, forbidden
dependencies = {line.split()[-1] for line in
                nm("multiboot_memory_map", "-u").splitlines()}
assert dependencies <= {"memcpy"}, dependencies
assert "secondary_stack" not in nm("memoryareas", "-u")

for unit, routine in (("multiboot", "Getmemoryareas"),
                      ("multiboot_memory_map", "Parse"),
                      ("memoryareas", "Allocation_Map")):
    rows = [line.split("\t") for line in
            Path(f"kernel/build/{unit}.su").read_text().splitlines()
            if line.split("\t")[0].endswith(":" + routine)]
    assert len(rows) == 1, (unit, rows)
    stack_bytes = int(rows[0][1])
    assert stack_bytes <= 2048, (routine, stack_bytes)
    print(f"PASS {routine}: {stack_bytes} bytes stack (per-function report)")

for unit, name in (("multiboot", "bootmapsnapshot"),
                   ("kmain", "bootmemoryareas"),
                   ("kmain", "bootallocationmap")):
    rows = [line.split() for line in nm(unit, "-S").splitlines()
            if line.endswith("__" + name)]
    assert len(rows) == 1 and rows[0][2].lower() in ("b", "d"), (name, rows)
    print(f"PASS {name}: {int(rows[0][1], 16)} bytes static workspace")

print("PASS decoder proof erasure; no normalization secondary-stack dependency")
