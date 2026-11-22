#!/usr/bin/env python3
"""Check proof erasure, static workspace and the absence of late raw reads."""
from pathlib import Path
import subprocess

symbols = subprocess.check_output(["nm", "kernel/build/boot_modules.o"], text=True)
for name in ("append", "seal", "reserved_end", "clear_padding", "in_ram"):
    assert f"boot_modules__{name}" in symbols, name
for forbidden in ("__consistent", "__postconditions", "__assert", "secondary_stack"):
    assert forbidden not in symbols, forbidden
dependencies = {line.split()[-1] for line in subprocess.check_output(
    ["nm", "-u", "kernel/build/boot_modules.o"], text=True).splitlines()}
assert dependencies <= {"memcmp", "memset"}, dependencies
for unit, routine in (("boot_modules", "Append"), ("boot_modules", "Clear_Padding"),
                      ("boot_modules", "Reserved_End"), ("multiboot", "Getmemoryareas")):
    rows = [line.split("\t") for line in Path(f"kernel/build/{unit}.su").read_text().splitlines()
            if line.split("\t")[0].endswith(":" + routine)]
    assert len(rows) == 1 and int(rows[0][1]) <= 2048, rows
    print(f"PASS {routine}: {rows[0][1]} bytes per-function stack")
workspace = subprocess.check_output(["nm", "-S", "kernel/build/multiboot.o"], text=True)
for name in ("bootcatalog", "capturedmodules"):
    rows = [line.split() for line in workspace.splitlines() if line.endswith("__" + name)]
    assert len(rows) == 1 and rows[0][2].lower() == "b", rows
    print(f"PASS {name}: {int(rows[0][1], 16)} bytes BSS workspace")
consumer = Path("kernel/src/modules.adb").read_text()
for stale in ("mods_addr", "mods_count", "mod_string", "MBModule", "Strings.toAda"):
    assert stale not in consumer, stale
print("PASS Ghost erasure, bounded workspace and no late descriptor/name reads")
