#!/usr/bin/env python3
"""Release code must not carry Ghost invariants or assertion machinery."""
from pathlib import Path
import subprocess

obj = "kernel/build/boot_framebuffer.o"
symbols = subprocess.check_output(["nm", obj], text=True)
for routine in ("decode", "pixel_offset"):
    assert f"boot_framebuffer__{routine}" in symbols
for forbidden in ("__valid", "assert", "postconditions", "secondary_stack"):
    assert forbidden not in symbols, forbidden
undefined = subprocess.check_output(["nm", "-u", obj], text=True)
assert not undefined.strip(), undefined
for line in Path("kernel/build/boot_framebuffer.su").read_text().splitlines():
    name, stack, kind = line.split("\t")
    assert int(stack) <= 2048 and kind == "static", line
    print(f"PASS {name.rsplit(':', 1)[-1]}: {stack} bytes stack")
print("PASS framebuffer Ghost erasure and no runtime dependencies")
