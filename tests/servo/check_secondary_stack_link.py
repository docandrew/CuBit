#!/usr/bin/env python3
"""Check actual final ELF runtime calls bind to the private stack getter."""
import pathlib
import re
import subprocess
import sys
binary = pathlib.Path(sys.argv[1])
# The callers that look the current stack up; SS_Release (upstream GNAT's)
# writes through its mark's stack and must not look it up at all.
for suffix, looks_up in (("ss_allocate", True), ("ss_mark", True),
                         ("ss_get_max", True), ("ss_release", False)):
    name = "system__secondary_stack__" + suffix
    code = subprocess.check_output(["objdump", "-d", f"--disassemble={name}", str(binary)], text=True)
    assert f"<{name}>:" in code, name
    assert ("<__wrap___gnat_get_secondary_stack>" in code) == looks_up, name
    assert "<__gnat_get_secondary_stack>" not in code, name
symbols = subprocess.check_output(["nm", "-S", "--defined-only", str(binary)], text=True)
match = re.search(r"^([0-9a-f]+) ([0-9a-f]+) [a-zA-Z] servo_secondary_stack__scratch$", symbols, re.M)
assert match, "missing static scratch object"
address, size = (int(value, 16) for value in match.groups())
# Runtime_Default_Sec_Stack_Size bytes of chunk plus the SS_Stack header
# (upstream GNAT's: 80 bytes); the ceiling bounds the header, not the chunk.
DEFAULT_CHUNK_BYTES = 32768
HEADER_CEILING_BYTES = 128
assert address % 16 == 0 and DEFAULT_CHUNK_BYTES <= size <= DEFAULT_CHUNK_BYTES + HEADER_CEILING_BYTES, (address, size)
print(f"SERVO-SECONDARY-LINK: PASS three runtime lookups wrapped, release by mark, aligned static capacity={size} bytes")
