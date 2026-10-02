#!/usr/bin/env python3
"""Check actual final ELF runtime calls bind to the private stack getter."""
import pathlib
import re
import subprocess
import sys
binary = pathlib.Path(sys.argv[1])
for suffix in ("ss_allocate", "ss_mark", "ss_release", "ss_get_max"):
    name = "system__secondary_stack__" + suffix
    code = subprocess.check_output(["objdump", "-d", f"--disassemble={name}", str(binary)], text=True)
    assert f"<{name}>:" in code, name
    assert "<__wrap___gnat_get_secondary_stack>" in code, name
    assert "<__gnat_get_secondary_stack>" not in code, name
symbols = subprocess.check_output(["nm", "-S", "--defined-only", str(binary)], text=True)
match = re.search(r"^([0-9a-f]+) ([0-9a-f]+) [a-zA-Z] servo_secondary_stack__scratch$", symbols, re.M)
assert match, "missing static scratch object"
address, size = (int(value, 16) for value in match.groups())
assert address % 16 == 0 and 32768 <= size <= 32832, (address, size)
print(f"SERVO-SECONDARY-LINK: PASS four runtime callers wrapped, aligned static capacity={size} bytes")
