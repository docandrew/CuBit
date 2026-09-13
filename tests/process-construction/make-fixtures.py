#!/usr/bin/env python3
"""Derive intentionally invalid ELFs from the authorityless regression app."""
import pathlib
import struct
import sys

source = pathlib.Path(sys.argv[1]).read_bytes()
destination = pathlib.Path(sys.argv[2])
destination.mkdir(parents=True, exist_ok=True)
phoff = struct.unpack_from("<Q", source, 32)[0]
entsize, count = struct.unpack_from("<HH", source, 54)
assert entsize == 56
headers = [phoff + i * entsize for i in range(count)]
loads = [h for h in headers if struct.unpack_from("<I", source, h)[0] == 1]
stacks = [h for h in headers if struct.unpack_from("<I", source, h)[0] == 0x6474E551]
assert len(loads) >= 2 and len(stacks) == 1

def emit(name, edits):
    data = bytearray(source)
    for fmt, offset, value in edits:
        struct.pack_into(fmt, data, offset, value)
    (destination / name).write_bytes(data)

emit("bad-phdr.app", [("<H", 54, 1)])
emit("bad-segment.app", [("<Q", loads[0] + 32, (1 << 64) - 1)])
emit("bad-stack.app", [("<Q", stacks[0] + 40, 0)])
# Keep the executable entry segment intact. A later mapping collides only
# AFTER the stack and earlier PT_LOAD segments have been allocated.
assert struct.unpack_from("<I", source, loads[-1] + 4)[0] & 1 == 0
first_va = struct.unpack_from("<Q", source, loads[0] + 16)[0]
emit("overlap.app", [("<Q", loads[-1] + 16, first_va)])
print("construction: generated malformed geometry and partial-load rollback fixtures")
