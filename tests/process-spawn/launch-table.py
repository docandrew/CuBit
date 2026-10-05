#!/usr/bin/env python3
"""INTERIM test fixture: emit a .cubit.launch section (CuBit.Launch_Authority
layout) naming the programs a test executable may start.

This stands in for the manifest form (may-launch "name" ...) until the CCL
manifest compiler emits the section from a typed declaration; then this
script and its use in build.sh go away (docs/process-arguments.md).

    launch-table.py NAME... > table.S
"""
import struct
import sys

MAGIC = b"LNCH"
VERSION = 1
MAXIMUM_NAMES = 32
MAXIMUM_TABLE_BYTES = 512
MAXIMUM_NAME_BYTES = 255

names = [n.encode() for n in sys.argv[1:]]
if len(names) > MAXIMUM_NAMES or any(
        not n or len(n) > MAXIMUM_NAME_BYTES or b"\0" in n for n in names):
    sys.exit("launch-table: invalid names")
data = MAGIC + struct.pack("<HH", VERSION, len(names))
for n in names:
    data += bytes([len(n)]) + n
if len(data) > MAXIMUM_TABLE_BYTES:
    sys.exit("launch-table: table too large")
print('.section .cubit.launch,"",@progbits')
for b in data:
    print(f".byte 0x{b:02x}")
print('.section .note.GNU-stack,"",@progbits')
