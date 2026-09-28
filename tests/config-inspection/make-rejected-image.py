"""Create a test-only ELF whose Config scope installer must reject its rights.

Keep the executable intact: if the launcher mistakenly resumes it, the normal
config-denied fixture will report a failure because its requested read scope
was never installed. Never modify the source/staged application in place.
"""
import pathlib
import struct
import sys

source, destination = map(pathlib.Path, sys.argv[1:])
data = bytearray(source.read_bytes())
assert data[:6] == b"\x7fELF\x02\x01"
sections = struct.unpack_from("<Q", data, 40)[0]
entry_size, count = struct.unpack_from("<HH", data, 58)
assert entry_size == 64 and sections + count * entry_size <= len(data)
changed = 0
for index in range(count):
    base = sections + index * entry_size
    kind = struct.unpack_from("<I", data, base + 4)[0]
    offset, length = struct.unpack_from("<QQ", data, base + 24)
    if kind != 1 or length < 16:
        continue
    assert offset + length <= len(data)
    if data[offset:offset + 4] != b"CACC":
        continue
    version, entries = struct.unpack_from("<HH", data, offset + 4)
    assert version == 1 and entries == 1 and length >= 96
    entry = offset + 16
    assert data[entry] == 1 and data[entry + 2] == 1  # read / Config
    data[entry] = 4  # invalid to Config's read/write mask; not a grant
    changed += 1
assert changed == 1
with destination.open("xb") as output:
    output.write(data)
