#!/usr/bin/env python3
"""Check genuine SHA-1 note retention and readable load coverage in CuBit ELF."""
from pathlib import Path
import struct
import sys

data = Path(sys.argv[1]).read_bytes()
assert len(data) >= 64 and data[:6] == b'\x7fELF\x02\x01', 'expected ELF64 LE'
phoff = struct.unpack_from('<Q', data, 32)[0]
entry_size, count = struct.unpack_from('<HH', data, 54)
assert entry_size == 56 and count > 0
assert phoff <= len(data) and count <= (len(data) - phoff) // entry_size
headers = [struct.unpack_from('<IIQQQQQQ', data, phoff + n * entry_size)
           for n in range(count)]
notes = []
for kind, flags, offset, address, _, size, memory_size, _ in headers:
    assert offset <= len(data) and size <= len(data) - offset
    if kind != 4:  # PT_NOTE
        continue
    pos, end = offset, offset + size
    while pos < end:
        assert end - pos >= 12, 'truncated note header'
        namesz, descsz, tag = struct.unpack_from('<III', data, pos)
        name = pos + 12
        desc = name + ((namesz + 3) & ~3)
        next_pos = desc + ((descsz + 3) & ~3)
        assert next_pos <= end, 'truncated note payload'
        if tag == 3 and namesz == 4 and data[name:name + 4] == b'GNU\0':
            assert descsz == 20 and any(data[desc:desc + descsz]), 'not genuine SHA1'
            note_address = address + pos - offset
            assert any(
                load[0] == 1 and load[1] & 4 and
                load[2] <= pos and next_pos <= load[2] + load[5] and
                load[3] + pos - load[2] == note_address and
                load[5] <= load[6]
                for load in headers), 'build ID not covered by readable PT_LOAD'
            notes.append(data[desc:desc + descsz].hex())
        pos = next_pos
assert len(notes) == 1, 'missing or ambiguous GNU build ID'
print('native ELF build-ID layout: PASS', notes[0])
