#!/usr/bin/env python3
"""Inspect the actual final ELF's path scopes, not only the manifest source."""
import json
from pathlib import Path
import struct
import sys


def access_section(path):
    with Path(path).open('rb') as file:
        header = file.read(64)
        assert header[:6] == b'\x7fELF\x02\x01', 'expected little-endian ELF64'
        offset = struct.unpack_from('<Q', header, 40)[0]
        size, count, names_index = struct.unpack_from('<HHH', header, 58)
        assert size == 64 and count < 4096 and names_index < count
        file.seek(offset)
        sections = [struct.unpack('<IIQQQQIIQQ', file.read(64)) for _ in range(count)]
        names = sections[names_index]
        assert names[5] < 65536
        file.seek(names[4]); strings = file.read(names[5])
        for section in sections:
            name = strings[section[0]:].split(b'\0', 1)[0]
            if name == b'.cubit.access':
                assert section[5] <= 4096
                file.seek(section[4]); return file.read(section[5])
    raise AssertionError('missing authority section')


def check(data):
    assert data[:4] == b'CACC'
    version, count = struct.unpack_from('<HH', data, 4)
    assert version == 1 and len(data) == 16 + 80 * count
    scopes = []
    for index in range(count):
        row = data[16 + index * 80:16 + (index + 1) * 80]
        rights, length, service = row[:3]
        assert 0 < length <= 64
        scopes.append((service, rights, row[8:8 + length].decode('ascii')))
    # FS service 0: read=1, write=2, create=8. Config service 1: read/write=3.
    expected = {(0, 11, '@nvme:0/Bookmarks'), (0, 11, '@nvme:0/Downloads'),
                (0, 1, '@nvme:0/fonts'), (0, 1, '@nvme:0/servo'), (0, 1, '@nvme:0/tls'),
                (1, 3, 'browser.servo')}
    assert len(scopes) == len(expected) and set(scopes) == expected, scopes
    return scopes


if __name__ == '__main__':
    print(json.dumps({'scopes': check(access_section(sys.argv[1])), 'result': 'PASS'}, indent=2))
