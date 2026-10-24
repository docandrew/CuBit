#!/usr/bin/env python3
"""check-embedded.py PROGRAM SYMBOL SHA256: the bytes between SYMBOL_start
and SYMBOL_end in the linked ELF64 PROGRAM hash to SHA256 (ccl-build's
embedded blobs, docs/device-matching.md "Firmware")."""
import hashlib
import struct
import sys

SHT_SYMTAB = 2
SECTION_HEADER = '<IIQQQQIIQQ'
SYMBOL = '<IBBHQQ'


def main(path, symbol, expected):
    data = open(path, 'rb').read()
    if data[:4] != b'\x7fELF' or data[4] != 2:
        sys.exit(f'{path}: not an ELF64 file')
    shoff, = struct.unpack_from('<Q', data, 40)
    shentsize, shnum = struct.unpack_from('<HH', data, 58)
    sections = [struct.unpack_from(SECTION_HEADER, data, shoff + i * shentsize) for i in range(shnum)]
    addresses = {}
    for kind, link, offset, size, entsize in [(s[1], s[6], s[4], s[5], s[9]) for s in sections]:
        if kind != SHT_SYMTAB:
            continue
        strings = sections[link][4]
        for at in range(offset, offset + size, entsize):
            name, _, _, index, value, _ = struct.unpack_from(SYMBOL, data, at)
            text = data[strings + name:data.index(b'\0', strings + name)].decode()
            addresses[text] = (value, index)
    start, index = addresses[symbol + '_start']
    end, _ = addresses[symbol + '_end']
    section = sections[index]
    address, offset = section[3], section[4]
    blob = data[offset + start - address:offset + end - address]
    actual = hashlib.sha256(blob).hexdigest()
    if actual != expected:
        sys.exit(f'{symbol}: {len(blob)} bytes hash to {actual}, expected {expected}')
    print(f'{symbol}: {len(blob)} bytes, sha256 {actual}')


if __name__ == '__main__':
    main(*sys.argv[1:])
