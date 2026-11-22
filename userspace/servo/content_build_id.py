#!/usr/bin/env python3
"""Give Penny a build ID that names its program content.

The linker's build ID hashes the whole unstripped output, whose symbol
table holds checkout-dependent local names (LLVM's per-module suffixes), so
two checkouts that ship identical code got different IDs. This sets the
GNU build-id note of the stripped program to the SHA-1 of that program with
the note's ID zeroed, and writes the same ID into the unstripped ELF it came
from, so crash addresses still match the unstripped build.

    content_build_id.py <stripped-elf> <unstripped-elf>
"""
import hashlib
import struct
import sys

ELF_SECTION_HEADER_OFFSET = 0x28   # e_shoff (ELF64)
ELF_SECTION_HEADER_SIZE = 0x3A     # e_shentsize
ELF_SECTION_COUNT = 0x3C           # e_shnum
ELF_SECTION_NAMES = 0x3E           # e_shstrndx
NOTE_HEADER_BYTES = 12             # namesz, descsz, type
GNU_NAME_BYTES = 4                 # "GNU\0"
NT_GNU_BUILD_ID = 3
SHA1_BYTES = 20


def build_id_range(data):
    """File offset and length of the GNU build-id descriptor."""
    shoff, = struct.unpack_from("<Q", data, ELF_SECTION_HEADER_OFFSET)
    entsize, count, names = struct.unpack_from("<HHH", data, ELF_SECTION_HEADER_SIZE)
    def section(index):
        name, _, _, _, offset, size = struct.unpack_from("<IIQQQQ", data, shoff + index * entsize)
        return name, offset, size
    _, names_offset, _ = section(names)
    for index in range(count):
        name, offset, size = section(index)
        end = data.index(b"\0", names_offset + name)
        if data[names_offset + name:end] == b".note.gnu.build-id":
            namesz, descsz, kind = struct.unpack_from("<III", data, offset)
            assert namesz == GNU_NAME_BYTES and kind == NT_GNU_BUILD_ID and descsz == SHA1_BYTES
            return offset + NOTE_HEADER_BYTES + GNU_NAME_BYTES, descsz
    sys.exit("content_build_id.py: no .note.gnu.build-id")


def main():
    stripped, unstripped = sys.argv[1], sys.argv[2]
    data = bytearray(open(stripped, "rb").read())
    start, length = build_id_range(data)
    data[start:start + length] = bytes(length)
    identity = hashlib.sha1(data).digest()
    for path in (stripped, unstripped):
        elf = bytearray(open(path, "rb").read())
        start, length = build_id_range(elf)
        elf[start:start + length] = identity
        open(path, "wb").write(elf)
    print(f"build-id {identity.hex()}")


if __name__ == "__main__":
    main()
