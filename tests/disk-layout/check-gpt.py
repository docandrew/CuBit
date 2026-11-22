#!/usr/bin/env python3
"""Checks an installer GPT independently of CuBit's decoder, for any block
size (sgdisk reads image files as 512-byte disks): protective MBR, both
headers' fields and CRC32s, both entry arrays equal, ESP and root entries.

usage: check-gpt.py IMAGE BLOCK_BYTES
"""
import struct
import sys
import uuid
import zlib

ESP_TYPE = uuid.UUID("C12A7328-F81F-11D2-BA4B-00A0C93EC93B").bytes_le
LINUX_TYPE = uuid.UUID("0FC63DAF-8483-4772-8E79-3D69D8477DE4").bytes_le
HEADER_BYTES = 92
ENTRY_COUNT = 128
ENTRY_BYTES = 128
MIB = 1 << 20


def main() -> int:
    path, block = sys.argv[1], int(sys.argv[2])
    with open(path, "rb") as f:
        image_size = f.seek(0, 2)

        def read(lba: int, count: int) -> bytes:
            f.seek(lba * block)
            return f.read(count)

        blocks = image_size // block
        problems = []
        mbr = read(0, 512)
        if mbr[510:512] != b"\x55\xaa" or mbr[446 + 4] != 0xEE:
            problems.append("protective MBR")
        if struct.unpack_from("<I", mbr, 446 + 8)[0] != 1:
            problems.append("protective record start")
        tables = []
        for current, other in ((1, blocks - 1), (blocks - 1, 1)):
            raw = bytearray(read(current, HEADER_BYTES))
            (sig, rev, size, crc, _, me, alt, first, last, disk, entries_lba,
             count, esize, ecrc) = struct.unpack("<8sIIIIQQQQ16sQIII", raw)
            raw[16:20] = b"\0\0\0\0"
            if sig != b"EFI PART" or rev != 0x10000 or size != HEADER_BYTES:
                problems.append(f"header {current} signature/revision/size")
            if zlib.crc32(raw) != crc:
                problems.append(f"header {current} crc")
            if me != current or alt != other or count != ENTRY_COUNT or esize != ENTRY_BYTES:
                problems.append(f"header {current} locations")
            table = read(entries_lba, ENTRY_COUNT * ENTRY_BYTES)
            if zlib.crc32(table) != ecrc:
                problems.append(f"header {current} entries crc")
            tables.append(table)
            esp_type, _, esp_first, esp_last = struct.unpack_from("<16s16sQQ", table, 0)
            root_type, _, root_first, root_last = struct.unpack_from("<16s16sQQ", table, ENTRY_BYTES)
            if esp_type != ESP_TYPE or root_type != LINUX_TYPE:
                problems.append("partition types")
            if not (first <= esp_first <= esp_last < root_first <= root_last <= last):
                problems.append("partition ranges")
            if (esp_first * block) % MIB or (root_first * block) % MIB:
                problems.append("1 MiB alignment")
        if tables[0] != tables[1]:
            problems.append("primary and backup entry arrays differ")
    for p in problems:
        print(f"check-gpt: {p} FAIL")
    if not problems:
        print(f"check-gpt: PASS ({block}-byte blocks)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
