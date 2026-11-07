#!/usr/bin/env python3
"""Writes the device-build test's UFS disk: a GPT image (4 KiB blocks, as
QEMU's ufs-lu reports and the IdeaPad's disk has) whose one Linux
filesystem partition holds an ext2 image, the installed root the filesystem
names @ufs:0 (and @system, by storage.system-volume).

usage: make-disk.py IMAGE ROOT_EXT2
"""
import struct
import sys
import uuid
import zlib

BLOCK = 4096
MIB = 1024 * 1024
ENTRY_COUNT = 128
ENTRY_BYTES = 128
LINUX_FILESYSTEM = uuid.UUID("0FC63DAF-8483-4772-8E79-3D69D8477DE4")
DISK = uuid.UUID("5B0E7C2A-2F4D-4C38-9E5A-0123456789AC")
PART = uuid.UUID("5B0E7C2A-2F4D-4C38-9E5A-00000000D0B1")


def main() -> None:
    path, root = sys.argv[1], sys.argv[2]
    data = open(root, "rb").read()
    per_mib = MIB // BLOCK
    first = 1 * per_mib
    root_blocks = (len(data) + BLOCK - 1) // BLOCK
    last = first + root_blocks - 1
    entry_blocks = (ENTRY_COUNT * ENTRY_BYTES + BLOCK - 1) // BLOCK
    blocks = last + 1 + per_mib            # 1 MiB after it: backup GPT
    first_usable = 2 + entry_blocks
    last_usable = blocks - 2 - entry_blocks

    entries = bytearray(ENTRY_COUNT * ENTRY_BYTES)
    entries[0:ENTRY_BYTES] = struct.pack(
        "<16s16sQQQ72s", LINUX_FILESYSTEM.bytes_le, PART.bytes_le, first, last, 0,
        "CuBit".encode("utf-16-le"))
    entries_crc = zlib.crc32(entries) & 0xFFFFFFFF

    def header(current: int, backup: int, entries_lba: int) -> bytes:
        fields = struct.pack(
            "<8sIIIIQQQQ16sQIII", b"EFI PART", 0x00010000, 92, 0, 0,
            current, backup, first_usable, last_usable, DISK.bytes_le,
            entries_lba, ENTRY_COUNT, ENTRY_BYTES, entries_crc)
        crc = zlib.crc32(fields) & 0xFFFFFFFF
        fields = fields[:16] + struct.pack("<I", crc) + fields[20:]
        return fields.ljust(BLOCK, b"\0")

    mbr = bytearray(BLOCK)
    mbr[446:462] = struct.pack("<BBBBBBBBII", 0, 0, 2, 0, 0xEE, 0xFF, 0xFF, 0xFF, 1,
                               min(blocks - 1, 0xFFFFFFFF))
    mbr[510:512] = b"\x55\xAA"
    with open(path, "wb") as f:
        f.truncate(blocks * BLOCK)
        f.seek(0)
        f.write(mbr)
        f.write(header(1, blocks - 1, 2))
        f.write(entries)
        f.seek(first * BLOCK)
        f.write(data)
        f.seek((blocks - 1 - entry_blocks) * BLOCK)
        f.write(entries)
        f.write(header(blocks - 1, 1, blocks - 1 - entry_blocks))


if __name__ == "__main__":
    main()
