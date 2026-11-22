#!/usr/bin/env python3
"""Writes the headless UFS test disk: a GPT image laid out like the IdeaPad's
Windows disk (EFI system, Microsoft reserved, basic data, recovery), with a
protective MBR and valid header and entry-array CRC32s.

usage: make-disk.py IMAGE [BLOCK_BYTES]

The partitions hold zeros; the test only reads the table. BLOCK_BYTES is the
logical block size the QEMU ufs-lu reports (4096 by default).
"""
import struct
import sys
import uuid
import zlib

MIB = 1024 * 1024
IMAGE_BYTES = 64 * MIB
ENTRY_COUNT = 128
ENTRY_BYTES = 128

EFI_SYSTEM = uuid.UUID("C12A7328-F81F-11D2-BA4B-00A0C93EC93B")
MS_RESERVED = uuid.UUID("E3C9E316-0B5C-4DB8-817D-F92DF00215AE")
MS_BASIC_DATA = uuid.UUID("EBD0A0A2-B9E5-4433-87C0-68B6B72699C7")
WINDOWS_RECOVERY = uuid.UUID("DE94BBA4-06D1-4D40-A16A-BFD50179D6AC")
DISK = uuid.UUID("5B0E7C2A-2F4D-4C38-9E5A-0123456789AB")


def main() -> None:
    path = sys.argv[1]
    block = int(sys.argv[2]) if len(sys.argv) > 2 else 4096
    blocks = IMAGE_BYTES // block
    entry_blocks = (ENTRY_COUNT * ENTRY_BYTES + block - 1) // block
    first_usable = 2 + entry_blocks
    last_usable = blocks - 2 - entry_blocks
    per_mib = MIB // block

    # name, type, first block, last block (1 MiB aligned, as Windows lays out)
    layout = [
        ("EFI system partition", EFI_SYSTEM, 1 * per_mib, 9 * per_mib - 1),
        ("Microsoft reserved partition", MS_RESERVED, 9 * per_mib, 13 * per_mib - 1),
        ("Basic data partition", MS_BASIC_DATA, 13 * per_mib, 56 * per_mib - 1),
        ("Recovery", WINDOWS_RECOVERY, 56 * per_mib, last_usable),
    ]
    entries = bytearray(ENTRY_COUNT * ENTRY_BYTES)
    for index, (name, kind, first, last) in enumerate(layout):
        unique = uuid.UUID(int=index + 1)
        entry = struct.pack("<16s16sQQQ72s", kind.bytes_le, unique.bytes_le,
                            first, last, 0, name.encode("utf-16-le"))
        entries[index * ENTRY_BYTES:(index + 1) * ENTRY_BYTES] = entry
    entries_crc = zlib.crc32(entries) & 0xFFFFFFFF

    def header(current: int, backup: int, entries_lba: int) -> bytes:
        fields = struct.pack(
            "<8sIIIIQQQQ16sQIII", b"EFI PART", 0x00010000, 92, 0, 0,
            current, backup, first_usable, last_usable, DISK.bytes_le,
            entries_lba, ENTRY_COUNT, ENTRY_BYTES, entries_crc)
        crc = zlib.crc32(fields) & 0xFFFFFFFF
        fields = fields[:16] + struct.pack("<I", crc) + fields[20:]
        return fields.ljust(block, b"\0")

    mbr = bytearray(512)
    # One protective record (type EEh) covering the disk, in 512-byte LBAs.
    sectors = min(IMAGE_BYTES // 512 - 1, 0xFFFFFFFF)
    mbr[446:462] = struct.pack("<B3sB3sII", 0, b"\x00\x02\x00", 0xEE,
                               b"\xff\xff\xff", 1, sectors)
    mbr[510:512] = b"\x55\xaa"

    image = bytearray(IMAGE_BYTES)
    image[0:512] = mbr
    image[block:2 * block] = header(1, blocks - 1, 2)
    image[2 * block:2 * block + len(entries)] = entries
    backup_entries = blocks - 1 - entry_blocks
    image[backup_entries * block:backup_entries * block + len(entries)] = entries
    image[(blocks - 1) * block:blocks * block] = header(blocks - 1, 1, backup_entries)
    with open(path, "wb") as out:
        out.write(image)


if __name__ == "__main__":
    main()
