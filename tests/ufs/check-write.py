#!/usr/bin/env python3
"""Checks what the headless ufs-write test's guest (disk-write-check) wrote
into the UFS image: the pattern in blocks 4096..4100, and nothing written
outside them (the GPT that make-disk.py laid down is intact).

usage: check-write.py IMAGE [BLOCK_BYTES]
"""
import subprocess
import sys
import tempfile
from pathlib import Path

# userspace/apps/disk-write-check/main.adb: Pattern_Block, Pattern_Blocks
# plus the one FUA block, Magic, Offset_Stride.
PATTERN_BLOCK = 4096
PATTERN_BLOCKS = 4 + 1
MAGIC = b"CUBIT-UFS-WRITE "
OFFSET_STRIDE = 7
BYTE_VALUES = 256


def expected(block: int, size: int) -> bytes:
    body = bytes((offset * OFFSET_STRIDE + block) % BYTE_VALUES
                 for offset in range(len(MAGIC), size))
    return MAGIC + body


def main() -> int:
    image = Path(sys.argv[1]).read_bytes()
    size = int(sys.argv[2]) if len(sys.argv) > 2 else 4096
    failures = 0
    for block in range(PATTERN_BLOCK, PATTERN_BLOCK + PATTERN_BLOCKS):
        if image[block * size:(block + 1) * size] != expected(block, size):
            print(f"check-write: block {block} does not hold the pattern")
            failures += 1
    # Everything else is as make-disk.py wrote it.
    with tempfile.TemporaryDirectory() as scratch:
        fresh_path = Path(scratch) / "fresh.img"
        subprocess.run([sys.executable, "-I",
                        str(Path(__file__).with_name("make-disk.py")),
                        str(fresh_path), str(size)], check=True)
        fresh = bytearray(fresh_path.read_bytes())
    first, last = PATTERN_BLOCK * size, (PATTERN_BLOCK + PATTERN_BLOCKS) * size
    fresh[first:last] = image[first:last]
    if len(image) != len(fresh) or image != bytes(fresh):
        print("check-write: the image changed outside the written blocks")
        failures += 1
    if failures == 0:
        print(f"check-write: PASS ({PATTERN_BLOCKS} blocks on the medium, "
              "nothing else changed)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
