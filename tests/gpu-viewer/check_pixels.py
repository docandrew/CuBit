#!/usr/bin/env python3
"""Verify the synthetic viewer's 32x32 magenta square in a QEMU P6 capture.

This is a compositor/viewer oracle, not evidence of Intel GPU execution.
"""
from pathlib import Path
import sys


def check(data: bytes) -> None:
    # QEMU screendump's P6 header is three LF-terminated lines.
    parts = data.split(b"\n", 3)
    if len(parts) != 4 or parts[0] != b"P6" or parts[2] != b"255":
        raise ValueError("not a QEMU RGB8 P6 screenshot")
    dimensions = parts[1].split()
    if len(dimensions) != 2:
        raise ValueError("invalid dimensions")
    width, height = map(int, dimensions)
    if not (32 <= width <= 16384 and 32 <= height <= 16384):
        raise ValueError("dimensions outside diagnostic bounds")
    pixels = parts[3]
    if len(pixels) != width * height * 3:
        raise ValueError("truncated or trailing pixel data")
    points = [(i % width, i // width) for i in range(width * height)
              if pixels[3*i:3*i+3] == b"\xff\x00\xff"]
    if len(points) != 1024:
        raise ValueError(f"expected 1024 magenta pixels, found {len(points)}")
    left, top = min(x for x, _ in points), min(y for _, y in points)
    right, bottom = max(x for x, _ in points), max(y for _, y in points)
    if right - left != 31 or bottom - top != 31:
        raise ValueError("magenta pixels do not form the expected 32x32 square")


def main() -> None:
    try:
        check(Path(sys.argv[1]).read_bytes())
    except (IndexError, OSError, ValueError) as error:
        raise SystemExit(f"GPU-VIEWER-PIXELS: FAIL: {error}") from error
    print("GPU-VIEWER-PIXELS: PASS (synthetic RAM, not Intel rendering)")


if __name__ == "__main__":
    main()
