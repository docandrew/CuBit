#!/usr/bin/env python3
"""Decode tests/ccl-remote/build/png-N.png (written by png_tests) with an
independent reader (zlib, chunk CRCs, the PNG filters) and compare each to
its expected RGB (png-N.rgb). Also usable on any 8-bit RGB PNG:
check_png.py FILE.png prints its size and distinct colours."""
import struct
import sys
import zlib
from pathlib import Path

BUILD = Path(__file__).resolve().parent / "build"
SIGNATURE = b"\x89PNG\r\n\x1a\n"


def decode(data):
    """(width, height, rgb bytes) of an 8-bit RGB, non-interlaced PNG."""
    if data[:8] != SIGNATURE:
        raise ValueError("no PNG signature")
    at, idat, header, ended = 8, b"", None, False
    while at < len(data):
        length, kind = struct.unpack(">I4s", data[at:at + 8])
        body = data[at + 8:at + 8 + length]
        crc, = struct.unpack(">I", data[at + 8 + length:at + 12 + length])
        if zlib.crc32(kind + body) != crc:
            raise ValueError(f"bad CRC in {kind!r}")
        if kind == b"IHDR":
            header = struct.unpack(">IIBBBBB", body)
        elif kind == b"IDAT":
            idat += body
        elif kind == b"IEND":
            ended = True
            break
        at += 12 + length
    if not ended or header is None:
        raise ValueError("truncated PNG")
    width, height, depth, colour, _, _, interlace = header
    if (depth, colour, interlace) != (8, 2, 0):
        raise ValueError("not 8-bit RGB")
    raw = zlib.decompress(idat)
    stride = 1 + 3 * width
    if len(raw) != stride * height:
        raise ValueError("wrong image data length")
    out, previous = bytearray(), bytearray(3 * width)
    for y in range(height):
        kind, row = raw[y * stride], bytearray(raw[y * stride + 1:(y + 1) * stride])
        for i in range(len(row)):
            left = row[i - 3] if i >= 3 else 0
            up = previous[i]
            up_left = previous[i - 3] if i >= 3 else 0
            if kind == 1:
                row[i] = (row[i] + left) & 255
            elif kind == 2:
                row[i] = (row[i] + up) & 255
            elif kind == 3:
                row[i] = (row[i] + (left + up) // 2) & 255
            elif kind == 4:
                p = left + up - up_left
                pa, pb, pc = abs(p - left), abs(p - up), abs(p - up_left)
                row[i] = (row[i] + (left if pa <= pb and pa <= pc else up if pb <= pc else up_left)) & 255
            elif kind != 0:
                raise ValueError("bad filter")
        out += row
        previous = row
    return width, height, bytes(out)


def main():
    if len(sys.argv) > 1:
        w, h, rgb = decode(Path(sys.argv[1]).read_bytes())
        colours = len({rgb[i:i + 3] for i in range(0, len(rgb), 3 * 7)})
        print(f"{sys.argv[1]}: {w}x{h}, {colours} distinct colours (sampled)")
        return 0
    failures = 0
    files = sorted(BUILD.glob("png-*.png"))
    for png in files:
        try:
            w, h, rgb = decode(png.read_bytes())
            ok = rgb == png.with_suffix(".rgb").read_bytes()
        except (ValueError, zlib.error) as error:
            ok, w, h = False, 0, 0
            print(f"  {png.name}: {error}")
        print(("PASS: " if ok else "FAIL: ") + f"{png.name} ({w}x{h}) decodes to its pixels")
        failures += not ok
    if not files:
        print("FAIL: no PNGs (run build/png_tests first)")
        failures = 1
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
