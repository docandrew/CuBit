#!/usr/bin/env python3
"""Write the QOI decoder test fixtures (tests/qoi/README.md).

Valid images are encoded twice, by tools/qoi.py and by Pillow's independent
QOI encoder, and each comes with its RGBA pixels. Malformed streams name the
failure the decoder must report. The manifest is one line per fixture:
  <file> <width> <height> <limit> <expected: Complete or a Failure name>
"""
import argparse
import io
from pathlib import Path
import random
import struct
import sys

from PIL import Image

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools"))
import qoi  # noqa: E402

SEED = 20261009


def images(rng):
    """(name, width, height, rgba) cases covering every chunk kind."""
    def solid(w, h, px):
        return bytes(px) * (w * h)
    yield "one-pixel", 1, 1, bytes((9, 8, 7, 255))
    # The initial previous pixel, so the stream starts with a run.
    yield "start-run", 70, 3, solid(70, 3, (0, 0, 0, 255))
    # (0,0,0,0) hashes to slot 0, which starts as (0,0,0,0): OP_INDEX first.
    yield "transparent-index", 5, 5, solid(5, 5, (0, 0, 0, 0))
    # Runs longer than 62 that end exactly on the last pixel.
    yield "long-runs", 125, 2, solid(125, 2, (200, 10, 30, 255))
    gradient = bytearray()
    for y in range(37):
        for x in range(61):
            gradient += bytes(((x * 4) % 256, (y * 7) % 256, (x + y) % 256, 255))
    yield "gradient", 61, 37, bytes(gradient)   # OP_DIFF and OP_LUMA
    noise = bytes(rng.randrange(256) if i % 4 != 3 else 255 for i in range(64 * 48 * 4))
    yield "noise-rgb", 64, 48, noise            # OP_RGB, OP_INDEX
    alpha = bytes(rng.randrange(256) for _ in range(33 * 17 * 4))
    yield "noise-rgba", 33, 17, alpha           # OP_RGBA
    mixed = bytearray()
    palette = [tuple(rng.randrange(256) for _ in range(3)) + (255,) for _ in range(9)]
    for _ in range(97 * 23):
        mixed += bytes(palette[rng.randrange(len(palette))] if rng.random() < 0.7
                       else (rng.randrange(256), rng.randrange(256), rng.randrange(256),
                             rng.choice((255, 128))))
    yield "palette", 97, 23, bytes(mixed)


def header(width, height, channels=qoi.CHANNELS_RGBA, colorspace=qoi.SRGB, magic=qoi.MAGIC):
    return magic + struct.pack(">IIBB", width, height, channels, colorspace)


def malformed(valid):
    """(name, width, height, limit, data, expected failure)."""
    w, h, data = valid
    body = data[14:]
    yield "bad-magic", "Bad_Magic", 4, b"qoix" + data[4:]
    yield "zero-width", "Bad_Size", 4, header(0, 1) + qoi.END_MARKER
    yield "zero-height", "Bad_Size", 4, header(1, 0) + qoi.END_MARKER
    yield "huge-side", "Bad_Size", 2**26, header(2**31, 1) + qoi.END_MARKER
    yield "wide-side", "Bad_Size", 2**26, header(16_385, 1) + qoi.END_MARKER
    yield "over-limit", "Over_Limit", w * h - 1, data
    yield "bad-channels", "Bad_Channels", w * h, header(w, h, channels=5) + body
    yield "bad-colorspace", "Bad_Colorspace", w * h, header(w, h, colorspace=2) + body
    yield "run-past-end", "Run_Past_End", 8, header(2, 2) + bytes((qoi.OP_RUN | 4,)) + qoi.END_MARKER
    yield "bad-marker", "Bad_Marker", w * h, data[:-1] + b"\x02"
    yield "missing-marker-run", "Bad_Marker", 4, header(2, 2) + bytes((qoi.OP_RUN | 3, qoi.OP_RUN | 0)) + qoi.END_MARKER
    yield "trailing", "Trailing_Data", w * h, data + b"\x00"
    yield "truncated-header", "Truncated", 4, data[:9]
    yield "truncated-chunk", "Truncated", 4, header(1, 1) + bytes((qoi.OP_RGBA, 1, 2))
    yield "truncated-marker", "Truncated", w * h, data[:-3]
    yield "empty", "Truncated", 4, b""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    rng = random.Random(SEED)
    lines = []
    reference = None
    for name, w, h, rgba in images(rng):
        ours = qoi.encode(rgba, w, h)
        assert qoi.decode(ours) == (w, h, rgba), name
        stream = io.BytesIO()
        Image.frombytes("RGBA", (w, h), rgba).save(stream, "QOI")
        pillow = stream.getvalue()
        assert qoi.decode(pillow)[2] == rgba, name + " (Pillow)"
        (args.output / f"{name}.rgba").write_bytes(rgba)
        for encoder, data in (("ours", ours), ("pillow", pillow)):
            (args.output / f"{name}.{encoder}.qoi").write_bytes(data)
            lines.append(f"{name}.{encoder}.qoi {name}.rgba {w} {h} {w * h} Complete")
        if name == "palette":
            reference = (w, h, ours)
    for name, failure, limit, data in malformed(reference):
        (args.output / f"{name}.qoi").write_bytes(data)
        lines.append(f"{name}.qoi none 0 0 {limit} {failure}")
    (args.output / "manifest.txt").write_text("\n".join(lines) + "\n")
    print(f"{len(lines)} fixtures in {args.output}")


if __name__ == "__main__":
    main()
