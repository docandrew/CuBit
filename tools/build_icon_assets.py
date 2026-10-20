#!/usr/bin/env python3
"""Build the Assets/cubit-icons/<version>/ package (docs/assets.md).

applications.qoi is Desktop's application icon atlas: every icon of
tools/generate_desktop_icons.py's ICONS at every exported edge (EDGES),
straight-alpha RGBA. Each edge has its own column (left to right in EDGES
order); in it every icon's square is stacked top to bottom in
Desktop_Icons.Icon_ID order, and the rest of the column is transparent. The
encoded file is decoded again and compared with the source PNGs, so a
published atlas is exactly the original CuBit icons, pixel for pixel.
"""
import argparse
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import generate_desktop_icons as icons  # noqa: E402
import qoi  # noqa: E402


def atlas():
    width, height = icons.ATLAS_WIDTH, icons.ATLAS_HEIGHT
    pixels = bytearray(width * height * 4)
    for edge, left in zip(icons.EDGES, icons.columns()):
        for index, (_, path, _) in enumerate(icons.ICONS):
            square = icons.rgba(path, edge)
            for row in range(edge):
                at = ((index * edge + row) * width + left) * 4
                pixels[at:at + edge * 4] = square[row * edge * 4:(row + 1) * edge * 4]
    return bytes(pixels)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="package directory, e.g. build/Assets/cubit-icons/4")
    parser.add_argument("--reference", type=Path,
                        help="also write the atlas as applications.rgba here (test oracle)")
    args = parser.parse_args()
    if args.output.name != str(icons.ICON_PACKAGE_VERSION) or args.output.parent.name != icons.ICON_PACKAGE:
        raise SystemExit(f"output must be .../{icons.ICON_PACKAGE}/{icons.ICON_PACKAGE_VERSION}")
    width, height = icons.ATLAS_WIDTH, icons.ATLAS_HEIGHT
    pixels = atlas()
    encoded = qoi.encode(pixels, width, height, channels=qoi.CHANNELS_RGBA)
    if qoi.decode(encoded) != (width, height, pixels):
        raise SystemExit("icon atlas: QOI round trip mismatch")
    args.output.mkdir(parents=True, exist_ok=True)
    target = args.output / icons.ATLAS_FILE
    temporary = target.with_suffix(".qoi.tmp")
    temporary.write_bytes(encoded)
    temporary.replace(target)
    if args.reference:
        args.reference.mkdir(parents=True, exist_ok=True)
        (args.reference / "applications.rgba").write_bytes(pixels)
    print(f"{target}: {width}x{height} ({len(icons.ICONS)} icons), {len(encoded)} bytes "
          f"(raw {len(pixels)} bytes, edges {', '.join(str(e) for e in icons.EDGES)})")


if __name__ == "__main__":
    main()
