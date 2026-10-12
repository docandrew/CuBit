#!/usr/bin/env python3
"""Build the Assets/cubit-icons/<version>/ package (docs/assets.md).

applications.qoi is Desktop's application icon atlas: every icon of
tools/generate_desktop_icons.py's ICONS, ICON_SIZE pixels square, stacked
top to bottom in Desktop_Icons.Icon_ID order, straight-alpha RGBA. The
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
    return b"".join(icons.rgba(path) for _, path, _ in icons.ICONS)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="package directory, e.g. build/Assets/cubit-icons/1")
    parser.add_argument("--reference", type=Path,
                        help="also write the atlas as applications.rgba here (test oracle)")
    args = parser.parse_args()
    if args.output.name != str(icons.ICON_PACKAGE_VERSION) or args.output.parent.name != icons.ICON_PACKAGE:
        raise SystemExit(f"output must be .../{icons.ICON_PACKAGE}/{icons.ICON_PACKAGE_VERSION}")
    width, height = icons.SIZE, icons.SIZE * len(icons.ICONS)
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
          f"(raw {len(pixels)} bytes)")


if __name__ == "__main__":
    main()
