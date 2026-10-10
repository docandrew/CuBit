#!/usr/bin/env python3
"""Build the Assets/cubit-wallpapers/<version>/ package (docs/assets.md).

Each wallpaper source PNG is resized to the raster the desktop samples
(Desktop_Backdrop_Style: its width and height bound the decoded memory) and
written as a QOI image. The encoded file is decoded again and compared with
the raster, so a published asset is exactly the pixels the desktop expects.
"""
import argparse
from pathlib import Path
import sys

from PIL import Image

sys.path.insert(0, str(Path(__file__).resolve().parent))
import qoi  # noqa: E402

# name -> (source file, required source size, raster size). The raster sizes
# must match Desktop_Backdrop_Style (userspace/services/desktop).
WALLPAPERS = {
    "cubes": ("wallpaper2.png", (5120, 1440), (2048, 576)),
    "cubie": ("cubit-girl-wallpaper-4k-3840x2160.png", (3840, 2160), (2048, 1152)),
}


def raster(source, source_size, raster_size):
    with Image.open(source) as picture:
        if picture.size != source_size:
            raise SystemExit(f"{source}: must be {source_size[0]}x{source_size[1]}")
        rgba = picture.convert("RGBA")
        if rgba.getextrema()[3] != (255, 255):
            raise SystemExit(f"{source}: a desktop wallpaper must be opaque")
        # Keep the source PNG intact; bound the decoded raster's memory cost.
        return rgba.resize(raster_size, Image.Resampling.LANCZOS)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("sources", type=Path, help="directory of wallpaper PNGs (assets/wallpapers)")
    parser.add_argument("output", type=Path, help="package directory, e.g. build/Assets/cubit-wallpapers/1")
    parser.add_argument("--reference", type=Path,
                        help="also write each raster as <name>.rgba here (test oracle)")
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    for name, (file, source_size, raster_size) in WALLPAPERS.items():
        image = raster(args.sources / file, source_size, raster_size)
        pixels = image.tobytes("raw", "RGBA")
        encoded = qoi.encode(pixels, *raster_size, channels=qoi.CHANNELS_RGB)  # opaque
        if qoi.decode(encoded) != (*raster_size, pixels):
            raise SystemExit(f"{name}: QOI round trip mismatch")
        target = args.output / f"{name}.qoi"
        temporary = target.with_suffix(".qoi.tmp")
        temporary.write_bytes(encoded)
        temporary.replace(target)
        if args.reference:
            args.reference.mkdir(parents=True, exist_ok=True)
            (args.reference / f"{name}.rgba").write_bytes(pixels)
        print(f"{target}: {raster_size[0]}x{raster_size[1]}, {len(encoded)} bytes "
              f"(raw {len(pixels)} bytes)")


if __name__ == "__main__":
    main()
