#!/usr/bin/env python3
"""Build the Assets/cubit-wallpapers/<version>/ package.

The wallpapers are declared once, in the source list <sources>/wallpapers.ccl
(a typed CCL Wallpaper_Sources, CCL.Interfaces.Wallpapers). ccl-manifest
--wallpaper-catalog checks it with the CCL type system and writes the
package's catalog.ccl (one canonical Wallpaper per line, read by the desktop
at run time); this script turns its raster plan into the QOI files. Each
source PNG is resized to its declared raster (which bounds the desktop's
decoded memory) and written as <name>.qoi; the encoded file is decoded again
and compared with the raster, so a published asset is exactly the pixels the
desktop will show. Files of earlier builds that the catalog no longer names
are removed, so the package is exactly the catalog.
"""
import argparse
from pathlib import Path
import subprocess
import sys

from PIL import Image

sys.path.insert(0, str(Path(__file__).resolve().parent))
import qoi  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
SOURCE_LIST = "wallpapers.ccl"
CATALOG = "catalog.ccl"
EXTENSION = ".qoi"
DEFAULT_TOOL = ROOT / "userspace/ccl/build/manifest/ccl-manifest"
PLAN_FIELDS = 4  # name, source, width, height (Asset_Catalogs.Build_Wallpapers)


def plan(tool, sources, output):
    """Check the source list, write the catalog, return the raster plan."""
    result = subprocess.run([str(tool), "--wallpaper-catalog", str(sources / SOURCE_LIST),
                             str(output / CATALOG)], capture_output=True, text=True, timeout=120)
    if result.returncode:
        raise SystemExit(result.stderr.strip() or f"{tool}: wallpaper catalog refused")
    rows = [line.split("\t") for line in result.stdout.splitlines()]
    if not rows or any(len(row) != PLAN_FIELDS for row in rows):
        raise SystemExit(f"{tool}: unexpected raster plan {result.stdout!r}")
    return [(name, source, (int(width), int(height))) for name, source, width, height in rows]


def raster(source, raster_size):
    with Image.open(source) as picture:
        rgba = picture.convert("RGBA")
        if rgba.getextrema()[3] != (255, 255):
            raise SystemExit(f"{source}: a desktop wallpaper must be opaque")
        # Keep the source PNG intact; bound the decoded raster's memory cost.
        return rgba.resize(raster_size, Image.Resampling.LANCZOS)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("sources", type=Path,
                        help=f"directory of wallpaper PNGs and {SOURCE_LIST} (assets/wallpapers)")
    parser.add_argument("output", type=Path, help="package directory, e.g. build/Assets/cubit-wallpapers/2")
    parser.add_argument("--reference", type=Path,
                        help="also write each raster as <name>.rgba here (test oracle)")
    parser.add_argument("--ccl-manifest", type=Path, default=DEFAULT_TOOL,
                        help="the ccl-manifest tool (make -C kernel ccl-manifest)")
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    wallpapers = plan(args.ccl_manifest, args.sources, args.output)
    published = {CATALOG}
    for name, file, raster_size in wallpapers:
        image = raster(args.sources / file, raster_size)
        pixels = image.tobytes("raw", "RGBA")
        encoded = qoi.encode(pixels, *raster_size, channels=qoi.CHANNELS_RGB)  # opaque
        if qoi.decode(encoded) != (*raster_size, pixels):
            raise SystemExit(f"{name}: QOI round trip mismatch")
        target = args.output / f"{name}{EXTENSION}"
        temporary = target.with_suffix(EXTENSION + ".tmp")
        temporary.write_bytes(encoded)
        temporary.replace(target)
        published.add(target.name)
        if args.reference:
            args.reference.mkdir(parents=True, exist_ok=True)
            (args.reference / f"{name}.rgba").write_bytes(pixels)
        print(f"{target}: {raster_size[0]}x{raster_size[1]}, {len(encoded)} bytes "
              f"(raw {len(pixels)} bytes)")
    for stale in args.output.iterdir():
        if stale.is_file() and stale.name not in published:
            stale.unlink()
    print(f"{args.output / CATALOG}: {len(wallpapers)} wallpaper(s)")


if __name__ == "__main__":
    main()
