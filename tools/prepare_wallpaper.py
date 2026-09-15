#!/usr/bin/env python3
"""Convert the supplied wallpaper to a linkable, read-only BGRA pixel asset."""
import argparse
from pathlib import Path
from PIL import Image

parser = argparse.ArgumentParser()
parser.add_argument("source", type=Path)
parser.add_argument("output", type=Path)
parser.add_argument("--asset", choices=("cubes", "cubie"), default="cubes")
args = parser.parse_args()
source_size, raster_size, stem, symbol = {
    "cubes": ((5120, 1440), (2048, 576), "wallpaper", "cubit_desktop_wallpaper"),
    "cubie": ((3840, 2160), (2048, 1152), "wallpaper_cubie", "cubit_desktop_wallpaper_cubie"),
}[args.asset]
args.output.mkdir(parents=True, exist_ok=True)
with Image.open(args.source) as picture:
    if picture.size != source_size:
        parser.error(f"{args.asset} source must be {source_size[0]}x{source_size[1]}")
    rgba = picture.convert("RGBA")
    if rgba.getextrema()[3] != (255, 255):
        parser.error("desktop wallpaper must be opaque")
    # Keep the source PNG intact; bound the linked raster's memory cost.
    rgba = rgba.resize(raster_size, Image.Resampling.LANCZOS)
    (args.output / f"{stem}.bgra").write_bytes(rgba.tobytes("raw", "BGRA"))
asset = str((args.output / f"{stem}.bgra").resolve())
asset = asset.replace("\\", "\\\\").replace('"', '\\"')
(args.output / f"{stem}.S").write_text(
    f'.section .rodata\n.balign 16\n.global {symbol}\n'
    f'.type {symbol}, @object\n{symbol}:\n'
    f'.incbin "{asset}"\n'
    f'.size {symbol}, .-{symbol}\n'
    '.section .note.GNU-stack,"",@progbits\n')
