#!/usr/bin/env python3
"""Convert the supplied wallpaper to a linkable, read-only BGRA pixel asset."""
import argparse
from pathlib import Path
from PIL import Image

parser = argparse.ArgumentParser()
parser.add_argument("source", type=Path)
parser.add_argument("output", type=Path)
args = parser.parse_args()
args.output.mkdir(parents=True, exist_ok=True)
with Image.open(args.source) as picture:
    if picture.size != (5120, 1440):
        parser.error("wallpaper2 source must be 5120x1440")
    rgba = picture.convert("RGBA")
    if rgba.getextrema()[3] != (255, 255):
        parser.error("desktop wallpaper must be opaque")
    # Keep the source PNG intact; bound the linked raster's memory cost.
    rgba = rgba.resize((2048, 576), Image.Resampling.LANCZOS)
    (args.output / "wallpaper.bgra").write_bytes(rgba.tobytes("raw", "BGRA"))
asset = str((args.output / "wallpaper.bgra").resolve())
asset = asset.replace("\\", "\\\\").replace('"', '\\"')
(args.output / "wallpaper.S").write_text(
    '.section .rodata\n.balign 16\n.global cubit_desktop_wallpaper\n'
    '.type cubit_desktop_wallpaper, @object\ncubit_desktop_wallpaper:\n'
    f'.incbin "{asset}"\n'
    '.size cubit_desktop_wallpaper, .-cubit_desktop_wallpaper\n'
    '.section .note.GNU-stack,"",@progbits\n')
