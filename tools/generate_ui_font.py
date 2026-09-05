#!/usr/bin/env python3
"""Generate a bounded ASCII bitmap-font package for CuBit userspace."""

from __future__ import annotations

import argparse
import math
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


FIRST_GLYPH = 32
LAST_GLYPH = 126


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--font", required=True, type=Path)
    parser.add_argument("--package", required=True)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--pixel-size", type=int, default=13)
    parser.add_argument(
        "--cell-width",
        type=int,
        help="Emit a fixed-width font using this cell width",
    )
    parser.add_argument("--line-height", type=int, default=17)
    parser.add_argument(
        "--y-offset",
        type=int,
        default=0,
        help="Shift rasterized glyphs vertically within the line cell",
    )
    parser.add_argument("--source-name", required=True)
    parser.add_argument("--license-note", required=True)
    args = parser.parse_args()

    font = ImageFont.truetype(str(args.font), args.pixel_size)
    if args.cell_width is None:
        widths = {
            code: max(
                1,
                int(math.floor(font.getlength(chr(code)) + 0.5)),
                font.getbbox(chr(code))[2],
            )
            for code in range(FIRST_GLYPH, LAST_GLYPH + 1)
        }
        bitmap_width = max(widths.values())
    else:
        widths = None
        bitmap_width = args.cell_width

    glyphs: dict[int, list[list[int]]] = {}
    for code in range(FIRST_GLYPH, LAST_GLYPH + 1):
        image = Image.new("L", (bitmap_width, args.line_height), 0)
        ImageDraw.Draw(image).text(
            (0, args.y_offset), chr(code), font=font, fill=255
        )
        glyphs[code] = [
            [image.getpixel((x, y)) for x in range(bitmap_width)]
            for y in range(args.line_height)
        ]

    lines = [
        "------------------------------------------------------------------------------",
        "--  CuBit",
        "--  Auto-generated bitmap font. Do not edit by hand.",
        f"--  Source font: {args.source_name}",
        f"--  License note: {args.license_note}",
        "------------------------------------------------------------------------------",
        "with Interfaces;",
        "",
        f"package {args.package} is",
        f"   FIRST_GLYPH : constant := {FIRST_GLYPH};",
        f"   LAST_GLYPH  : constant := {LAST_GLYPH};",
        f"   GLYPH_HEIGHT : constant := {args.line_height};",
        (
            f"   GLYPH_WIDTH : constant := {bitmap_width};"
            if widths is None
            else f"   MAX_GLYPH_WIDTH : constant := {bitmap_width};"
        ),
        f"   LINE_HEIGHT : constant := {args.line_height};",
        "",
        "   subtype Glyph_Index is Natural range FIRST_GLYPH .. LAST_GLYPH;",
        "   subtype Glyph_Row_Index is Natural range 0 .. GLYPH_HEIGHT - 1;",
        (
            "   subtype Glyph_Column_Index is Natural range 0 .. GLYPH_WIDTH - 1;"
            if widths is None
            else "   subtype Glyph_Column_Index is Natural range 0 .. MAX_GLYPH_WIDTH - 1;"
        ),
        "",
    ]
    if widths is not None:
        lines.extend(
            [
                "   type Glyph_Width_Table is array (Glyph_Index) of Natural;",
                "   Widths : constant Glyph_Width_Table := (",
            ]
        )
        for code in range(FIRST_GLYPH, LAST_GLYPH + 1):
            suffix = "," if code < LAST_GLYPH else ""
            lines.append(f"      {code} => {widths[code]}{suffix}")
        lines.extend(["   );", ""])
    lines.extend([
        "   type Glyph_Row is array (Glyph_Column_Index) of Interfaces.Unsigned_8;",
        "   type Glyph_Bitmap is array (Glyph_Row_Index) of Glyph_Row;",
        "   type Font_Bitmap is array (Glyph_Index) of Glyph_Bitmap;",
        "",
        "   Alpha : constant Font_Bitmap := [",
    ])
    for code in range(FIRST_GLYPH, LAST_GLYPH + 1):
        lines.append(f"      {code} => [")
        for y, row in enumerate(glyphs[code]):
            suffix = "," if y + 1 < args.line_height else ""
            values = ", ".join(str(value) for value in row)
            lines.append(f"         {y} => [{values}]{suffix}")
        suffix = "," if code < LAST_GLYPH else ""
        lines.append(f"      ]{suffix}")
    lines.extend(["   ];", f"end {args.package};", ""])
    args.output.write_text("\n".join(lines), encoding="ascii")


if __name__ == "__main__":
    main()
