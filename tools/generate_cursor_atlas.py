#!/usr/bin/env python3
"""Generate CuBit's desktop cursor atlas from an Xcursor theme.

The generated pixels retain Xcursor's premultiplied ARGB representation so
the compositor can reproduce the source artwork without converting or
resampling it at runtime.
"""

from __future__ import annotations

import argparse
import pathlib
import struct
from dataclasses import dataclass


XCURSOR_MAGIC = 0x72756358
XCURSOR_IMAGE_TYPE = 0xFFFD0002


@dataclass(frozen=True)
class Cursor:
    ada_name: str
    source_name: str
    width: int
    height: int
    hotspot_x: int
    hotspot_y: int
    pixels: tuple[int, ...]


CURSORS = (
    ("Arrow", "left_ptr"),
    ("Text", "xterm"),
    ("Horizontal_Resize", "sb_h_double_arrow"),
    ("Vertical_Resize", "sb_v_double_arrow"),
    ("Diagonal_Resize", "bottom_right_corner"),
)


def load_cursor(path: pathlib.Path, nominal_size: int, ada_name: str) -> Cursor:
    data = path.read_bytes()
    if len(data) < 16:
        raise ValueError(f"{path}: truncated Xcursor header")

    magic, header_size, _version, toc_count = struct.unpack_from("<4I", data)
    if magic != XCURSOR_MAGIC or header_size < 16:
        raise ValueError(f"{path}: invalid Xcursor header")
    if header_size + toc_count * 12 > len(data):
        raise ValueError(f"{path}: truncated Xcursor table of contents")

    image_position = None
    for index in range(toc_count):
        item_type, subtype, position = struct.unpack_from(
            "<3I", data, header_size + index * 12
        )
        if item_type == XCURSOR_IMAGE_TYPE and subtype == nominal_size:
            image_position = position
            break

    if image_position is None:
        raise ValueError(f"{path}: no image with nominal size {nominal_size}")
    if image_position + 36 > len(data):
        raise ValueError(f"{path}: truncated image header")

    (
        chunk_header_size,
        item_type,
        subtype,
        _version,
        width,
        height,
        hotspot_x,
        hotspot_y,
        _delay,
    ) = struct.unpack_from("<9I", data, image_position)
    if (
        chunk_header_size < 36
        or item_type != XCURSOR_IMAGE_TYPE
        or subtype != nominal_size
        or width == 0
        or height == 0
        or hotspot_x >= width
        or hotspot_y >= height
    ):
        raise ValueError(f"{path}: invalid image metadata")

    pixel_position = image_position + chunk_header_size
    pixel_count = width * height
    if pixel_position + pixel_count * 4 > len(data):
        raise ValueError(f"{path}: truncated pixel data")
    pixels = struct.unpack_from(f"<{pixel_count}I", data, pixel_position)

    return Cursor(
        ada_name=ada_name,
        source_name=path.name,
        width=width,
        height=height,
        hotspot_x=hotspot_x,
        hotspot_y=hotspot_y,
        pixels=pixels,
    )


def ada_hex(value: int) -> str:
    digits = f"{value:08X}"
    return f"16#{digits[:4]}_{digits[4:]}#"


def generate(cursors: list[Cursor], nominal_size: int, source: str) -> str:
    max_width = max(cursor.width for cursor in cursors)
    max_height = max(cursor.height for cursor in cursors)
    offsets: list[int] = []
    next_offset = 0
    for cursor in cursors:
        offsets.append(next_offset)
        next_offset += len(cursor.pixels)

    lines = [
        "------------------------------------------------------------------------------",
        "--  CuBit",
        "--  Auto-generated Bluecurve cursor atlas. Do not edit by hand.",
        f"--  Source: {source}",
        f"--  Nominal Xcursor size: {nominal_size}",
        "--  Pixel format: premultiplied ARGB",
        "------------------------------------------------------------------------------",
        "with Interfaces;",
        "",
        "package Desktop_Cursors is",
        f"   MAX_WIDTH  : constant Positive := {max_width};",
        f"   MAX_HEIGHT : constant Positive := {max_height};",
        "",
        "   type Cursor_ID is (",
    ]
    for index, cursor in enumerate(cursors):
        separator = "," if index + 1 < len(cursors) else ""
        lines.append(f"      {cursor.ada_name}{separator}")
    lines.extend(
        [
            "   );",
            "",
            "   type Cursor_Metadata is record",
            "      Width     : Positive;",
            "      Height    : Positive;",
            "      Hotspot_X : Natural;",
            "      Hotspot_Y : Natural;",
            "      Offset    : Natural;",
            "   end record;",
            "",
            "   type Cursor_Metadata_Table is array (Cursor_ID) of Cursor_Metadata;",
            "   Metadata : constant Cursor_Metadata_Table := (",
        ]
    )
    for index, (cursor, offset) in enumerate(zip(cursors, offsets)):
        separator = "," if index + 1 < len(cursors) else ""
        lines.append(
            f"      {cursor.ada_name} => (Width => {cursor.width}, "
            f"Height => {cursor.height}, Hotspot_X => {cursor.hotspot_x}, "
            f"Hotspot_Y => {cursor.hotspot_y}, Offset => {offset}){separator}"
        )
    lines.extend(
        [
            "   );",
            "",
            "   type Pixel_Array is array (Natural range <>) of Interfaces.Unsigned_32;",
            f"   Pixels : constant Pixel_Array (0 .. {next_offset - 1}) := (",
        ]
    )

    all_pixels = [pixel for cursor in cursors for pixel in cursor.pixels]
    for start in range(0, len(all_pixels), 6):
        chunk = all_pixels[start : start + 6]
        rendered = ", ".join(ada_hex(pixel) for pixel in chunk)
        if start + len(chunk) < len(all_pixels):
            rendered += ","
        lines.append(f"      {rendered}")

    lines.extend(["   );", "end Desktop_Cursors;", ""])
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-dir", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--nominal-size", type=int, default=24)
    parser.add_argument(
        "--source-label",
        default="neeeeow/Bluecurve icons/icon-set/Bluecurve/cursors",
    )
    args = parser.parse_args()

    cursors = [
        load_cursor(args.source_dir / source_name, args.nominal_size, ada_name)
        for ada_name, source_name in CURSORS
    ]
    args.output.write_text(
        generate(cursors, args.nominal_size, args.source_label), encoding="utf-8"
    )


if __name__ == "__main__":
    main()
