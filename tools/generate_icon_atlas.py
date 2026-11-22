#!/usr/bin/env python3
"""Rasterize an icon set into CuBit.UI.Icons' compile-time atlas, at 16 and
32 pixels (the 32-pixel images serve canvases of density 2 and above, so
icons stay sharp).

The pipeline is asset-agnostic: each entry in SETS names a source directory,
a layout and the SVG basename for every CuBit.UI.Icons.Icon literal.
  "scaled": one SVG per icon, rasterized at every size (Bluecurve's 16x16).
  "sized":  hand-tuned SVG per size, <dir>/<size>/<name>.svg (cubit-icons).
Switching the toolkit's artwork is the one-line ICON_SET change below (or
--set NAME), then regenerating; CuBit.UI.Icons.Icon stays the same.

Run inside nix develop (Pillow and rsvg-convert):
  python3 tools/generate_icon_atlas.py > userspace/lib/ui/cubit-ui-icons-atlas.ads
  python3 tools/generate_icon_atlas.py --set cubit > ...
"""

import io
from pathlib import Path
import subprocess
import sys

from PIL import Image

ICON_SET = "cubit"

ASSETS = Path(__file__).resolve().parent.parent / "userspace/lib/ui/assets"

# (Ada literal, SVG basename) per set; the order is CuBit.UI.Icons.Icon's.
SETS = {
    "bluecurve": {
        "dir": ASSETS / "bluecurve",
        "layout": "scaled",
        "label": "Bluecurve @ 013ba225e78d9767b274ac6f16a67cb19f0673c6 (16x16 SVGs).",
        "see": "assets/bluecurve",
        "icons": (
            ("Go_Back", "stock-go-back"), ("Go_Forward", "stock-go-forward"), ("Go_Up", "stock_up-one-dir"),
            ("Refresh", "stock-refresh"), ("New_Folder", "stock_new-dir"), ("Copy", "stock-copy"),
            ("Move", "stock-cut"), ("Delete", "stock-delete"), ("Search", "stock_search"),
            ("Bookmarks", "stock-bookmarks"), ("Home", "stock-home"), ("Drawer", "stock-panel-drawer"),
            ("New_Tab", "stock-new-tab"), ("Columns", "stock_insert-columns"), ("Properties", "stock-properties"),
            ("Add_Pane", "stock-new-window"), ("Close", "stock-close"), ("Paste", "stock-paste"),
            ("Rename", "stock-edit"), ("History", "stock-history"), ("Folder", "folder"),
            ("File", "file-generic"), ("Drive", "harddrive"), ("Image_File", "file-gfx"),
            ("Document_File", "file-document"), ("Archive_File", "archive"), ("Program_File", "file-executable"),
            ("Trash", "trash-empty"), ("Network", "folder-network"), ("Recent", "folder-recent"),
            ("Favorites", "folder-favorites"), ("Home_Folder", "folder-home"),
        ),
    },
    "cubit": {
        "dir": ASSETS / "cubit-icons",
        "layout": "sized",
        "label": "CuBit original icon set (hand-tuned SVG per size).",
        "see": "assets/cubit-icons",
        "icons": (
            ("Go_Back", "go-back"), ("Go_Forward", "go-forward"), ("Go_Up", "go-up"),
            ("Refresh", "refresh"), ("New_Folder", "new-folder"), ("Copy", "copy"),
            ("Move", "cut"), ("Delete", "delete"), ("Search", "search"),
            ("Bookmarks", "bookmark"), ("Home", "home"), ("Drawer", "sidebar"),
            ("New_Tab", "new-tab"), ("Columns", "columns"), ("Properties", "info"),
            ("Add_Pane", "split"), ("Close", "close"), ("Paste", "paste"),
            ("Rename", "rename"), ("History", "recent"), ("Folder", "folder"),
            ("File", "file"), ("Drive", "drive"), ("Image_File", "file-image"),
            ("Document_File", "file-document"), ("Archive_File", "file-archive"), ("Program_File", "file-executable"),
            ("Trash", "trash-empty"), ("Network", "network"), ("Recent", "recent"),
            ("Favorites", "bookmark"), ("Home_Folder", "home"),
        ),
    },
}
SIZES = (16, 32)


class Art:
    """An icon supplied as existing per-size artwork rather than set SVGs:
    <dir>/<stem>-<size>.png used as is when that size exists, otherwise
    <dir>/<stem>.svg rasterized. Use it in place of a basename in "icons",
    e.g. ("Penny", Art(REPO / "assets/penny", "penny"))."""

    def __init__(self, directory, stem):
        self.directory, self.stem = Path(directory), stem

    def __str__(self):
        return self.stem


REPO = ASSETS.parents[3]


def source(name, size):
    if isinstance(name, Art):
        png = name.directory / f"{name.stem}-{size}.png"
        return png if png.exists() else name.directory / f"{name.stem}.svg"
    spec = SETS[ICON_SET]
    if spec["layout"] == "sized":
        return spec["dir"] / str(size) / (name + ".svg")
    return spec["dir"] / (name + ".svg")


def raster(name, size):
    path = source(name, size)
    if path.suffix == ".png":
        image = Image.open(path).convert("RGBA")
    else:
        data = subprocess.check_output(["rsvg-convert", "-w", str(size), "-h", str(size), str(path)])
        image = Image.open(io.BytesIO(data)).convert("RGBA")
    if image.size != (size, size):
        raise ValueError(f"{name}: expected {size}x{size}, got {image.size}")
    return image


def table(size):
    lines = [f"   Pixels_{size} : constant Table_{size} := ["]
    icons = SETS[ICON_SET]["icons"]
    for index, (literal, name) in enumerate(icons):
        image = raster(name, size)
        lines.append(f"      --  {source(name, size).name if isinstance(name, Art) else str(name) + '.svg'} at {size}x{size}; "
                     "straight (not premultiplied) ARGB.")
        lines.append(f"      {literal} => [")
        for y in range(size):
            row = []
            for x in range(size):
                r, g, b, a = image.getpixel((x, y))
                row.append(f"16#{a:02X}{r:02X}_{g:02X}{b:02X}#")
            chunks = [", ".join(row[i:i + 8]) for i in range(0, size, 8)]
            lines.append("         [" + (",\n          ").join(chunks) + "]" + ("," if y + 1 < size else ""))
        lines.append("      ]" + ("," if index + 1 < len(icons) else ""))
    lines.append("   ];")
    return lines


def generate():
    lines = [
        "--  Generated by tools/generate_icon_atlas.py; do not edit.",
        "--  " + SETS[ICON_SET]["label"],
        "--  See ASSET_LICENSES.md and " + SETS[ICON_SET]["see"] + " for source artwork.",
        "private package CuBit.UI.Icons.Atlas is",
    ]
    for size in SIZES:
        lines.append(f"   type Table_{size} is array (Icon) of CuBit.UI.ARGB_Bitmap (0 .. {size - 1}, 0 .. {size - 1});")
    for size in SIZES:
        lines += table(size)
    lines += ["end CuBit.UI.Icons.Atlas;", ""]
    return "\n".join(lines)


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--set" and sys.argv[2] in SETS:
        ICON_SET = sys.argv[2]
    elif len(sys.argv) != 1:
        sys.exit("usage: generate_icon_atlas.py [--set " + "|".join(SETS) + "]")
    print(generate(), end="")
