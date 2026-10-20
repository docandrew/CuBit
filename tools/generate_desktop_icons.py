#!/usr/bin/env python3
"""The desktop's application icons: the CuBit desktop icons
(assets/desktop-icons, made by its make.py) and Penny's own artwork
(assets/penny), straight-alpha, in Desktop_Icons.Icon_ID order, at every
exported edge (EDGES: 16, 24, 32, 48 and 64 pixels).

This writes Desktop_Icons (the Icon_ID literals and their names, the design
edge ICON_SIZE, the exported edges and the atlas layout) and
CCL.Interfaces.Desktop_Launch.Icons, the same names for the configuration
checker: a Launch_Entry's icon must name one of the atlas's icons. The pixels are not compiled in:
tools/build_icon_assets.py packs the same PNGs into
Assets/cubit-icons/<version>/applications.qoi (docs/assets.md), which
Desktop loads at startup. The atlas holds one column per edge, side by side
(EDGES order, left to right); in each column every icon's square is stacked
top to bottom in Icon_ID order. Desktop draws the edge nearest the output's
density 1:1 instead of scaling one size. Changing ICONS or EDGES changes the
atlas layout: bump ICON_PACKAGE_VERSION here and
Desktop_Icon_Assets.Package_Path together.

Run from the repository root inside nix develop, after make.py:
  python3 tools/generate_desktop_icons.py
"""
from pathlib import Path

from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
TARGET = ROOT / "userspace/services/desktop/desktop_icons.ads"
CCL_TARGET = ROOT / "userspace/ccl/src/ccl-interfaces-desktop_launch-icons.ads"
SIZE = 24          # the design edge: Desktop's 1x icon (Deskbar, Apps menu)
EDGES = (16, 24, 32, 48, 64)
ICON_PACKAGE = "cubit-icons"
ICON_PACKAGE_VERSION = 4
ATLAS_FILE = "applications.qoi"

# (Ada literal, PNG at SIZE, what it shows); the other edges are the same
# name with -<edge>.png. Start is the OS logo on the
# Apps button; Category_* head the Apps menu's category rows.
ICONS = (
    ("Start", "assets/desktop-icons/png/cubit-24.png", "CuBit logo: purple cube, one teal face"),
    ("Workbench", "assets/desktop-icons/png/workbench-24.png", "CCL Workbench"),
    ("Console", "assets/desktop-icons/png/console-24.png", "CCL Console"),
    ("Logs", "assets/desktop-icons/png/logs-24.png", "Logs"),
    ("Trace", "assets/desktop-icons/png/trace-24.png", "Desktop trace"),
    ("Doom", "assets/desktop-icons/png/doom-24.png", "DOOM"),
    ("Devices", "assets/desktop-icons/png/devices-24.png", "Devices"),
    ("Penny", "assets/penny/penny-24.png", "Penny (her own artwork)"),
    ("Files", "assets/desktop-icons/png/files-24.png", "Files"),
    ("Gameboy", "assets/desktop-icons/png/gameboy-24.png", "SameBoy"),
    ("Settings", "assets/desktop-icons/png/settings-24.png", "Settings"),
    ("Inspector", "assets/desktop-icons/png/inspector-24.png", "Config Inspector"),
    ("Mesa", "assets/desktop-icons/png/mesa-24.png", "Mesa demos"),
    ("Boot", "assets/desktop-icons/png/boot-24.png", "Boot Diagnostics"),
    ("Power", "assets/desktop-icons/png/power-24.png", "Power"),
    ("Category_System", "assets/desktop-icons/png/system-24.png", "System category"),
    ("Category_Development", "assets/desktop-icons/png/development-24.png", "Development category"),
    ("Category_Web", "assets/desktop-icons/png/web-24.png", "Web category"),
    ("Category_Games", "assets/desktop-icons/png/games-24.png", "Games category"),
    ("Category_Media", "assets/desktop-icons/png/media-24.png", "Media category"),
    ("Category_Tools", "assets/desktop-icons/png/tools-24.png", "Tools category"),
    ("Clipboard", "assets/desktop-icons/png/clipboard-24.png", "Clipboard manager"),
    ("Clip_Text", "assets/desktop-icons/png/clip-text-24.png", "Clipboard text entry"),
    ("Clip_Value", "assets/desktop-icons/png/clip-value-24.png", "Clipboard typed CCL value"),
    ("Clip_Mask", "assets/desktop-icons/png/clip-mask-24.png", "Clipboard secret (paste once)"),
    ("Clip_Pin", "assets/desktop-icons/png/clip-pin-24.png", "Pinned clipboard entry"),
    ("Text", "assets/desktop-icons/png/text-24.png", "Text editor"),
    ("Teapots", "assets/desktop-icons/png/teapots-24.png", "Teapots"),
    ("Calculator", "assets/desktop-icons/png/calculator-24.png", "Calculator"),
    ("Dictionary", "assets/desktop-icons/png/dictionary-24.png", "Dictionary"),
    ("Thesaurus", "assets/desktop-icons/png/thesaurus-24.png", "Thesaurus"),
    ("Agent", "assets/desktop-icons/png/agent-24.png", "An agent (CTX-001 task entries)"),
    ("Archive", "assets/desktop-icons/png/archive-24.png", "An archive"),
    ("Status_Wifi", "assets/desktop-icons/png/wifi-24.png", "Deskbar status: Wi-Fi"),
    ("Status_Battery", "assets/desktop-icons/png/battery-24.png", "Deskbar status: battery"),
    ("Status_Volume", "assets/desktop-icons/png/volume-24.png", "Deskbar status: volume"),
    ("Status_Muted", "assets/desktop-icons/png/volume-muted-24.png", "Deskbar status: volume muted"),
)


def at_edge(path, edge):
    """The PNG of the icon whose SIZE export is path, at edge pixels."""
    suffix = f"-{SIZE}.png"
    if not path.endswith(suffix):
        raise ValueError(f"{path}: expected a name ending {suffix}")
    return path[: -len(suffix)] + f"-{edge}.png"


def rgba(path, edge=SIZE):
    """The icon's straight-alpha RGBA bytes at edge pixels, row by row."""
    source = at_edge(path, edge)
    image = Image.open(ROOT / source).convert("RGBA")
    if image.size != (edge, edge):
        raise ValueError(f"{source}: expected {edge}x{edge}, got {image.size}")
    return image.tobytes("raw", "RGBA")


def columns():
    """Each edge's column: its left pixel in the atlas."""
    left, result = 0, []
    for edge in EDGES:
        result.append(left)
        left += edge
    return result


ATLAS_WIDTH = sum(EDGES)
ATLAS_HEIGHT = max(EDGES) * len(ICONS)


def generate():
    lines = [
        "------------------------------------------------------------------------------",
        "--  CuBit",
        "--  Generated by tools/generate_desktop_icons.py; do not edit by hand.",
        "--  Sources: assets/desktop-icons (CuBit originals, make.py) and",
        "--  assets/penny (Penny's own artwork). The pixels are the atlas",
        f"--  Assets/{ICON_PACKAGE}/{ICON_PACKAGE_VERSION}/{ATLAS_FILE} (tools/build_icon_assets.py),",
        "--  one column per exported edge (Edges, left to right at Columns), each",
        "--  holding one square per Icon_ID, top to bottom in this order.",
        "------------------------------------------------------------------------------",
        "package Desktop_Icons with Pure is",
        "   --  The design edge: the icon's size at 1x (Deskbar, Apps menu).",
        f"   ICON_SIZE : constant := {SIZE};",
        "   --  Every exported edge, ascending; drawn 1:1, never scaled.",
        f"   type Edge_Index is range 1 .. {len(EDGES)};",
        "   type Edge_Table is array (Edge_Index) of Positive;",
        "   Edges : constant Edge_Table := [" + ", ".join(str(e) for e in EDGES) + "];",
        "   --  Each edge's column in the atlas: its left pixel.",
        "   type Column_Table is array (Edge_Index) of Natural;",
        "   Columns : constant Column_Table := [" + ", ".join(str(c) for c in columns()) + "];",
        "   --  ICON_SIZE's place in Edges.",
        f"   Design_Edge : constant Edge_Index := {EDGES.index(SIZE) + 1};",
        f"   MAX_EDGE : constant := {max(EDGES)};",
        f"   ATLAS_WIDTH : constant := {ATLAS_WIDTH};",
        "   type Icon_ID is (",
    ]
    for index, (literal, _, shows) in enumerate(ICONS):
        lines.append(f"      {literal}{',' if index + 1 < len(ICONS) else ''}  --  {shows}")
    lines += ["   );",
              "   ATLAS_HEIGHT : constant := MAX_EDGE * Icon_ID'Range_Length;",
              "   --  Each icon's name, as a Launch_Entry's icon spells it",
              "   --  (CCL.Interfaces.Desktop_Launch.Icons; 'Image is numeric natively).",
              "   function Name_Of (Icon : Icon_ID) return String is",
              "     (case Icon is"]
    for index, (literal, _, _) in enumerate(ICONS):
        lines.append(f'         when {literal} => "{literal}"{"," if index + 1 < len(ICONS) else ");"}')
    lines += ["end Desktop_Icons;", ""]
    return "\n".join(lines)


def generate_ccl():
    """The icon names a Launch_Entry may use, for the configuration checker."""
    lines = [
        "------------------------------------------------------------------------------",
        "--  CuBit",
        "--  Generated by tools/generate_desktop_icons.py; do not edit by hand.",
        "--  The icons a Launch_Entry may name: every icon of the atlas",
        f"--  Assets/{ICON_PACKAGE}/{ICON_PACKAGE_VERSION}/{ATLAS_FILE}, spelled as",
        "--  Desktop_Icons.Name_Of spells it. CCL.Typed_Settings refuses any other",
        "--  name when a desktop.launch.* setting is checked or read back.",
        "------------------------------------------------------------------------------",
        "package CCL.Interfaces.Desktop_Launch.Icons with SPARK_Mode, Pure is",
        f'   ICON_PACKAGE : constant String := "{ICON_PACKAGE}/{ICON_PACKAGE_VERSION}";',
        "   function Known (Name : String) return Boolean is",
    ]
    for index, (literal, _, _) in enumerate(ICONS):
        lead = "     (" if index == 0 else "      or else "
        lines.append(f'{lead}Name = "{literal}"{");" if index + 1 == len(ICONS) else ""}')
    lines += ["end CCL.Interfaces.Desktop_Launch.Icons;", ""]
    return "\n".join(lines)


if __name__ == "__main__":
    TARGET.write_text(generate())
    CCL_TARGET.write_text(generate_ccl())
    print(f"{len(ICONS)} icon names -> {TARGET.relative_to(ROOT)} and {CCL_TARGET.relative_to(ROOT)}")
