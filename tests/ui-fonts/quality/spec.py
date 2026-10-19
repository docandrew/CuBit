# Shared sample spec: (face, fg, bg, text). face: sans | mono | semi
# (semi: IBM Plex Sans SemiBold, Desktop's emphasis face; only Desktop draws
# it, so the toolkit render shows those rows through the Desktop text path)
DARK_ON_LIGHT = (0x1E2228, 0xF2F2F0)
LIGHT_ON_DARK = (0xE8EAF0, 0x343B4D)
SAMPLES = [
    ("sans", *DARK_ON_LIGHT, "File  Edit  View  Go  Window  Help"),
    ("sans", *DARK_ON_LIGHT, "Open Recent...   Save As...   Find and Replace"),
    ("sans", *DARK_ON_LIGHT, "Documents   Pictures   readme.txt   4.2 KiB   2026-10-18"),
    ("sans", *DARK_ON_LIGHT, "AVATAR Type Wave LTA To. Yes, VAT ff fi rn m"),
    ("sans", *DARK_ON_LIGHT, "The quick brown fox jumps over the lazy dog"),
    ("sans", *LIGHT_ON_DARK, "CuBit Text - notes.txt"),
    ("sans", *LIGHT_ON_DARK, "Files - Home   Workbench   Settings"),
    ("sans", *LIGHT_ON_DARK, "The quick brown fox jumps over the lazy dog"),
    ("semi", *DARK_ON_LIGHT, "CCL Workbench   Settings   Files   Text"),
    ("semi", *LIGHT_ON_DARK, "Files - Home   Notifications   12:45"),
    ("mono", *DARK_ON_LIGHT, "procedure Insert (T : in out Table) is"),
    ("mono", *LIGHT_ON_DARK, "   Count : constant Natural := 0; -- ok"),
]
WIDTH = 480
ROW = 24
TOP = 4
LEFT = 8

# Density of a render (FONT_QUALITY_SCALE="n/d"): the reference and the
# metrics use these physical sizes. Only densities that keep ROW, TOP, LEFT
# and the mono cell whole (1, 5/4, 3/2, 2) are meaningful.
import os as _os
_n, _d = (int(v) for v in _os.environ.get("FONT_QUALITY_SCALE", "1/1").split("/"))
SCALE = (_n, _d)
def phys(v): return v * _n // _d
ROW_PX, TOP_PX, LEFT_PX, WIDTH_PX = phys(ROW), phys(TOP), phys(LEFT), phys(WIDTH)
