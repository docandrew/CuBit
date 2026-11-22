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
# The status panel at 1x (UI-032): its real strings and colours, sampled from
# the QEMU screendumps (panel #D1D5D7, primary ink #172026, secondary
# #4F5B64, field #FFFFFF, button #DEE1E3) plus the dark title-bar colours.
PANEL_BG, FIELD_BG, BUTTON_BG = 0xD1D5D7, 0xFFFFFF, 0xDEE1E3
PRIMARY, SECONDARY = 0x172026, 0x4F5B64
PANEL = [
    ("semi", SECONDARY, PANEL_BG, "Quick settings   Network   Notifications"),
    ("sans", PRIMARY, PANEL_BG, "Do not disturb   Memory   CPU   Mute   100%"),
    ("sans", SECONDARY, PANEL_BG, "Addressing   Gateway   DNS (all)   No notifications"),
    ("sans", PRIMARY, FIELD_BG, "10.0.2.15/24   10.0.2.2   10.0.2.3"),
    ("sans", PRIMARY, BUTTON_BG, "Apply   Revert   DHCP   Static   Off"),
    ("semi", PRIMARY, PANEL_BG, "Wired   pci-00-10-0   Static"),
    ("sans", *LIGHT_ON_DARK, "Do not disturb   Addressing   10.0.2.15/24"),
    ("semi", *LIGHT_ON_DARK, "Quick settings   Notifications   Apply"),
]
import os as _os
if _os.environ.get("FONT_QUALITY_SET") == "panel":
    SAMPLES = PANEL
# The UI text size (CuBit.UI_Metrics.Base_Em) and the monospace grid
# (Mono_Advance); rows hold the 21-pixel line with room around it.
BASE_EM = 16
MONO_ADVANCE = 10
WIDTH = 640
ROW = 28
TOP = 4
LEFT = 8

# Density of a render (FONT_QUALITY_SCALE="n/d"): the reference and the
# metrics use these physical sizes. Only densities that keep ROW, TOP and
# LEFT whole (1, 5/4, 3/2, 2) are meaningful.
import os as _os
_n, _d = (int(v) for v in _os.environ.get("FONT_QUALITY_SCALE", "1/1").split("/"))
SCALE = (_n, _d)
def phys(v): return v * _n // _d
ROW_PX, TOP_PX, LEFT_PX, WIDTH_PX = phys(ROW), phys(TOP), phys(LEFT), phys(WIDTH)
