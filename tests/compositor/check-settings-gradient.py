"""Check the real default Settings preview gradient in a 100% primary scanout.

Input: P6 screenshot from check-dual-desktop.py's settings-appearance phase.
The expected Alloy Light endpoints are the public toolkit palette constants;
this oracle uses signed channel increments rather than the production blend.
"""
from pathlib import Path
import sys

magic, size, maximum, pixels = Path(sys.argv[1]).read_bytes().split(b"\n", 3)
width, height = map(int, size.split())
assert (magic, maximum) == (b"P6", b"255")
assert len(pixels) == width * height * 3
assert width >= 448 and height >= 213
# Settings starts at (96,72). Preview title occupies (293,193,154,20).
# Its right strip avoids the title text, borders and the parked cursor.
top, bottom = (0x47, 0x76, 0x8E), (0x29, 0x4E, 0x68)
checked = 0
for row in range(20):
    alpha = row * 255 // 19
    expected = bytes((a * 255 + (b - a) * alpha + 127) // 255
                     for a, b in zip(top, bottom))
    for x in range(400, 447):
        offset = ((193 + row) * width + x) * 3
        actual = pixels[offset:offset + 3]
        assert actual == expected, (x, row, tuple(actual), tuple(expected))
        checked += 1
print(f"SETTINGS-GRADIENT: PASS {checked} native scanout pixels, all 20 rows")
