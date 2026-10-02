#!/usr/bin/env python3
"""Exact scanout wallpaper restoration outside Servo's restored window."""
from pathlib import Path

# Fixed 1024x768 native fixture. Outside the restored window and its shadow;
# inside regions covered by the larger window. Excludes cursor and taskbar.
REGIONS = ((912, 160, 960, 710), (110, 722, 900, 729))


def pixels(path):
    magic, dimensions, maximum, data = Path(path).read_bytes().split(b'\n', 3)
    assert magic == b'P6' and dimensions == b'1024 768' and maximum == b'255'
    assert len(data) == 1024 * 768 * 3
    return data


def compare(before, after):
    assert len(before) == len(after) == 1024 * 768 * 3
    checked = changed = 0
    for left, top, right, bottom in REGIONS:
        for y in range(top, bottom):
            for x in range(left, right):
                offset = (y * 1024 + x) * 3
                checked += 1
                changed += before[offset:offset + 3] != after[offset:offset + 3]
    return checked, changed


if __name__ == '__main__':
    import sys
    checked, changed = compare(pixels(sys.argv[1]), pixels(sys.argv[2]))
    assert changed == 0, f'{changed}/{checked} stale scanout pixels'
    print(f'SERVO-RESIZE-PIXELS: PASS {checked} exact wallpaper pixels')
