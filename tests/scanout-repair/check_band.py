#!/usr/bin/env python3
"""Headless scanout-repair: no black band left by the kernel's stuck-call panel.

The kernel erases its panel (Boot_Diagnostics.Stuck_Calls) with black rows
across the bottom of the boot framebuffer. After display.svc's next present
the desktop must cover them again, so no row of the screenshot may be black
across most of its width.
"""
import sys

# A row this black across this share of its width is the erased panel; the
# desktop's wallpaper and windows are never pure black over such a span.
BLACK_LEVEL = 8
BLACK_SHARE = 0.5
SAMPLE_STEP = 4


def load_ppm(path):
    with open(path, 'rb') as f:
        data = f.read()
    fields = data.split(maxsplit=4)
    if fields[0] != b'P6' or int(fields[3]) != 255:
        raise SystemExit(f"scanout-repair: {path}: not an 8-bit P6 image")
    return int(fields[1]), int(fields[2]), fields[4]


def main():
    width, height, pixels = load_ppm(sys.argv[1])
    black_rows = []
    for y in range(height):
        row = pixels[y * width * 3:(y + 1) * width * 3]
        samples = range(0, width * 3, 3 * SAMPLE_STEP)
        black = sum(1 for x in samples
                    if max(row[x], row[x + 1], row[x + 2]) < BLACK_LEVEL)
        if black > len(samples) * BLACK_SHARE:
            black_rows.append(y)
    if black_rows:
        print(f"scanout-repair: FAIL {len(black_rows)} black rows "
              f"({black_rows[0]}..{black_rows[-1]}) of {width}x{height}")
        return 1
    print(f"scanout-repair: PASS no black rows in {width}x{height}")
    return 0


if __name__ == '__main__':
    sys.exit(main())
