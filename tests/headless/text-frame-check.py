#!/usr/bin/env python3
"""Text's frames after cursor moves (tests/headless/run.sh --test text).

The screendump is taken after Up, Down, Up, Down: exactly one band of the
current-line tint may show in Text's text area. A second band is a row a
presented buffer kept from an older frame (the flashing-lines bug: rows
repainted against the previous frame instead of the buffer drawn into).
"""
import sys

CURRENT_LINE = (0xFF, 0xF9, 0xE0)   # Alloy light current-line tint (Text_Editors palette)
COLUMNS = range(320, 900)           # right of the text, inside Text's window
MIN_SHARE = 0.9                     # a tinted pixel row: most of these columns
BAND_HEIGHT = range(15, 22)         # one text row (19 pixels, the glyph rows within)


def read_ppm(path):
    data = open(path, "rb").read()
    fields, pos = [], 0
    while len(fields) < 4:
        while data[pos:pos + 1].isspace():
            pos += 1
        if data[pos:pos + 1] == b"#":
            while data[pos:pos + 1] not in (b"\n", b""):
                pos += 1
            continue
        start = pos
        while not data[pos:pos + 1].isspace():
            pos += 1
        fields.append(data[start:pos])
    width, height = int(fields[1]), int(fields[2])
    return width, height, data[pos + 1:]


def main():
    width, height, rgb = read_ppm(sys.argv[1])
    tinted = []
    for y in range(height):
        row = rgb[y * width * 3:(y + 1) * width * 3]
        hits = sum(1 for x in COLUMNS if tuple(row[x * 3:x * 3 + 3]) == CURRENT_LINE)
        tinted.append(hits >= MIN_SHARE * len(COLUMNS))
    bands, start = [], None
    for y, t in enumerate(tinted + [False]):
        if t and start is None:
            start = y
        elif not t and start is not None:
            bands.append((start, y - start))
            start = None
    if len(bands) != 1 or bands[0][1] not in BAND_HEIGHT:
        print(f"text frame check: expected one current-line band, found {bands}")
        sys.exit(1)
    print(f"text frame check: one current-line band at y={bands[0][0]} height={bands[0][1]}")


if __name__ == "__main__":
    main()
