#!/usr/bin/env python3
"""Compare the bench frames of two breakdown runs: rendering must not change.

    compare_frames.py <before-directory> <after-directory>

Pairs every <page>-<platform>-<run>/penny-<frame>.ppm present in both and
reports identical frames, or the count of differing pixels and the largest
channel difference. Exits non-zero if any frame differs or none was compared.
"""
import pathlib
import sys

PPM_HEADER_FIELDS = 4  # magic, width, height, maximum value


def pixels(path):
    data = path.read_bytes()
    fields, offset = [], 0
    while len(fields) < PPM_HEADER_FIELDS:
        while data[offset:offset + 1].isspace():
            offset += 1
        end = offset
        while not data[end:end + 1].isspace():
            end += 1
        fields.append(data[offset:end])
        offset = end
    offset += 1  # the single whitespace byte after the header
    assert fields[0] == b"P6", path
    return int(fields[1]), int(fields[2]), data[offset:]


def main():
    before, after = (pathlib.Path(arg) for arg in sys.argv[1:3])
    compared = differing = 0
    for old in sorted(before.glob("*/penny-*.ppm")):
        new = after / old.parent.name / old.name
        if not new.exists():
            continue
        compared += 1
        (w1, h1, a), (w2, h2, b) = pixels(old), pixels(new)
        name = f"{old.parent.name}/{old.name}"
        if (w1, h1) != (w2, h2):
            print(f"DIFFERENT SIZE {name}: {w1}x{h1} vs {w2}x{h2}")
            differing += 1
        elif a == b:
            print(f"identical {name}")
        else:
            changed = sum(1 for i in range(0, len(a), 3) if a[i:i + 3] != b[i:i + 3])
            largest = max(abs(x - y) for x, y in zip(a, b))
            print(f"DIFFERENT {name}: {changed} of {w1 * h1} pixels, largest channel difference {largest}")
            differing += 1
    print(f"compared={compared} differing={differing}")
    sys.exit(0 if compared and not differing else 1)


if __name__ == "__main__":
    main()
