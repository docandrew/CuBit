#!/usr/bin/env python3
"""The reading faces draw every character the Bible images may hold (APP-005):
each pinned face in assets/bible/sources.json maps every character of
tools/build_bible.py's DRAWN set (the converter refuses verse text outside it)
to a glyph in its cmap. The fonts are untrusted data: read as bytes, bounded,
never executed."""
import struct
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
import build_bible as bb  # noqa: E402

MAXIMUM_FONT = 16 * 1024 * 1024


def cmap_of(data):
    """The code points a TrueType/OpenType font maps (Unicode subtables,
    formats 4 and 12) to a glyph other than .notdef."""
    if len(data) < 12:
        raise ValueError('too short')
    tables = struct.unpack_from('>H', data, 4)[0]
    cmap = None
    for i in range(tables):
        tag, _, offset, length = struct.unpack_from('>4sIII', data, 12 + 16 * i)
        if tag == b'cmap':
            if offset + length > len(data):
                raise ValueError('cmap outside the file')
            cmap = data[offset:offset + length]
    if cmap is None:
        raise ValueError('no cmap')
    mapped = set()
    for i in range(struct.unpack_from('>H', cmap, 2)[0]):
        platform, encoding, offset = struct.unpack_from('>HHI', cmap, 4 + 8 * i)
        if not (platform == 0 or (platform == 3 and encoding in (1, 10))):
            continue
        fmt = struct.unpack_from('>H', cmap, offset)[0]
        if fmt == 4:
            segments = struct.unpack_from('>H', cmap, offset + 6)[0] // 2
            ends = offset + 14
            starts = ends + 2 * segments + 2
            deltas = starts + 2 * segments
            ranges = deltas + 2 * segments
            for s in range(segments):
                end = struct.unpack_from('>H', cmap, ends + 2 * s)[0]
                start = struct.unpack_from('>H', cmap, starts + 2 * s)[0]
                delta = struct.unpack_from('>h', cmap, deltas + 2 * s)[0]
                range_offset = struct.unpack_from('>H', cmap, ranges + 2 * s)[0]
                for code in range(start, min(end, 0xFFFE) + 1):
                    if range_offset == 0:
                        glyph = (code + delta) & 0xFFFF
                    else:
                        at = ranges + 2 * s + range_offset + 2 * (code - start)
                        glyph = struct.unpack_from('>H', cmap, at)[0]
                        glyph = (glyph + delta) & 0xFFFF if glyph else 0
                    if glyph:
                        mapped.add(code)
        elif fmt == 12:
            groups = struct.unpack_from('>I', cmap, offset + 12)[0]
            for g in range(groups):
                start, end, glyph = struct.unpack_from('>III', cmap, offset + 16 + 12 * g)
                for code in range(start, min(end, 0x10FFFF) + 1):
                    if glyph + code - start:
                        mapped.add(code)
    return mapped


def main():
    import json
    fonts = json.loads(bb.SOURCES.read_text())['fonts']
    failures = 0
    for font in fonts:
        data = bb.pinned(font).read_bytes()
        if len(data) > MAXIMUM_FONT:
            print('FAIL: %s is larger than %d bytes' % (font['name'], MAXIMUM_FONT))
            failures += 1
            continue
        mapped = cmap_of(data)
        missing = sorted(c for c in bb.DRAWN if ord(c) not in mapped and c != ' ')
        if missing:
            failures += 1
            print('FAIL: %s draws no %s' % (font['name'], ' '.join('U+%04X' % ord(c) for c in missing)))
        else:
            print('  %s maps all %d drawn characters' % (font['name'], len(bb.DRAWN)))
    print('reading faces: %d fonts, %d failures' % (len(fonts), failures))
    sys.exit(1 if failures else 0)


if __name__ == '__main__':
    main()
