#!/usr/bin/env python3
"""Reference QOI 1.0 encoder and decoder (https://qoiformat.org/).

A direct transcription of the specification and the reference qoi.h, used
by the asset build (tools/build_wallpaper_assets.py) and as the oracle for
CuBit's SPARK decoder tests (tests/qoi). Pixels are RGBA byte strings.
"""
import struct

MAGIC = b"qoif"
END_MARKER = b"\x00" * 7 + b"\x01"
OP_INDEX, OP_DIFF, OP_LUMA, OP_RUN = 0x00, 0x40, 0x80, 0xC0
OP_RGB, OP_RGBA = 0xFE, 0xFF
MAX_RUN = 62
SRGB, LINEAR = 0, 1
CHANNELS_RGB, CHANNELS_RGBA = 3, 4


def _hash(r, g, b, a):
    return (r * 3 + g * 5 + b * 7 + a * 11) % 64


def encode(rgba, width, height, channels=CHANNELS_RGBA, colorspace=SRGB):
    """Encode width*height RGBA pixels (a bytes-like of 4 bytes each)."""
    if len(rgba) != width * height * 4 or width < 1 or height < 1:
        raise ValueError("pixel data does not match the size")
    out = bytearray(MAGIC + struct.pack(">IIBB", width, height, channels, colorspace))
    index = [(0, 0, 0, 0)] * 64
    previous = (0, 0, 0, 255)
    run = 0
    count = width * height
    view = memoryview(rgba)
    for position in range(count):
        offset = position * 4
        pixel = tuple(view[offset:offset + 4])
        if pixel == previous:
            run += 1
            if run == MAX_RUN or position == count - 1:
                out.append(OP_RUN | (run - 1))
                run = 0
            continue
        if run:
            out.append(OP_RUN | (run - 1))
            run = 0
        slot = _hash(*pixel)
        if index[slot] == pixel:
            out.append(OP_INDEX | slot)
        else:
            index[slot] = pixel
            r, g, b, a = pixel
            if a == previous[3]:
                dr = (r - previous[0] + 128) % 256 - 128
                dg = (g - previous[1] + 128) % 256 - 128
                db = (b - previous[2] + 128) % 256 - 128
                dr_dg, db_dg = dr - dg, db - dg
                if -2 <= dr <= 1 and -2 <= dg <= 1 and -2 <= db <= 1:
                    out.append(OP_DIFF | (dr + 2) << 4 | (dg + 2) << 2 | (db + 2))
                elif -32 <= dg <= 31 and -8 <= dr_dg <= 7 and -8 <= db_dg <= 7:
                    out += bytes((OP_LUMA | (dg + 32), (dr_dg + 8) << 4 | (db_dg + 8)))
                else:
                    out += bytes((OP_RGB, r, g, b))
            else:
                out += bytes((OP_RGBA, r, g, b, a))
        previous = pixel
    out += END_MARKER
    return bytes(out)


def decode(data):
    """Decode a QOI stream strictly; returns (width, height, rgba bytes)."""
    if len(data) < 14 + 8 or data[:4] != MAGIC:
        raise ValueError("not a QOI image")
    width, height, channels, colorspace = struct.unpack(">IIBB", data[4:14])
    if width == 0 or height == 0 or channels not in (CHANNELS_RGB, CHANNELS_RGBA) or colorspace not in (0, 1):
        raise ValueError("bad QOI header")
    index = [(0, 0, 0, 0)] * 64
    r, g, b, a = 0, 0, 0, 255
    out = bytearray()
    position, count = 14, width * height
    produced = 0
    while produced < count:
        if position >= len(data):
            raise ValueError("truncated")
        op = data[position]
        position += 1
        run = 1
        if op == OP_RGB:
            r, g, b = data[position:position + 3]
            position += 3
        elif op == OP_RGBA:
            r, g, b, a = data[position:position + 4]
            position += 4
        elif op & 0xC0 == OP_INDEX:
            r, g, b, a = index[op]
        elif op & 0xC0 == OP_DIFF:
            r = (r + ((op >> 4) & 3) - 2) % 256
            g = (g + ((op >> 2) & 3) - 2) % 256
            b = (b + (op & 3) - 2) % 256
        elif op & 0xC0 == OP_LUMA:
            second = data[position]
            position += 1
            dg = (op & 0x3F) - 32
            r = (r + dg - 8 + (second >> 4)) % 256
            g = (g + dg) % 256
            b = (b + dg - 8 + (second & 0x0F)) % 256
        else:
            run = (op & 0x3F) + 1
            if run > count - produced:
                raise ValueError("run past the end")
        index[_hash(r, g, b, a)] = (r, g, b, a)
        out += bytes((r, g, b, a)) * run
        produced += run
    if data[position:] != END_MARKER:
        raise ValueError("bad end marker")
    return width, height, bytes(out)
