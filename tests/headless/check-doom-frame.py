#!/usr/bin/env python3
"""Check real QEMU framebuffer evidence, not just DOOM startup messages."""
import pathlib
import sys


def ppm(path):
    with pathlib.Path(path).open("rb") as source:
        fields = []
        while len(fields) < 4:
            line = source.readline()
            if not line:
                raise ValueError("incomplete PPM header")
            fields.extend(line.split(b"#", 1)[0].split())
        magic, width, height, maximum = fields
        width, height = int(width), int(height)
        pixels = source.read()
    if (magic, maximum) != (b"P6", b"255") or len(pixels) != width * height * 3:
        raise ValueError("unsupported or incomplete framebuffer capture")
    if (width, height) != (1024, 768):
        raise ValueError("DOOM fixture requires a 1024x768 framebuffer")
    return pixels


def region(pixels, left, top, right, bottom):
    return [pixels[(y * 1024 + x) * 3:(y * 1024 + x) * 3 + 3]
            for y in range(top, bottom) for x in range(left, right)]


def check(prefix):
    game = ppm(prefix + "-game.ppm")
    apps = ppm(prefix + "-apps.ppm")
    # Interior of the 640x400 DOOM client, excluding all window decorations.
    # A created-but-blank window (the original regression) fails this check.
    if len(set(region(game, 140, 160, 700, 450))) < 32:
        raise ValueError("DOOM client is blank or has no game image")
    # This strip is outside DOOM's window: game animation cannot impersonate
    # the Apps menu opening in response to a real keyboard event.
    before = region(game, 12, 450, 85, 700)
    after = region(apps, 12, 450, 85, 700)
    if sum(a != b for a, b in zip(before, after)) < 1000:
        raise ValueError("desktop did not visibly open Apps after DOOM started")


if __name__ == "__main__":
    try:
        check(sys.argv[1])
    except (OSError, ValueError, IndexError) as error:
        sys.exit(f"headless: FAIL DOOM framebuffer/liveness: {error}")
    print("headless: DOOM game pixels and responsive Apps menu verified")
