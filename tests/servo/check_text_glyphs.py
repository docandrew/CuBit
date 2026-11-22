#!/usr/bin/env python3
"""Fail when Penny's page of text only reaches the frame without glyphs.

The headless `servo` test loads a page whose body is black text on white and
nothing else (tests/headless/run.sh, "penny-text-check"); cubitshell reports
each page's frame as `CUBITSHELL: page N frame WxH inked=K ...`, K being the
pixels that are not white. Without fonts, or with glyphs that never reach the
frame, that page is blank.

    check_text_glyphs.py <serial-log> <page-index>
"""
import re
import sys

# The page sets 56 characters at 32 px; each glyph inks well over 50 pixels
# (stems plus antialiasing), so a rendered line is several thousand. A few
# stray pixels are not text.
MINIMUM_GLYPH_PIXELS = 2000


def glyph_pixels(log_text, page):
    pattern = re.compile(rf"CUBITSHELL: page {page} frame (\d+)x(\d+) inked=(\d+)")
    found = pattern.findall(log_text)
    if not found:
        return None
    return int(found[-1][2])


def main():
    log, page = sys.argv[1], int(sys.argv[2])
    with open(log, encoding="utf-8", errors="replace") as handle:
        inked = glyph_pixels(handle.read(), page)
    if inked is None:
        print(f"PENNY-TEXT: FAIL page {page} reported no frame")
        return 1
    if inked < MINIMUM_GLYPH_PIXELS:
        print(f"PENNY-TEXT: FAIL page {page} inked={inked} < {MINIMUM_GLYPH_PIXELS}: no glyphs")
        return 1
    print(f"PENNY-TEXT: PASS page {page} inked={inked}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
