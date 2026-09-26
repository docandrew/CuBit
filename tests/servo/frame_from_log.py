#!/usr/bin/env python3
"""Extract the frames cubitshell dumps (CUBITSHELL-FRAME/ROW lines, written
when /servo/dump-frames exists) from a serial log as PGM images.

    frame_from_log.py <serial-log> <output-prefix>   # -> <prefix>-<n>.pgm
"""
import re
import sys

log, prefix = sys.argv[1], sys.argv[2]
frame = None
rows = []


def flush():
    if frame is None:
        return
    index, w, h = frame
    with open(f"{prefix}-{index}.pgm", "wb") as out:
        out.write(f"P5\n{w} {h}\n255\n".encode())
        for row in rows[:h]:
            out.write(bytes.fromhex(row[: w * 2].ljust(w * 2, "f")))
        for _ in range(h - len(rows)):
            out.write(b"\xff" * w)
    print(f"{prefix}-{index}.pgm ({len(rows)} rows)")


for line in open(log, errors="replace"):
    m = re.search(r"CUBITSHELL-FRAME (\d+) (\d+) (\d+)", line)
    if m:
        flush()
        frame = tuple(int(g) for g in m.groups())
        rows = []
        continue
    m = re.search(r"CUBITSHELL-ROW ([0-9a-f]+)", line)
    if m and frame is not None:
        rows.append(m.group(1))
flush()
