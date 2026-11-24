#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Print the last console line carrying MARKER in a serial log, whole.

A program's debug text reaches the serial console as log lines of at most
CuBit.Console_Lines.Maximum_Line_Bytes (512) bytes each: a longer line goes
on in the next line of the same program ("NAME: " prefix). A REPL reply
such as a directory listing can be longer than one line, so this joins the
pieces back: a piece of exactly the maximum continues in the program's next
line (other programs' lines in between are skipped).

Usage: console-reply.py SERIAL_LOG MARKER
Exit status 1 when no line carries MARKER.
"""
import sys

MAXIMUM_LINE_BYTES = 512  # CuBit.Console_Lines.Maximum_Line_Bytes
PREFIX_END = ": "


def main() -> int:
    log_path, marker = sys.argv[1], sys.argv[2]
    with open(log_path, "rb") as log:
        lines = log.read().decode("utf-8", "replace").splitlines()
    start = None
    for index, line in enumerate(lines):
        if marker in line:
            start = index
    if start is None:
        return 1
    prefix = lines[start][: lines[start].index(PREFIX_END) + len(PREFIX_END)]
    piece = lines[start][len(prefix):]
    reply = piece
    index = start + 1
    while len(piece.encode("utf-8")) == MAXIMUM_LINE_BYTES and index < len(lines):
        if lines[index].startswith(prefix):
            piece = lines[index][len(prefix):]
            reply += piece
        index += 1
    print(reply)
    return 0


if __name__ == "__main__":
    sys.exit(main())
