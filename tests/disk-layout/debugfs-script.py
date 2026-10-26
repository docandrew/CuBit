#!/usr/bin/env python3
"""Prints debugfs commands that copy a host directory tree into an ext2
image under TARGET (mkdir for each directory, write for each file).

usage: debugfs-script.py HOST_DIR TARGET > commands; debugfs -w -f commands IMAGE
"""
import os
import sys


def main() -> None:
    host, target = sys.argv[1], sys.argv[2]
    parts = target.split("/")
    for i in range(1, len(parts) + 1):
        print(f"mkdir {'/'.join(parts[:i])}")
    for root, dirs, files in os.walk(host):
        dirs.sort()
        relative = os.path.relpath(root, host)
        base = target if relative == "." else f"{target}/{relative}"
        for d in dirs:
            print(f"mkdir {base}/{d}")
        for f in sorted(files):
            print(f"write {os.path.join(root, f)} {base}/{f}")


if __name__ == "__main__":
    main()
