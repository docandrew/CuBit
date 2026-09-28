#!/usr/bin/env python3
"""Seed the headless runner's disposable image with a sparse Linux double tree."""
import argparse
from pathlib import Path
import struct
from run import debug, inode_number, inode_bytes

parser = argparse.ArgumentParser()
parser.add_argument("image", type=Path, help="runner's temporary disk, never its base image")
parser.add_argument("scratch", type=Path, help="runner-owned temporary fixture file")
args = parser.parse_args()

with args.scratch.open("wb") as stream:
    stream.seek(8 * 1024 * 1024)
    stream.write(b"D" * 8192)
debug(args.image, "rm double-existing")
debug(args.image, f"write {args.scratch} double-existing")
record = inode_bytes(args.image, inode_number(args.image, "double-existing"))
assert struct.unpack_from("<I", record, 92)[0] != 0, "fixture needs a double-indirect root"
assert struct.unpack_from("<I", record, 4)[0] == 8 * 1024 * 1024 + 8192
print("DOUBLE-OVERWRITE-FIXTURE: PASS")
