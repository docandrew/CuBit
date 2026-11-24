#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""FAT32 headless test (fat32-volume): make the stick, then check it.

  check-image.py make IMAGE      an MBR disk with one FAT32 partition (type
                                 0C at sector 2048, as sticks come), holding
                                 the files fat-volume.c reads first
  check-image.py check IMAGE     after the run: fsck.fat -n finds nothing,
                                 and every file holds exactly the bytes
                                 fat-volume.c wrote (the same pattern)
Host tools only (mkfs.fat, fsck.fat, mtools); the image is a temporary file.
"""
import os
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

FIRST = 2048                      # the partition's first sector
SECTORS = 96 * 2048               # 96 MiB
ENV = dict(os.environ, MTOOLS_SKIP_CHECK="1", LC_ALL="C.UTF-8",
           PATH=os.environ.get("PATH", "") + ":/usr/sbin")


def pattern(k, n):
    return bytes((i * 31 + k * 7 + (i >> 8)) & 0xFF for i in range(n))


def run(*args):
    r = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=ENV)
    out = r.stdout.decode("utf-8", "replace")
    return r.returncode, out


SEEDED = {"from linux/hello from linux.txt": pattern(1, 1234),
          "from linux/Grüße.bin": pattern(2, 70000),
          "from linux/empty": b""}
WRITTEN = {"cubit/README.TXT": pattern(10, 600),
           "cubit/tree/a file with a long name.dat": pattern(11, 300001),
           "cubit/tree/deeper and deeper/big.bin": pattern(12, 2100000),
           "cubit/tree/αβγ 日本.txt": pattern(13, 4096),
           "cubit/tree/zero": b"",
           "cubit/tree/renamed across directories.txt": pattern(15, 5000)}
WRITTEN.update({f"cubit/many/entry {i:02d} of the crowded directory.txt": pattern(20 + i, i * 101)
                for i in range(40)})
DIRS = {"from linux", "from linux/nested", "cubit", "cubit/tree", "cubit/tree/deeper and deeper", "cubit/many"}


def make(image):
    with open(image, "wb") as f:
        f.truncate((FIRST + SECTORS) * 512)
    code, out = run("mkfs.fat", "-F", "32", "-s", "1", "-n", "CUBITFAT", f"--offset={FIRST}", image,
                    str(SECTORS // 2))
    if code:
        sys.exit(f"mkfs.fat: {out}")
    with open(image, "r+b") as f:
        f.seek(446)
        f.write(struct.pack("<B3sB3sII", 0, b"\0\0\0", 0x0C, b"\0\0\0", FIRST, SECTORS))
        f.seek(510)
        f.write(b"\x55\xaa")
    target = f"{image}@@{FIRST * 512}"
    work = Path(tempfile.mkdtemp())
    try:
        run("mmd", "-i", target, "::/from linux")
        run("mmd", "-i", target, "::/from linux/nested")
        for path, data in SEEDED.items():
            (work / "f").write_bytes(data)
            code, out = run("mcopy", "-i", target, work / "f", f"::/{path}")
            if code:
                sys.exit(f"mcopy {path}: {out}")
    finally:
        shutil.rmtree(work)


def check(image):
    part = Path(tempfile.mkdtemp()) / "partition.img"
    with open(image, "rb") as f, open(part, "wb") as o:
        f.seek(FIRST * 512)
        o.write(f.read(SECTORS * 512))
    failed = []
    code, out = run("fsck.fat", "-n", part)
    findings = [l for l in out.splitlines() if l.strip() and not l.startswith("fsck.fat ") and " files, " not in l]
    if code or findings:
        failed.append(f"fsck.fat -n: exit {code}: {findings}")
    out_dir = part.parent / "tree"
    out_dir.mkdir()
    run("mcopy", "-s", "-n", "-m", "-i", part, "::/*", out_dir)
    files, dirs = {}, set()
    for root, dnames, fnames in os.walk(out_dir):
        for d in dnames:
            dirs.add(os.path.relpath(os.path.join(root, d), out_dir))
        for n in fnames:
            p = os.path.join(root, n)
            files[os.path.relpath(p, out_dir)] = Path(p).read_bytes()
    want = dict(SEEDED)
    want.update(WRITTEN)
    for path in sorted(set(want) | set(files)):
        if path not in files:
            failed.append(f"missing {path}")
        elif path not in want:
            failed.append(f"unexpected {path}")
        elif files[path] != want[path]:
            failed.append(f"contents of {path}")
    if dirs != DIRS:
        failed.append(f"directories {sorted(dirs ^ DIRS)}")
    code, listing = run("mdir", "-i", part, "::/cubit/tree")
    if "A FILE~1" not in listing.replace("AFILEW~1", "A FILE~1") and "AFILEW~1" not in listing:
        failed.append("aliases: " + listing)
    shutil.rmtree(part.parent)
    with open(image, "rb") as f:
        if f.read(446) != bytes(446):
            failed.append("bytes written before the partition")
    for line in failed:
        print(f"FAT32-IMAGE-CHECK: FAIL {line}")
    if not failed:
        print(f"FAT32-IMAGE-CHECK: PASS ({len(want)} files, fsck.fat clean)")
    return not failed


if __name__ == "__main__":
    if len(sys.argv) != 3 or sys.argv[1] not in ("make", "check"):
        sys.exit(__doc__)
    if sys.argv[1] == "make":
        make(sys.argv[2])
    elif not check(sys.argv[2]):
        sys.exit(1)
