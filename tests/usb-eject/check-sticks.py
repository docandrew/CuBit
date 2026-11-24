#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""Linux's view of the usb-eject sticks after the run (FS-034).

  check-sticks.py STICK_DIR

Stick B was ejected cleanly: fsck.fat -n must find nothing and its file
must hold exactly the bytes written. Stick A was pulled out while written
and came back read only: fsck.fat -n must report the dirty bit (CuBit never
repaired it), and every file written before the surprise removal must hold
exactly its bytes. Host tools only (fsck.fat, mtools).
"""
import os
import pathlib
import subprocess
import sys
import tempfile

ENV = dict(os.environ, MTOOLS_SKIP_CHECK="1", LC_ALL="C.UTF-8",
           PATH=os.environ.get("PATH", "") + ":/usr/sbin")


def pattern(k, n):
    # usb-eject.c's pattern().
    return bytes((i * 29 + k * 13 + (i >> 9)) & 0xFF for i in range(n))


def run(*args):
    r = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=ENV)
    return r.returncode, r.stdout.decode("utf-8", "replace")


def files_of(image, want, failed, label):
    with tempfile.TemporaryDirectory() as work:
        for name, data in want.items():
            target = pathlib.Path(work) / name
            code, out = run("mcopy", "-n", "-i", image, f"::/{name}", target)
            if code != 0:
                failed.append(f"{label}: mcopy {name}: {out.strip()}")
            elif target.read_bytes() != data:
                failed.append(f"{label}: {name} differs")


def main(stick_dir):
    d = pathlib.Path(stick_dir)
    failed = []
    code, out = run("fsck.fat", "-n", d / "stickb.img")
    findings = [l for l in out.splitlines() if l.strip() and not l.startswith("fsck.fat ") and " files, " not in l]
    if code != 0 or findings:
        failed.append(f"stick B: fsck.fat -n: exit {code}: {findings}")
    files_of(d / "stickb.img", {"b.bin": pattern(3, 200000)}, failed, "stick B")
    code, out = run("fsck.fat", "-n", d / "sticka.img")
    if "Dirty bit is set" not in out:
        failed.append(f"stick A: no dirty bit after the surprise removal: {out.strip()}")
    files_of(d / "sticka.img", {"first.bin": pattern(1, 300000), "kept.bin": pattern(2, 5000),
                                "second.bin": pattern(4, 100000)}, failed, "stick A")
    if failed:
        for f in failed:
            print(f"USB-EJECT-STICKS: FAIL {f}")
        return 1
    print("USB-EJECT-STICKS: PASS (B clean, A dirty with its files intact)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
