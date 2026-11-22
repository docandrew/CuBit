#!/usr/bin/env python3
"""Journal operations under cache pressure (pressure.adb).

A 150 MiB write to a 192 MiB ext3 image with jbd2's smallest journal (1 KiB
blocks: 24 groups of 8192 blocks, so every group's bitmap falls in the same
cache set) dirties more metadata blocks in one set than the cache has ways,
and more than half the log, so transactions commit between its operations. Operations then meet full sets: metadata waits in the spill (file
data goes home directly anyway), and no commit may split an operation
(SPLIT COMMITS 0). A power cut at
sampled commands (all, none or alternate cached writes lost) must leave,
after replay by e2fsck and by CuBit (block-identical), a clean volume whose
file is 'P' * n for some n: a prefix of whole operations (the write is
not atomic; each allocation run publishes its inode in its own operation).
"""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
WORKLOAD = Path(os.environ.get("PRESSURE_WORKLOAD", HERE / "build/crash/pressure"))
REPLAY = Path(os.environ.get("JOURNAL_REPLAY", HERE / "build/replay"))
BLOCK = 1024
BIG = 150 * 1024 * 1024
LOSSES = ("KEEP_ALL", "LOSE_ALL", "LOSE_ALTERNATE")


def run(*args, env=None, check=True):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, env=env)
    if check and result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def check(image, root, synced, label):
    result = run("e2fsck", "-fn", image, check=False)
    if result.returncode:
        raise AssertionError(f"{label}: e2fsck -fn exit {result.returncode}\n{result.stdout}")
    out = run("debugfs", "-R", "stat big", image).stdout
    if "File not found" in out:
        if synced >= 1:
            raise AssertionError(f"{label}: big lost")
        return
    dump = root / "big.dump"
    run("debugfs", "-R", f"dump big {dump}", image)
    data = dump.read_bytes()
    if data != b"P" * len(data):
        raise AssertionError(f"{label}: big is not a prefix ({len(data)} bytes)")
    if synced >= 2 and len(data) != BIG:
        raise AssertionError(f"{label}: synced write lost ({len(data)} bytes)")


def compare(a, b, label):
    """Block-identical outside the superblock and the journal superblock."""
    with open(a, "rb") as fa, open(b, "rb") as fb:
        number = 0
        while True:
            da, db = fa.read(BLOCK), fb.read(BLOCK)
            if not da:
                break
            if number != 1 and da != db and not da.startswith(
                    b"\xc0\x3b\x39\x98\x00\x00\x00\x04"):
                raise AssertionError(f"{label}: block {number} differs")
            number += 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--cuts", type=int, default=40)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="cubit-pressure-") as tmp:
        root = Path(tmp)
        base = root / "base.img"
        with base.open("wb") as f:
            f.truncate(192 * 1024 * 1024)
        run("mke2fs", "-q", "-t", "ext3", "-b", BLOCK, "-J", "size=1", "-F", base)
        full = root / "full.img"
        shutil.copy(base, full)
        out = run(WORKLOAD, full).stdout
        total = int(out.split("DEVICE COMMANDS")[1].split()[0])
        splits = int(out.split("SPLIT COMMITS")[1].split()[0])
        pressure = int(out.split("PRESSURE BLOCKS")[1].split()[0])
        if splits:
            raise AssertionError(f"{splits} commits split an operation")
        if not pressure:
            raise AssertionError("no operation met a full cache set")
        check(full, root, 2, "uninterrupted")
        cases = 0
        stride = max(1, total // args.cuts)
        for cut in range(1, total + 1, stride):
            for loss in LOSSES:
                crashed = root / "crashed.img"
                shutil.copy(base, crashed)
                env = dict(os.environ, CUBIT_CUT=str(cut), CUBIT_LOSS=loss)
                out = run(WORKLOAD, crashed, env=env).stdout
                assert "POWER CUT" in out, out
                synced = max([int(l.split()[1]) for l in out.splitlines()
                              if l.startswith("SYNCED")], default=0)
                linux, cubit = root / "linux.img", root / "cubit.img"
                shutil.copy(crashed, linux)
                shutil.copy(crashed, cubit)
                run("e2fsck", "-y", "-E", "journal_only", linux, check=False)
                run(REPLAY, "admit", cubit)
                label = f"cut {cut} {loss} synced {synced}"
                check(linux, root, synced, label + " (e2fsck replay)")
                check(cubit, root, synced, label + " (CuBit replay)")
                compare(linux, cubit, label)
                cases += 1
        print(f"JBD2-PRESSURE-CHECK: PASS {cases} power cuts over {total} commands, "
              f"{pressure} blocks beside full sets, 0 split operations")


if __name__ == "__main__":
    main()
