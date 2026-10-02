#!/usr/bin/env python3
"""Power cuts at every device command of a journaled CuBit workload.

workload.adb runs the production Ext2/JBD2 code on an ext3 image over a
device with a volatile write cache (crash-fixture). For every command N, the
power is cut as N arrives, keeping all, none or every other write still in
the device cache. The image is then recovered twice: by e2fsck's journal
replay (Linux) and by CuBit's volume admission. Both must leave a clean
filesystem (e2fsck -fn), agree block for block outside the superblocks, and
hold every file as of the last flush reported complete ("SYNCED k"), or as of
the step after it (its commit may already be durable); a file overwritten in
place in the next step may hold each block old or new, as with Linux
data=ordered (overwrites are not journaled).
"""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
WORKLOAD = Path(os.environ.get("JOURNAL_WORKLOAD", HERE / "build/crash/workload"))
REPLAY = Path(os.environ.get("JOURNAL_REPLAY", HERE / "build/replay"))
BLOCK = 1024
KIB = 1024
LOSSES = ("KEEP_ALL", "LOSE_ALL", "LOSE_ALTERNATE")


def run(*args, env=None, check=True):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, env=env)
    if check and result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def states():
    """Expected files after each step (None: absent)."""
    victim = b"v" * 16 * KIB
    alpha1 = b"A" * 3 * KIB
    alpha2 = alpha1 + bytes(47 * KIB) + b"B" * 20 * KIB
    alpha3 = alpha2[:10 * KIB]
    victim3 = victim[:5 * KIB] + b"D" * 6 * KIB + victim[11 * KIB:]
    beta = b"C" * 40 * KIB
    gamma4 = beta + b"E" * 30 * KIB
    delta4 = b"F" * 200 * KIB
    delta5 = delta4[:100 * KIB] + b"G" * 8 * KIB + delta4[108 * KIB:]
    base = {"alpha": None, "beta": None, "gamma": None, "delta": None,
            "epsilon": None, "victim": victim}
    s1 = {**base, "alpha": alpha1}
    s2 = {**s1, "alpha": alpha2, "beta": beta}
    s3 = {**s2, "alpha": alpha3, "victim": victim3}
    s4 = {**s3, "beta": None, "gamma": gamma4, "delta": delta4}
    s5 = {**s4, "gamma": b"", "delta": delta5, "epsilon": b"H" * 70 * KIB}
    return [base, s1, s2, s3, s4, s5, s5]


def contents(image, name, root):
    out = run("debugfs", "-R", f"stat {name}", image).stdout
    if "File not found" in out:
        return None
    dump = root / f"{name}.dump"
    run("debugfs", "-R", f"dump {name} {dump}", image)
    return dump.read_bytes()


def mixed(found, before, after):
    """An in-place overwrite not yet flushed, as with Linux data=ordered:
    each block old or new (overwritten blocks are not journaled)."""
    if found is None or before is None or after is None or \
            not len(found) == len(before) == len(after):
        return False
    return all(found[i:i + BLOCK] in (before[i:i + BLOCK], after[i:i + BLOCK])
               for i in range(0, len(found), BLOCK))


def check_image(image, synced, expected, root, label):
    result = run("e2fsck", "-fn", image, check=False)
    if result.returncode:
        raise AssertionError(f"{label}: e2fsck -fn exit {result.returncode}\n{result.stdout}")
    for name in expected[0]:
        found = contents(image, name, root)
        allowed = [expected[synced][name], expected[synced + 1][name]]
        if found not in allowed and not mixed(found, *allowed):
            raise AssertionError(f"{label}: {name} is neither step {synced} nor "
                                 f"{synced + 1} ({None if found is None else len(found)} bytes)")


def compare(a, b, label):
    skip = {1}  # the filesystem superblock (1 KiB blocks)
    with open(a, "rb") as fa, open(b, "rb") as fb:
        number = 0
        while True:
            da, db = fa.read(BLOCK), fb.read(BLOCK)
            if not da:
                break
            if number not in skip and da != db:
                # The journal superblock: sequences may differ by recoverer.
                if not da.startswith(b"\xc0\x3b\x39\x98\x00\x00\x00\x04"):
                    raise AssertionError(f"{label}: block {number} differs")
            number += 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--stride", type=int, default=1)
    args = parser.parse_args()
    expected = states()
    with tempfile.TemporaryDirectory(prefix="cubit-crash-") as tmp:
        root = Path(tmp)
        stage = root / "stage"
        stage.mkdir()
        (stage / "victim").write_bytes(expected[0]["victim"])
        base = root / "base.img"
        with base.open("wb") as f:
            f.truncate(8 * 1024 * 1024)
        run("mke2fs", "-q", "-t", "ext3", "-b", BLOCK, "-F", "-d", stage, base)
        full = root / "full.img"
        shutil.copy(base, full)
        out = run(WORKLOAD, full).stdout
        total = int(out.split("DEVICE COMMANDS")[1].split()[0])
        check_image(full, 5, expected, root, "uninterrupted")
        cases = 0
        for cut in range(1, total + 1, args.stride):
            for loss in LOSSES:
                crashed = root / "crashed.img"
                shutil.copy(base, crashed)
                env = dict(os.environ, CUBIT_CUT=str(cut), CUBIT_LOSS=loss)
                out = run(WORKLOAD, crashed, env=env).stdout
                assert "POWER CUT" in out, out
                synced = max([int(line.split()[1]) for line in out.splitlines()
                              if line.startswith("SYNCED")], default=0)
                linux = root / "linux.img"
                cubit = root / "cubit.img"
                shutil.copy(crashed, linux)
                shutil.copy(crashed, cubit)
                run("e2fsck", "-y", "-E", "journal_only", linux, check=False)
                run(REPLAY, "admit", cubit)
                label = f"cut {cut} {loss} synced {synced}"
                check_image(linux, synced, expected, root, label + " (e2fsck replay)")
                check_image(cubit, synced, expected, root, label + " (CuBit replay)")
                compare(linux, cubit, label)
                cases += 1
        print(f"JBD2-CRASH-CHECK: PASS {cases} power cuts over {total} device commands")


if __name__ == "__main__":
    main()
