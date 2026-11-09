#!/usr/bin/env python3
"""FS-031: unlink and rename-over of files with several names (hard links).

hardlinks.adb runs the production Ext2 code on mke2fs-made ext2 and ext3
images whose staged tree has hard links (mke2fs -d keeps them): one = two,
keep/three = keep/four, held = held2 (two names each) and plain. Checked:

* uninterrupted: e2fsck -fn clean (every link count equals its names, no
  leaked or doubly used block or inode); the names left and their bytes;
  each released inode free, and exactly the released files' blocks and
  inodes returned to the free counts (no leak, no double free);
* power cuts at every device command (ext3: all, none or alternate cached
  writes lost; clean after replay by e2fsck and by CuBit. ext2: completed
  writes kept; e2fsck finds at most leaks and link over-counts, never an
  under-count, a name for a freed inode or a shared block);
* an error reply at every device command, before or after the transfer:
  the image is as in a power cut.
Every name present after any of these holds its file's bytes.
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import sys
import tempfile

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
from namespace import (BLOCK, KIB, LOSSES, FAIL_MODES, REPLAY,  # noqa: E402
                       check_benign, check_clean, run, synced_of)

WORKLOAD = Path(os.environ.get("HARDLINKS_WORKLOAD", HERE / "build/crash/hardlinks"))
APPENDED = b"H" * 3 * KIB
#  Staged files: (first name, other name or None, bytes).
FILES = {
    "one": ("two", 20 * KIB + 7),
    "keep/three": ("keep/four", 13 * KIB),
    "held": ("held2", 9 * KIB + 100),
    "plain": (None, 2 * KIB),
}
NAMES = ["one", "two", "keep/three", "keep/four", "held", "held2", "plain"]


def stat(image, name):
    return run("debugfs", "-R", f"stat {name}", image, check=False).stdout


def present(image, name):
    return "Inode:" in stat(image, name)


def inode_of(image, name):
    return int(re.search(r"Inode: (\d+)", stat(image, name)).group(1))


def links_of(image, name):
    return int(re.search(r"Links: (\d+)", stat(image, name)).group(1))


def kib_blocks(image, name):
    """1 KiB blocks the file holds, its indirect blocks included."""
    return int(re.search(r"Blockcount: (\d+)", stat(image, name)).group(1)) // 2


def dump(image, name, root):
    target = root / "dump.bin"
    if target.exists():
        target.unlink()
    run("debugfs", "-R", f"dump {name} {target}", image)
    return target.read_bytes()


def free_counts(image):
    out = run("dumpe2fs", "-h", image).stdout
    return (int(re.search(r"Free blocks:\s+(\d+)", out).group(1)),
            int(re.search(r"Free inodes:\s+(\d+)", out).group(1)))


def inode_free(image, number):
    return "not in use" in run("debugfs", "-R", f"testi <{number}>", image).stdout


def allowed(stage, name):
    """The bytes a name may hold: its staged file's, or (held) with the
    workload's append."""
    for first, (other, _) in FILES.items():
        if name in (first, other):
            data = (stage / first).read_bytes()
            return [data, data + APPENDED] if first == "held" else [data]
    raise KeyError(name)


def check_names(image, stage, root, label):
    """Every name left holds its file's bytes; keep/four may hold plain's
    (the rename) and two names of one inode hold the same bytes."""
    for name in NAMES:
        if not present(image, name):
            continue
        data = dump(image, name, root)
        ok = allowed(stage, name)
        if name == "keep/four":
            ok = ok + allowed(stage, "plain")
        if data not in ok:
            raise AssertionError(f"{label}: {name} holds {len(data)} wrong bytes")


def make_stage(stage):
    (stage / "keep").mkdir(parents=True)
    for first, (other, size) in FILES.items():
        (stage / first).write_bytes(os.urandom(size))
        if other:
            os.link(stage / first, stage / other)


def check_uninterrupted(base, full, stage, root, profile):
    label = f"{profile} uninterrupted"
    check_clean(full, label)
    for name in ("one", "two", "held", "held2", "plain", "keep/three"):
        if present(full, name):
            raise AssertionError(f"{label}: {name} still named")
    if dump(full, "keep/four", root) != (stage / "plain").read_bytes():
        raise AssertionError(f"{label}: keep/four is not the renamed plain")
    if links_of(full, "keep/four") != 1:
        raise AssertionError(f"{label}: keep/four links {links_of(full, 'keep/four')}")
    released = [inode_of(base, n) for n in ("one", "keep/three", "held")]
    for number in released:
        if not inode_free(full, number):
            raise AssertionError(f"{label}: inode {number} not freed")
    blocks = sum(kib_blocks(base, n) for n in ("one", "keep/three", "held"))
    (base_blocks, base_inodes), (blocks_now, inodes_now) = free_counts(base), free_counts(full)
    if (blocks_now - base_blocks, inodes_now - base_inodes) != (blocks, len(released)):
        raise AssertionError(
            f"{label}: freed {blocks_now - base_blocks} blocks and "
            f"{inodes_now - base_inodes} inodes, expected {blocks} and {len(released)}")


def journal_checks(crashed, root, stage, label):
    linux, cubit = root / "linux.img", root / "cubit.img"
    shutil.copy(crashed, linux)
    shutil.copy(crashed, cubit)
    run("e2fsck", "-y", "-E", "journal_only", linux, check=False)
    run(REPLAY, "admit", cubit)
    check_clean(linux, label + " (e2fsck replay)")
    check_clean(cubit, label + " (CuBit replay)")
    check_names(linux, stage, root, label + " (e2fsck replay)")
    check_names(cubit, stage, root, label + " (CuBit replay)")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--stride", type=int, default=1)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="cubit-hardlinks-") as tmp:
        root = Path(tmp)
        stage = root / "stage"
        make_stage(stage)
        counts = {}
        for profile in ("ext2", "ext3"):
            base = root / f"{profile}.img"
            with base.open("wb") as f:
                f.truncate(8 * 1024 * 1024)
            run("mke2fs", "-q", "-t", profile, "-b", BLOCK, "-F", "-d", stage, base)
            for first, (other, _) in FILES.items():
                expect = 2 if other else 1
                if links_of(base, first) != expect:
                    raise AssertionError(f"mke2fs -d did not keep {first}'s links")
            full = root / "full.img"
            shutil.copy(base, full)
            out = run(WORKLOAD, full).stdout
            if "DETACHED" not in out:
                raise AssertionError(f"{profile}: workload failed\n{out}")
            total = int(out.split("DEVICE COMMANDS")[1].split()[0])
            check_uninterrupted(base, full, stage, root, profile)
            cuts = faults = 0
            for cut in range(1, total + 1, args.stride):
                for loss in (LOSSES if profile == "ext3" else ("KEEP_ALL",)):
                    crashed = root / "crashed.img"
                    shutil.copy(base, crashed)
                    env = dict(os.environ, CUBIT_CUT=str(cut), CUBIT_LOSS=loss)
                    out = run(WORKLOAD, crashed, env=env).stdout
                    assert "POWER CUT" in out, out
                    label = f"{profile} cut {cut} {loss} synced {synced_of(out)}"
                    if profile == "ext3":
                        journal_checks(crashed, root, stage, label)
                    else:
                        check_benign(crashed, label)
                        check_names(crashed, stage, root, label)
                    cuts += 1
                for mode in FAIL_MODES:
                    failed = root / "failed.img"
                    shutil.copy(base, failed)
                    env = dict(os.environ, CUBIT_FAIL=str(cut), CUBIT_FAIL_MODE=mode)
                    out = run(WORKLOAD, failed, env=env).stdout
                    label = f"{profile} fault {cut} {mode}"
                    assert "FAILED command" in out, label + "\n" + out
                    if "OP FAILED" not in out and "DETACHED" in out:
                        raise AssertionError(f"{label}: error not reported\n{out}")
                    if profile == "ext3":
                        journal_checks(failed, root, stage, label)
                    else:
                        check_benign(failed, label)
                        check_names(failed, stage, root, label)
                    faults += 1
            counts[profile] = (cuts, faults, total)
        print("HARDLINKS-CHECK: PASS " + ", ".join(
            f"{p}: {c} power cuts, {f} injected errors over {t} commands"
            for p, (c, f, t) in counts.items()))


if __name__ == "__main__":
    main()
