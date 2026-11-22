#!/usr/bin/env python3
"""mkdir, unlink (also of an open file, reclaimed at last close) and rmdir.

namespace.adb runs the production Ext2 code on mke2fs-made ext2 and ext3
images through the file-backed device of crash-fixture. Checked:

* uninterrupted: e2fsck -fn clean, the expected tree, untouched files intact;
* ext3 power cuts at every device command (all, none or alternate cached
  writes lost): replay by e2fsck and by CuBit agree block for block, are
  clean, and hold the tree of the last completed flush or of the next step;
* ext2 (write-through, no journal) power cuts keeping every completed write
  (a device without a volatile cache): e2fsck -fn finds at most leaks and
  link over-counts (e2fsck repairs these without loss), never a name for a
  freed inode or a shared or freed block; after repair the tree is again
  that of the last flush or the next step (lost+found aside);
* ext3's orphan list: a file unlinked while open is on it until released;
  a crash in between leaves it for recovery, which e2fsck (as Linux's
  mount) and CuBit's admission both complete (released inodes, their freed
  blocks and the log may then differ; nothing else may);
* an error reply at every device command, before or after the transfer:
  the failure reaches the caller, and the image is as in a power cut
  (ext3: clean after journal replay; ext2: leaks/over-counts only).
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
WORKLOAD = Path(os.environ.get("NAMESPACE_WORKLOAD", HERE / "build/crash/namespace"))
REPLAY = Path(os.environ.get("JOURNAL_REPLAY", HERE / "build/replay"))
BLOCK = 1024
KIB = 1024
MANY = 24
LOSSES = ("KEEP_ALL", "LOSE_ALL", "LOSE_ALTERNATE")
FAIL_MODES = ("BEFORE", "AFTER")

#  e2fsck -fn findings that are only leaks or over-counts: space or inodes
#  marked used that nothing references, link counts above the true count,
#  summary counters, and allocated inodes nothing names (lost+found).
BENIGN = [
    r"Block bitmap differences:",
    r"Inode bitmap differences:",
    r"Free blocks count wrong",
    r"Free inodes count wrong",
    r"Directories count wrong for group",
    r"Unattached (zero-length )?inode \d+",
    r"Deleted inode \d+ has zero dtime",
    r"Padding at end of inode bitmap",
    r"Connect to /lost\+found\?",
    r"Fix\? no",
    r"Clear\? no",
    r"Unconnected directory inode \d+",
    r"/lost\+found not found",
]
OVERCOUNT = re.compile(r"Inode (\d+) ref count is (\d+), should be (\d+)\.")


def run(*args, env=None, check=True):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, env=env)
    if check and result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def many(index):
    return f"many-directories-with-long-names-to-fill-blocks-{index}"


def states():
    """Expected tree (path -> 'dir' or file) after each step."""
    base = {"lost+found": "dir", "victim": "file", "keep": "dir",
            "keep/data": "file", "plain": "file"}
    s1 = {**base, "d1": "dir"}
    s2 = {**s1, "d1/sub": "dir"}
    s3 = {**s2, "d1/f": "file"}
    s4 = {k: v for k, v in s3.items() if k != "victim"}
    s5 = {k: v for k, v in s4.items() if k != "d1/f"}
    s6 = {k: v for k, v in s5.items() if k != "d1/sub"}
    s7 = {k: v for k, v in s6.items() if k != "d1"}
    s8 = {**s7, **{many(i): "dir" for i in range(1, MANY + 1)}}
    # Step 5 unlinks d1/f while open (an orphan until step 6 frees it).
    return [base, s1, s2, s3, s4, s5, dict(s5), s6, s7, s8, dict(s7), dict(s7)]


def tree(image, path="/", prefix=""):
    found = {}
    out = run("debugfs", "-R", f"ls -p {path}", image).stdout
    for line in out.splitlines():
        fields = line.split("/")
        if len(fields) < 7 or not fields[1].isdigit() or fields[1] == "0":
            continue
        name = "/".join(fields[5:-2]) if len(fields) > 7 else fields[5]
        if name in (".", ".."):
            continue
        kind = "dir" if fields[2].startswith("04") else "file"
        full = prefix + name
        found[full] = kind
        if kind == "dir" and full != "lost+found":
            found.update(tree(image, f"{path.rstrip('/')}/{name}", full + "/"))
    return found


def dump(image, name, root):
    target = root / "dump.bin"
    if target.exists():
        target.unlink()
    run("debugfs", "-R", f"dump {name} {target}", image)
    return target.read_bytes()


def check_tree(image, synced, expected, stage, root, label):
    found = tree(image)
    found = {k: v for k, v in found.items() if not k.startswith("lost+found/")}
    manys = {k for k in found if k.startswith("many-")}
    partial = synced in (8, 9) and all(found[k] == "dir" for k in manys) and \
        {k: v for k, v in found.items() if k not in manys} == expected[8]
    #  Step 3 first makes and removes a directory d1/f.
    probe = synced == 2 and found == {**expected[2], "d1/f": "dir"}
    if found not in (expected[synced], expected[synced + 1]) and not partial \
            and not probe:
        extra = set(found) ^ set(expected[synced])
        raise AssertionError(f"{label}: tree is neither step {synced} nor "
                             f"{synced + 1}: differs by {sorted(extra)}")
    for name in ("keep/data", "plain", "victim"):
        if name in found and dump(image, name, root) != (stage / name).read_bytes():
            raise AssertionError(f"{label}: {name} changed")
    if "d1/f" in found and synced >= 3 and dump(image, "d1/f", root) != b"F" * 30 * KIB:
        raise AssertionError(f"{label}: d1/f is not its 30 KiB")


def check_clean(image, label):
    result = run("e2fsck", "-fn", image, check=False)
    if result.returncode:
        raise AssertionError(f"{label}: e2fsck -fn exit {result.returncode}\n{result.stdout}")


def check_benign(image, label):
    """Only leaks and over-counts; then repair, which must leave it clean."""
    result = run("e2fsck", "-fn", image, check=False)
    if result.returncode not in (0, 4):
        raise AssertionError(f"{label}: e2fsck -fn exit {result.returncode}\n{result.stdout}")
    unconnected = set(re.findall(r"Unconnected directory inode (\d+)", result.stdout))
    for line in result.stdout.splitlines():
        line = line.strip()
        dotdot = re.search(r"'\.\.' in .* \((\d+)\) is .*, should be <The NULL inode> \(0\)",
                           line)
        if dotdot and dotdot.group(1) in unconnected:
            continue  # the unnamed directory's own "..", fixed on connection
        if (not line or line.startswith(("e2fsck ", "Pass ", image.name, str(image)))
                or re.match(r"^[-+]?\(?\d", line)):
            continue
        over = OVERCOUNT.search(line)
        if over:
            if int(over.group(2)) <= int(over.group(3)):
                raise AssertionError(f"{label}: link under-count: {line}\n{result.stdout}")
            continue
        if not any(re.search(p, line) for p in BENIGN):
            raise AssertionError(f"{label}: e2fsck finding: {line}\n{result.stdout}")
    run("e2fsck", "-fy", image, check=False)
    check_clean(image, label + " (repaired)")
    return result.returncode == 0


def inode_slots(image, numbers):
    """Byte ranges of the given inodes (debugfs imap)."""
    ranges = []
    size = int(re.search(r"Inode size:\s+(\d+)",
                         run("dumpe2fs", "-h", image).stdout).group(1))
    for number in numbers:
        out = run("debugfs", "-R", f"imap <{number}>", image).stdout
        found = re.search(r"located at block (\d+), offset 0x([0-9a-f]+)", out)
        start = int(found.group(1)) * BLOCK + int(found.group(2), 16)
        ranges.append((start, start + size))
    return ranges


def free_blocks(image):
    """Byte ranges of the blocks free in image (dumpe2fs group listings)."""
    ranges = []
    for listing in re.findall(r"^\s+Free blocks: (.*)$", run("dumpe2fs", image).stdout, re.M):
        for part in filter(None, (p.strip() for p in listing.split(","))):
            lo, _, hi = part.partition("-")
            ranges.append((int(lo) * BLOCK, (int(hi or lo) + 1) * BLOCK))
    return ranges


def compare(a, b, label, ignored=()):
    """Block-identical outside the superblock, the journal superblock and
    ignored byte ranges (orphans each recoverer released its own way)."""
    with open(a, "rb") as fa, open(b, "rb") as fb:
        number = 0
        while True:
            da, db = fa.read(BLOCK), fb.read(BLOCK)
            if not da:
                break
            if number != 1 and da != db and not da.startswith(
                    b"\xc0\x3b\x39\x98\x00\x00\x00\x04"):
                base = number * BLOCK
                for i in range(BLOCK):
                    if da[i] != db[i] and not any(lo <= base + i < hi for lo, hi in ignored):
                        raise AssertionError(f"{label}: block {number} differs")
            number += 1


def synced_of(out):
    return max([int(line.split()[1]) for line in out.splitlines()
                if line.startswith("SYNCED")], default=0)


def journal_checks(crashed, root, stage, expected, synced, label):
    linux, cubit = root / "linux.img", root / "cubit.img"
    shutil.copy(crashed, linux)
    shutil.copy(crashed, cubit)
    replayed = run("e2fsck", "-y", "-E", "journal_only", linux, check=False).stdout
    run(REPLAY, "admit", cubit)
    check_clean(linux, label + " (e2fsck replay)")
    check_clean(cubit, label + " (CuBit replay)")
    # Both free the orphans a crash left (e2fsck like Linux's mount does),
    # each with its own dtime and truncated size in the released inode.
    orphans = [int(n) for n in re.findall(r"Clearing orphaned inode (\d+)", replayed)]
    # Released blocks' old contents differ too (e2fsck zeroes pointers), and
    # CuBit released them through its journal (stale log blocks differ).
    ignored = inode_slots(linux, orphans)
    if orphans:
        ignored += free_blocks(linux) + [
            (int(n) * BLOCK, (int(n) + 1) * BLOCK)
            for n in run("debugfs", "-R", "blocks <8>", linux).stdout.split() if n.isdigit()]
    compare(linux, cubit, label, ignored)
    check_tree(cubit, synced, expected, stage, root, label)
    return 1 if orphans else 0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--stride", type=int, default=1)
    args = parser.parse_args()
    expected = states()
    with tempfile.TemporaryDirectory(prefix="cubit-namespace-") as tmp:
        root = Path(tmp)
        stage = root / "stage"
        (stage / "keep").mkdir(parents=True)
        (stage / "victim").write_bytes(os.urandom(16 * KIB))
        (stage / "keep" / "data").write_bytes(os.urandom(5 * KIB))
        (stage / "plain").write_bytes(os.urandom(KIB))
        counts = {}
        orphaned = 0
        for profile in ("ext2", "ext3"):
            base = root / f"{profile}.img"
            with base.open("wb") as f:
                f.truncate(8 * 1024 * 1024)
            run("mke2fs", "-q", "-t", profile, "-b", BLOCK, "-F", "-d", stage, base)
            full = root / "full.img"
            shutil.copy(base, full)
            out = run(WORKLOAD, full).stdout
            total = int(out.split("DEVICE COMMANDS")[1].split()[0])
            check_clean(full, f"{profile} uninterrupted")
            check_tree(full, 10, expected, stage, root, f"{profile} uninterrupted")
            cuts = faults = 0
            for cut in range(1, total + 1, args.stride):
                for loss in (LOSSES if profile == "ext3" else ("KEEP_ALL",)):
                    crashed = root / "crashed.img"
                    shutil.copy(base, crashed)
                    env = dict(os.environ, CUBIT_CUT=str(cut), CUBIT_LOSS=loss)
                    out = run(WORKLOAD, crashed, env=env).stdout
                    assert "POWER CUT" in out, out
                    synced = synced_of(out)
                    label = f"{profile} cut {cut} {loss} synced {synced}"
                    if profile == "ext3":
                        orphaned += journal_checks(crashed, root, stage, expected, synced, label)
                    else:
                        check_benign(crashed, label)
                        check_tree(crashed, synced, expected, stage, root, label)
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
                    synced = synced_of(out)
                    if profile == "ext3":
                        orphaned += journal_checks(failed, root, stage, expected, synced, label)
                    else:
                        check_benign(failed, label)
                        check_tree(failed, synced, expected, stage, root, label)
                    faults += 1
            counts[profile] = (cuts, faults, total)
        print("NAMESPACE-CHECK: PASS " + ", ".join(
            f"{p}: {c} power cuts, {f} injected errors over {t} commands"
            for p, (c, f, t) in counts.items()) +
            f"; {orphaned} ext3 crashes left an orphan (released by both)")


if __name__ == "__main__":
    main()
