#!/usr/bin/env python3
"""Directory growth and htree directories: Linux-made ext2/ext3 images, the
hosted production CuBit driver (directory_growth.adb), then e2fsprogs.

Disposable temporary images only. Run under nix develop after building
directory_growth.gpr.
"""
import re
import subprocess
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
DRIVER = HERE / "build/directory-growth/directory_growth"
INDEX_FLAG = 0x1000


def run(*args, check=True):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    if check and result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def query(image, command):
    lines = run("debugfs", "-R", command, image).stdout.splitlines()
    return "\n".join(l for l in lines if not l.startswith("debugfs "))


def names(image, directory):
    listing = query(image, f"ls -p {directory}")
    found = set()
    for line in listing.splitlines():
        fields = line.strip().strip("/").split("/")
        # /inode/mode/uid/gid/name/size/
        if len(fields) >= 5 and fields[0] not in ("", "0") and fields[4] not in (".", ".."):
            found.add(fields[4])
    return found


def flags(image, directory):
    return int(re.search(r"Flags: (0x[0-9a-f]+)", query(image, f"stat {directory}")).group(1), 16)


def entry(index):
    return f"entry-{index:05d}-" + "x" * 40


def case(root, block, profile, count):
    stage = root / "stage"
    (stage / "big").mkdir(parents=True)
    indexed = stage / "indexed"
    indexed.mkdir()
    for index in range(400):
        (indexed / f"linux-{index:04d}-padding-padding-padding").write_bytes(b"")
    (indexed / "remove-me").write_bytes(b"")
    (indexed / "rename-me").write_bytes(b"")
    moves = stage / "moves"
    for part in ("a/sub", "b/empty", "b/full"):
        (moves / part).mkdir(parents=True)
    (moves / "a/moved").write_bytes(b"moved")
    (moves / "a/new-version").write_bytes(b"new version")
    (moves / "b/target").write_bytes(b"old version" * 100)
    (moves / "b/index").write_bytes(b"old index")
    (moves / "b/index.lock").write_bytes(b"new index")
    (moves / "a/sub/child").write_bytes(b"child")
    (moves / "b/full/occupant").write_bytes(b"x")
    image = root / "disk.img"
    with image.open("wb") as f:
        f.truncate(64 * 1024 * 1024)
    run("mke2fs", "-q", "-t", profile, "-F", "-b", block, "-N", count + 2000,
        "-O", "dir_index", "-d", stage, image)
    # e2fsck -D builds the htree index Linux would keep for "indexed".
    run("e2fsck", "-fyD", image, check=False)
    run("e2fsck", "-fn", image)
    assert flags(image, "indexed") & INDEX_FLAG, "fixture: no htree index"

    output = run(DRIVER, image, count).stdout
    assert "DIRECTORY-GROWTH: driver done" in output, output

    fsck = run("e2fsck", "-fn", image, check=False)
    assert fsck.returncode == 0, f"e2fsck after CuBit:\n{fsck.stdout}"
    expected = {entry(i) for i in range(count) if i % 3 != 0}
    expected |= {entry(i) for i in range(count, count + 10)}
    expected.discard(entry(count - 1))
    expected.add(entry(count + 20) + "-a-much-longer-name-than-before")
    found = names(image, "big")
    assert found == expected, (f"big: {len(found)} names, expected {len(expected)}; "
                               f"missing {sorted(expected - found)[:5]} "
                               f"extra {sorted(found - expected)[:5]}")
    stat = query(image, "stat big")
    assert "(IND)" in stat, "big never reached its single-indirect block"
    reached_double = "(DIND)" in stat
    indexed_names = names(image, "indexed")
    assert "added-by-cubit" in indexed_names and "renamed-by-cubit" in indexed_names
    assert "remove-me" not in indexed_names and "rename-me" not in indexed_names
    assert len(indexed_names) == 402, len(indexed_names)
    assert not flags(image, "indexed") & INDEX_FLAG, "htree flag not cleared"
    # POSIX renames.
    assert names(image, "moves/a") == set(), names(image, "moves/a")
    assert names(image, "moves/b") == {"arrived", "target", "index", "empty", "full"}, \
        names(image, "moves/b")
    assert query(image, "cat moves/b/target") == "new version"
    assert query(image, "cat moves/b/index") == "new index"
    assert query(image, "cat moves/b/arrived") == "moved"
    assert names(image, "moves/b/empty") == {"child"}
    parent = re.search(r"Inode:\s+(\d+)", query(image, "stat moves/b")).group(1)
    dotdot = [l for l in query(image, "ls -p moves/b/empty").splitlines()
              if l.strip().strip("/").split("/")[4:5] == [".."]]
    assert dotdot and dotdot[0].strip().strip("/").split("/")[0] == parent, dotdot
    long_name = entry(count + 20) + "-a-much-longer-name-than-before"
    assert long_name in names(image, "big")
    # Linux can index the directory again from what CuBit left.
    run("e2fsck", "-fyD", image, check=False)
    run("e2fsck", "-fn", image)
    assert names(image, "indexed") == indexed_names
    return reached_double


def main():
    cases = [(1024, "ext2", 4700), (1024, "ext3", 4700), (4096, "ext2", 1500),
             (4096, "ext3", 1500)]
    doubles = 0
    for block, profile, count in cases:
        with tempfile.TemporaryDirectory(prefix="cubit-directories-") as tmp:
            doubles += case(Path(tmp), block, profile, count)
    assert doubles >= 2, "no case reached a double-indirect directory block"
    print(f"DIRECTORY-GROWTH: PASS {len(cases)} images (indirect and double-indirect "
          f"directory blocks, htree directories; e2fsck clean)")


if __name__ == "__main__":
    main()
