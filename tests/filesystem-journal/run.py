#!/usr/bin/env python3
"""JBD2 replay: CuBit's production code against e2fsprogs' own replay.

Journals are written by e2fsprogs (debugfs journal_open/write/close), left
dirty (needs_recovery), then copied. e2fsck -E journal_only replays one copy;
CuBit replays the other (volume admission for ext3, the bare replay engine
for metadata_csum/64-bit volumes Ext2 does not admit). Every block outside
the two superblocks must match, both results must be clean (e2fsck -fn), and
a replayed file must hold its journaled contents. Damaged logs (a torn
commit, a bad checksum) must stop both at the same transaction. Some cases
move the log so that it wraps around the end of the journal area.
All images are disposable; run under nix develop after building journal.gpr.
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
REPLAY = Path(os.environ.get("JOURNAL_REPLAY", HERE / "build/replay"))
MAGIC = 0xC03B3998
SUPERBLOCK = 1024


def run(*args, check=True):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    if check and result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def query(image, command):
    return run("debugfs", "-R", command, image).stdout


def geometry(image):
    with open(image, "rb") as f:
        f.seek(SUPERBLOCK)
        sb = f.read(1024)
    blocks = struct.unpack_from("<I", sb, 4)[0]
    block = 1024 << struct.unpack_from("<I", sb, 24)[0]
    return block, blocks


def block_listing(stat):
    """The mapping section of debugfs stat: block map or extent tree."""
    for marker in ("BLOCKS:", "EXTENTS:"):
        if marker in stat:
            return stat.split(marker, 1)[1]
    raise AssertionError("no block listing")


def journal_map(image):
    """Journal logical -> physical, from debugfs stat's BLOCKS listing."""
    listing = block_listing(query(image, "stat <8>"))
    mapping = {}
    for first, last, physical in re.findall(r"\((\d+)(?:-(\d+))?\):(\d+)", listing):
        first, physical = int(first), int(physical)
        for index in range(int(last or first) - first + 1):
            mapping[first + index] = physical + index
    return [mapping[i] for i in range(len(mapping))]


def debugfs_script(image, lines):
    script = "\n".join(lines) + "\n"
    result = subprocess.run(["debugfs", "-w", "-f", "-", str(image)], input=script,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    if result.returncode:
        raise RuntimeError(result.stdout)
    return result.stdout


def file_blocks(image, name):
    listing = block_listing(query(image, f"stat {name}"))
    blocks = []
    for first, last, physical in re.findall(r"\((\d+)(?:-(\d+))?\):(\d+)", listing):
        first, physical = int(first), int(physical)
        blocks += [physical + i for i in range(int(last or first) - first + 1)]
    return blocks


def read_block(image, number, block):
    with open(image, "rb") as f:
        f.seek(number * block)
        return f.read(block)


def journal_super(image, jmap, block):
    raw = read_block(image, jmap[0], block)
    magic, kind, _, bsize, maxlen, first, sequence, start = struct.unpack_from(">8I", raw, 0)
    assert magic == MAGIC
    return dict(maxlen=maxlen, first=first, sequence=sequence, start=start)


def wrap_log(image, jmap, block, shift):
    """Relocate the log so it starts `shift` blocks before the end of the
    journal area and wraps: tags name home blocks, never log positions."""
    js = journal_super(image, jmap, block)
    area = js["maxlen"] - js["first"]
    with open(image, "r+b") as f:
        def at(logical):
            return jmap[logical] * block
        old = []
        position = js["start"]
        for _ in range(area):
            f.seek(at(position))
            old.append(f.read(block))
            position = js["first"] + (position - js["first"] + 1) % area
        new_start = js["maxlen"] - shift
        for index, data in enumerate(old):
            logical = js["first"] + (new_start - js["first"] + index) % area
            f.seek(at(logical))
            f.write(data)
        f.seek(at(0) + 0x1C)
        f.write(struct.pack(">I", new_start))


def make_image(root, block, features, journal_mib):
    image = root / "base.img"
    with image.open("wb") as f:
        f.truncate(32 * 1024 * 1024)
    run("mke2fs", "-q", "-t", "ext3", "-b", block, "-J", f"size={journal_mib}", "-F",
        *(["-O", features] if features else []), image)
    victim = root / "victim.data"
    victim.write_bytes(bytes((i * 7) % 251 for i in range(16 * block)))
    debugfs_script(image, [f"write {victim} victim"])
    return image


def compare(a, b, block, jmap, name):
    """Every block except the filesystem and journal superblocks."""
    skip = {SUPERBLOCK // block, jmap[0]}
    size = os.path.getsize(a)
    with open(a, "rb") as fa, open(b, "rb") as fb:
        for number in range(size // block):
            da, db = fa.read(block), fb.read(block)
            if number not in skip and da != db:
                raise AssertionError(f"{name}: block {number} differs after replay")


def check_clean(image, name):
    out = run("dumpe2fs", "-h", image).stdout
    assert "needs_recovery" not in out, f"{name}: needs_recovery still set"
    assert re.search(r"Journal start:\s+0\b", out), f"{name}: journal not empty"
    run("e2fsck", "-fn", image)


def scenario(root, block, features, raw, build, name, damage=None, wrap=0):
    base = make_image(root, block, features, block // 1024)  # jbd2's minimum log
    jmap = journal_map(base)
    victims = file_blocks(base, "victim")
    lines, expected = build(root, block, victims)
    debugfs_script(base, ["jo" + (" -c" if "csum" in name else "")] + lines + ["jc"])
    assert "needs_recovery" in run("dumpe2fs", "-h", base).stdout
    if damage:
        damage(base, jmap, block)
    if wrap:
        wrap_log(base, jmap, block, wrap)
    linux = root / "linux.img"
    cubit = root / "cubit.img"
    shutil.copy(base, linux)
    shutil.copy(base, cubit)
    run("e2fsck", "-y", "-E", "journal_only", linux, check=False)
    _, fs_blocks = geometry(cubit)
    if raw:
        mapfile = root / "journal.map"
        mapfile.write_text("\n".join(map(str, jmap)) + "\n")
        out = run(REPLAY, "raw", cubit, block, fs_blocks, mapfile).stdout
    else:
        out = run(REPLAY, "admit", cubit).stdout
    compare(linux, cubit, block, jmap, name)
    # Both continue after the last replayed transaction and skip the one
    # that stopped replay (jbd2: the next transaction is end + 1), so no
    # leftover block of a torn transaction can pass for a new one.
    theirs = journal_super(linux, jmap, block)["sequence"]
    ours = journal_super(cubit, jmap, block)["sequence"]
    assert ours == theirs, f"{name}: next sequence {ours}, e2fsck {theirs}"
    check_clean(linux, name + " (e2fsck)")
    if raw:
        js = journal_super(cubit, jmap, block)
        assert js["start"] == 0, f"{name}: CuBit left the journal dirty"
    else:
        check_clean(cubit, name)
    if damage is None and name != "csum-v1-revoke":
        # Damaged logs are judged by agreement with e2fsck alone, as is a v1
        # revoke-only transaction: debugfs gives its commit a checksum jbd2's
        # rule (descriptor and data blocks only) does not reproduce, so both
        # stop before it.
        for number, contents in expected.items():
            assert read_block(cubit, number, block) == contents, f"{name}: block {number}"
    print(f"PASS {name} block={block} {out.strip().splitlines()[-1]}", flush=True)


def payload(root, block, blocks, fill):
    path = root / f"payload-{fill}.bin"
    data = bytes([fill]) * (block * len(blocks))
    path.write_bytes(data)
    return path, {b: bytes([fill]) * block for b in blocks}


def build_plain(root, block, victims):
    p1, e1 = payload(root, block, victims[:3], 0xA1)
    p2, e2 = payload(root, block, victims[3:5], 0xB2)
    p3, e3 = payload(root, block, victims[1:2], 0xC3)  # later rewrite wins
    lines = [f"jw -b {','.join(map(str, victims[:3]))} {p1}",
             f"jw -b {','.join(map(str, victims[3:5]))} {p2}",
             f"jw -b {victims[1]} {p3}"]
    expected = {**e1, **e2, **e3}
    return lines, expected


def build_revoke(root, block, victims):
    p1, e1 = payload(root, block, victims[:3], 0xD4)
    lines = [f"jw -b {','.join(map(str, victims[:3]))} {p1}",
             f"jw -r {victims[0]}"]  # the later revoke cancels the earlier copy
    original = bytes((i * 7) % 251 for i in range(16 * block))
    expected = {victims[1]: e1[victims[1]], victims[2]: e1[victims[2]],
                victims[0]: original[0:block]}
    return lines, expected


def build_escape(root, block, victims):
    path = root / "escape.bin"
    data = struct.pack(">I", MAGIC) + b"\x5a" * (block - 4)
    path.write_bytes(data)
    return [f"jw -b {victims[6]} {path}"], {victims[6]: data}


def build_many(root, block, victims):
    lines, expected = [], {}
    for index in range(40):
        target = victims[index % 16]
        path, e = payload(root, block, [target], index + 1)
        lines.append(f"jw -b {target} {path}")
        expected.update(e)
    return lines, expected


def torn_commit(image, jmap, block):
    """Zero the last commit block: its transaction is incomplete."""
    js = journal_super(image, jmap, block)
    last_commit = None
    for logical in range(js["first"], js["maxlen"]):
        raw = read_block(image, jmap[logical], block)
        magic, kind, _ = struct.unpack_from(">3I", raw, 0)
        if magic == MAGIC and kind == 2:
            last_commit = logical
    with open(image, "r+b") as f:
        f.seek(jmap[last_commit] * block)
        f.write(bytes(block))


def bad_checksum(image, jmap, block):
    """Flip a byte of the last transaction's first data block."""
    js = journal_super(image, jmap, block)
    last_descriptor = None
    for logical in range(js["first"], js["maxlen"]):
        raw = read_block(image, jmap[logical], block)
        magic, kind, _ = struct.unpack_from(">3I", raw, 0)
        if magic == MAGIC and kind == 1:
            last_descriptor = logical
    with open(image, "r+b") as f:
        f.seek(jmap[last_descriptor + 1] * block + 100)
        byte = f.read(1)[0]
        f.seek(jmap[last_descriptor + 1] * block + 100)
        f.write(bytes([byte ^ 0xFF]))


def main():
    parser = argparse.ArgumentParser()
    parser.parse_args()
    cases = 0
    for block in (1024, 2048, 4096):
        for name, build, damage, wrap in (
                ("plain", build_plain, None, 0),
                ("revoke", build_revoke, None, 0),
                ("escape", build_escape, None, 0),
                ("many", build_many, None, 0),
                ("wrapped", build_many, None, 7),
                ("torn-commit", build_many, torn_commit, 0),
                ("csum-v1", build_plain, None, 0),
                ("csum-v1-revoke", build_revoke, None, 0),
                ("csum-v1-bad", build_many, bad_checksum, 0),
                ("csum-v1-wrapped", build_many, None, 5)):
            with tempfile.TemporaryDirectory(prefix="cubit-jbd2-") as tmp:
                scenario(Path(tmp), block, None, False, build, name, damage, wrap)
                cases += 1
        for name, features in (("csum-v3", "metadata_csum"),
                               ("csum-v3-64bit", "metadata_csum,64bit,extent")):
            with tempfile.TemporaryDirectory(prefix="cubit-jbd2-") as tmp:
                scenario(Path(tmp), block, features, True, build_revoke, name)
                cases += 1
    print(f"JBD2-REPLAY-CHECK: PASS {cases} journals (CuBit replay = e2fsck replay)")


if __name__ == "__main__":
    main()
