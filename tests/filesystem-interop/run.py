#!/usr/bin/env python3
"""Linux-created Ext2 -> hosted production CuBit driver -> e2fsprogs.

All writes target disposable temporary fixtures, never supplied disks.
Run under nix develop after building interop.gpr.
"""
import argparse
from pathlib import Path
import re
import struct
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
PAYLOAD = b"CuBit roundtrip"


def run(*args):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    if result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result.stdout


def debug(image, command):
    return run("debugfs", "-w", "-R", command, image)


def inode_location(image, number):
    with image.open("rb") as f:
        f.seek(1024)
        sb = f.read(1024)
        block = 1024 << struct.unpack_from("<I", sb, 24)[0]
        first = struct.unpack_from("<I", sb, 20)[0]
        per_group = struct.unpack_from("<I", sb, 40)[0]
        stride = struct.unpack_from("<H", sb, 88)[0]
        group, index = divmod(number - 1, per_group)
        f.seek((first + 1) * block + group * 32 + 8)
        table = struct.unpack("<I", f.read(4))[0]
        return table * block + index * stride, stride


def inode_bytes(image, number):
    offset, stride = inode_location(image, number)
    with image.open("rb") as f:
        f.seek(offset)
        return f.read(stride)


def query(image, command):
    """Read-only debugfs query; returns output without the version banner."""
    lines = run("debugfs", "-R", command, image).splitlines()
    return "\n".join(l for l in lines if not l.startswith("debugfs "))


def data_mapping(stat):
    """Logical -> physical data blocks from debugfs stat's BLOCKS listing."""
    mapping = {}
    if "BLOCKS:" not in stat:
        return mapping
    listing = stat.split("BLOCKS:", 1)[1]
    for first, last, physical in re.findall(r"\((\d+)(?:-(\d+))?\):(\d+)", listing):
        first, physical = int(first), int(physical)
        for index in range(int(last or first) - first + 1):
            mapping[first + index] = physical + index
    return mapping


def check_sparse(image, name, size, expected):
    """Check a (possibly huge) sparse file without dumping its holes.

    expected maps logical block -> full block contents; every other block
    must be a hole. Pointer blocks are not data; e2fsck checks their count.
    """
    block = fs_block_size(image)
    stat = query(image, f"stat {name}")
    assert int(re.search(r"Size: (\d+)", stat).group(1)) == size, f"{name}: size"
    mapping = data_mapping(stat)
    assert sorted(mapping) == sorted(expected), f"{name}: mapped {sorted(mapping)}"
    with image.open("rb") as f:
        for logical, contents in expected.items():
            f.seek(mapping[logical] * block)
            visible = min(block, size - logical * block)
            assert f.read(visible) == contents[:visible], f"{name}: block {logical}"


def fs_block_size(image):
    with image.open("rb") as f:
        f.seek(1024 + 24)
        return 1024 << struct.unpack("<I", f.read(4))[0]


def block_of(block, *pieces):
    """One block: zeroes overlaid with (offset, bytes) pieces."""
    data = bytearray(block)
    for offset, piece in pieces:
        data[offset:offset + len(piece)] = piece
    return bytes(data)


def inode_number(image, name):
    return int(re.search(r"Inode:\s+(\d+)", debug(image, f"stat {name}")).group(1))


def poison_free_inode(image):
    # Locate the first free ordinary inode exactly as the allocator scans it.
    with image.open("r+b") as f:
        f.seek(1024)
        sb = f.read(1024)
        block = 1024 << struct.unpack_from("<I", sb, 24)[0]
        first = struct.unpack_from("<I", sb, 20)[0]
        total = struct.unpack_from("<I", sb, 0)[0]
        per_group = struct.unpack_from("<I", sb, 40)[0]
        first_ordinary = struct.unpack_from("<I", sb, 84)[0]
        for group in range((total + per_group - 1) // per_group):
            f.seek((first + 1) * block + group * 32)
            _, bitmap, table = struct.unpack("<III", f.read(12))
            f.seek(bitmap * block)
            bits = f.read(block)
            for i in range(min(per_group, total - group * per_group)):
                number = group * per_group + i + 1
                if number < first_ordinary or bits[i // 8] & (1 << (i % 8)):
                    continue
                stride = struct.unpack_from("<H", sb, 88)[0]
                offset = table * block + i * stride
                # Leave the free inode's standard header alone, but simulate
                # stale bytes from an earlier owner in its extended area.
                f.seek(offset + 128)
                f.write(b"\xa5" * (stride - 128))
                return number
    raise AssertionError("no free inode")


def matrix_case(root, block, stride, profile):
    stage = root / "stage"
    stage.mkdir()
    original = b"E" * 4096
    (stage / "existing").write_bytes(original)
    (stage / "truncate-me").write_bytes(b"T" * (block * 16))
    (stage / "resize-me").write_bytes(b"R" * (block * 16))
    (stage / "resize-gap").write_bytes(b"G" * (block * 16))
    double_length = (12 + 2 * (block // 4) + 4) * block
    (stage / "double-existing").write_bytes(b"D" * double_length)
    first_double = (12 + block // 4) * block
    next_leaf = (12 + 2 * (block // 4)) * block
    with (stage / "double-resize").open("wb") as f:
        f.seek(first_double)
        f.write(b"S" * 64)
        f.seek(next_leaf)
        f.write(b"T" * 64)
    # Sparse triple-indirect trees. A 4 KiB volume without LARGE_FILE cannot
    # represent them; CuBit must then reject growth into that range instead.
    pointers = block // 4
    first_triple = 12 + pointers + pointers * pointers
    middle = pointers * pointers
    triple = not (block == 4096 and profile == "minimal")
    if triple:
        with (stage / "triple-existing").open("wb") as f:
            f.seek((first_triple - 1) * block)
            f.write(b"D" * block + b"X" * 64)
            f.seek((first_triple + middle) * block)
            f.write(b"Y" * 64)
        (stage / "triple-grow").write_bytes(b"g" * 100)
        with (stage / "triple-resize").open("wb") as f:
            f.seek(first_triple * block)
            f.write(b"R" * 64)
            f.seek((first_triple + 1) * block)
            f.write(b"S" * block)
            f.seek((first_triple + middle) * block)
            f.write(b"T" * 64)
        with (stage / "triple-empty").open("wb") as f:
            f.write(b"e" * 100)
            f.seek(first_triple * block)
            f.write(b"E" * 64)
    image = root / "disk.ext2"
    with image.open("wb") as f:
        f.truncate(16 * 1024 * 1024)
    features = (["-O", "none,filetype,ext_attr"] if profile == "minimal" else [])
    # "ext3": the same with an internal journal, which CuBit writes
    # transactions to (data=ordered) and leaves empty and clean on detach.
    run("mke2fs", "-q", "-t", "ext3" if profile == "ext3" else "ext2", "-F",
        "-b", block, "-I", stride, *features, "-d", stage, image)
    debug(image, "ea_set existing user.cubit.test portable-metadata")
    assert "portable-metadata" in debug(image, "ea_get existing user.cubit.test")
    existing_number = inode_number(image, "existing")
    before_existing = inode_bytes(image, existing_number)
    double_number = inode_number(image, "double-existing")
    before_double = inode_bytes(image, double_number)
    if triple:
        triple_number = inode_number(image, "triple-existing")
        before_triple = inode_bytes(image, triple_number)
        assert struct.unpack_from("<I", before_triple, 96)[0] != 0, "needs a triple root"
    next_inode = poison_free_inode(image)
    run("e2fsck", "-fn", image)
    run(HERE / "build/main", image, next_inode)
    new_inode = inode_bytes(image, next_inode)
    assert new_inode[128:] == bytes(stride - 128), "stale inode tail exposed on reuse"
    after_existing = inode_bytes(image, existing_number)
    assert after_existing[128:] == before_existing[128:], "existing extended metadata changed"
    assert inode_bytes(image, double_number) == before_double, "double overwrite changed inode"
    assert "portable-metadata" in debug(image, "ea_get existing user.cubit.test")
    assert "File not found" in debug(image, "stat created")
    expected_new = bytearray(next_leaf + 7 + len(PAYLOAD))
    for offset in (0, block * 2 + 3, block * 14 + 7):
        expected_new[offset:offset + len(PAYLOAD)] = PAYLOAD
    expected_new[first_double + 7:first_double + 17] = PAYLOAD[:10]
    expected_new[next_leaf + 7:next_leaf + 7 + len(PAYLOAD)] = PAYLOAD
    expected_resize = bytearray(next_leaf + 64)
    expected_resize[first_double:first_double + 17] = b"S" * 17
    expected_double = bytearray(b"D" * double_length)
    for offset in ((12 + block // 4) * block + 7, (12 + 2 * (block // 4)) * block - 7):
        expected_double[offset:offset + len(PAYLOAD)] = PAYLOAD
    for name, expected in (("renamed", expected_new),
                           ("existing", PAYLOAD + original[len(PAYLOAD):]),
                           ("truncate-me", PAYLOAD),
                           ("resize-me", b"R" * (13 * block + 17)
                            + bytes(2 * block - 6)),
                           ("resize-gap", b"G" * 17 + bytes(513 - 17) + PAYLOAD),
                           ("double-existing", expected_double),
                           ("double-resize", expected_resize)):
        dump = root / f"{name}.dump"
        debug(image, f"dump {name} {dump}")
        assert dump.read_bytes() == expected, f"{name}: content mismatch"
    if triple:
        assert inode_bytes(image, triple_number) == before_triple, "triple overwrite changed inode"
        check_sparse(image, "triple-existing", (first_triple + middle) * block + 64, {
            first_triple - 1: block_of(block, (0, b"D" * (block - 7)),
                                       (block - 7, PAYLOAD[:7])),
            first_triple: block_of(block, (0, PAYLOAD[7:]), (8, b"X" * 56)),
            first_triple + middle: block_of(block, (0, b"Y" * 64), (20, PAYLOAD))})
        check_sparse(image, "triple-grow",
                     (first_triple + middle + 3) * block + 1 + len(PAYLOAD), {
            0: block_of(block, (0, b"g" * 100)),
            first_triple: block_of(block, (7, PAYLOAD[:10])),
            first_triple + middle + 3: block_of(block, (1, PAYLOAD))})
        check_sparse(image, "triple-resize", (first_triple + middle) * block, {
            first_triple: block_of(block, (0, b"R" * 64)),
            first_triple + 1: block_of(block, (0, b"S" * 5))})
        check_sparse(image, "triple-empty", 0, {})
    run("e2fsck", "-fn", image)
    if profile == "ext3":
        summary = run("dumpe2fs", "-h", image)
        assert "needs_recovery" not in summary, "journal left needing recovery"
        assert re.search(r"Journal start:\s+0\b", summary), "journal left non-empty"
        sequence = int(re.search(r"Journal sequence:\s+(0x[0-9a-f]+)", summary).group(1), 16)
        assert sequence > 2, "no transaction was committed"
    print(f"PASS block={block} inode={stride} profile={profile} triple={triple}", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--one", action="store_true", help="256-byte inode reproduction only")
    args = parser.parse_args()
    cases = [(1024, 256, "default")] if args.one else [
        (b, i, p) for b in (1024, 2048, 4096) for i in (128, 256, 512)
        for p in ("default", "minimal", "ext3")]
    for block, stride, profile in cases:
        with tempfile.TemporaryDirectory(prefix="cubit-ext2-interop-") as tmp:
            matrix_case(Path(tmp), block, stride, profile)
    print(f"EXT2-LINUX-ROUNDTRIP: PASS {len(cases)} images (hosted CuBit driver)")


if __name__ == "__main__":
    main()
