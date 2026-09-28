#!/usr/bin/env python3
"""Before/after checks on the headless runner's disposable bench-storage disk."""
import argparse
import json
from pathlib import Path
import tempfile

from run import debug, inode_bytes, inode_number, poison_free_inode, run


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("phase", choices=("before", "after"))
    parser.add_argument("image", type=Path)
    parser.add_argument("state", type=Path)
    args = parser.parse_args()
    if args.phase == "before":
        run("e2fsck", "-fn", args.image)
        number = poison_free_inode(args.image)
        run("e2fsck", "-fn", args.image)
        stride = len(inode_bytes(args.image, number))
        args.state.write_text(json.dumps({"inode": number, "inode_bytes": stride}))
        print(f"EXT2-NATIVE-PRECHECK: PASS (clean image, poisoned free inode tail, inode_bytes={stride})")
        return
    number = json.loads(args.state.read_text())["inode"]
    assert inode_number(args.image, "cubit-latency.dat") == number
    data = inode_bytes(args.image, number)
    assert data[128:] == bytes(len(data) - 128), "native allocation retained stale inode tail"
    with tempfile.TemporaryDirectory(prefix="cubit-ext2-native-dump-") as tmp:
        dump = Path(tmp) / "payload"
        debug(args.image, f"dump cubit-latency.dat {dump}")
        expected = b"Z" * 4096 + b"".join(bytes([65 + i]) * 4096 for i in range(1, 16))
        assert dump.read_bytes() == expected, "native benchmark payload mismatch"
    run("e2fsck", "-fn", args.image)
    print("EXT2-NATIVE-ROUNDTRIP: PASS (inode reuse, 64 KiB payload, e2fsck)")


if __name__ == "__main__":
    main()
