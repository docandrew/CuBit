#!/usr/bin/env python3
"""Exercise actual debugfs replacement on a private ext2 disk."""
import pathlib
import subprocess
import tempfile
from install_fixture import install

with tempfile.TemporaryDirectory(prefix="cubit-servo-fixture-test-") as directory:
    root = pathlib.Path(directory)
    disk, source, copied = (root / name for name in ("disk.img", "source", "copied"))
    with disk.open("wb") as output:
        output.truncate(8 * 1024 * 1024)
    subprocess.run(["mke2fs", "-q", "-t", "ext2", "-F", str(disk)], check=True)
    for payload in (b"old development pages\n", b"new controlled pages\n", b"", bytes(range(256)) * 1024):
        source.write_bytes(payload)
        install(disk, source, "fixture")
        copied.unlink(missing_ok=True)
        subprocess.run(["debugfs", "-R", f"dump fixture {copied}", str(disk)],
                       check=True, capture_output=True)
        assert copied.read_bytes() == payload
    try:
        install(disk, source, "missing-directory/fixture")
    except RuntimeError:
        pass
    else:
        raise AssertionError("debugfs false success was accepted")
print("SERVO-FIXTURE-INSTALL: PASS replacement, empty flag, large bytes, failed write rejected")
