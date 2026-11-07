#!/usr/bin/env python3
"""Hosted checks of the device-build install payload (no native build):
tools/install_payload.py's INDEX lines (whole CuBit names, "executable",
empty directories) and realize.py's reading of a live RAM root (an ext2
image made as tests/device-builds/build.sh makes it). Run inside nix develop
(mke2fs, debugfs): python3 -I tests/device-builds/test_payload.py"""
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools"))
sys.path.insert(0, str(ROOT / "userspace/ccl/tools/ccl-image"))
import install_payload  # noqa: E402
import realize  # noqa: E402


class Index(unittest.TestCase):
    def index(self, **extra):
        files = install_payload.entries(b"e", b"k", b"i", {"init.ccl": b"x"}, **extra)
        return files["INDEX"].decode("ascii").splitlines()

    def test_live_root_lines(self):
        lines = self.index(on_medium={"toolchain/bin/gcc": "@mem:0/toolchain/bin/gcc", "apps/a.svc": "apps/a.svc"},
                           executable=frozenset({"toolchain/bin/gcc"}),
                           directories=frozenset({"tmp", "work/mail/to-host"}))
        self.assertIn("root/toolchain/bin/gcc\t@mem:0/toolchain/bin/gcc\texecutable", lines)
        self.assertIn("root/apps/a.svc\tapps/a.svc", lines)
        self.assertIn("root/tmp/", lines)
        self.assertIn("root/work/mail/to-host/", lines)

    def test_executable_needs_a_source(self):
        with self.assertRaises(ValueError):
            self.index(on_medium={}, executable=frozenset({"x"}))


class LiveRoot(unittest.TestCase):
    def test_reads_modes_and_empty_directories(self):
        with tempfile.TemporaryDirectory(prefix="live-root-") as work:
            tree = Path(work) / "root"
            (tree / "toolchain/bin").mkdir(parents=True)
            (tree / "tmp").mkdir()
            (tree / "toolchain/bin/gcc").write_bytes(b"\x7fELF")
            os.chmod(tree / "toolchain/bin/gcc", 0o755)
            (tree / "toolchain/README").write_text("r")
            image = Path(work) / "root.ext2"
            subprocess.run(["mke2fs", "-q", "-t", "ext2", "-b", "4096", "-F", "-d", str(tree), str(image), "8M"],
                           check=True)
            data = image.read_bytes()
            names, directories = realize.live_root_files(
                [realize.File("BOOTSTRAP", "CONTENT", "live-rw-build", realize.LIVE_ROOT_FILE, data)])
            self.assertEqual(names, [("toolchain/README", False), ("toolchain/bin/gcc", True)])
            self.assertEqual(directories, ["tmp", "toolchain", "toolchain/bin"])


if __name__ == "__main__":
    unittest.main()
