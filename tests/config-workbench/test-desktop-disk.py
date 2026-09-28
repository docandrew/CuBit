#!/usr/bin/env python3
"""Real ext2 host-tool tests; every writable image is in a temporary directory."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("desktop_disk",
    Path(__file__).resolve().parents[2] / "tools/prepare_desktop_disk.py")
disk = importlib.util.module_from_spec(spec)
spec.loader.exec_module(disk)


class DesktopDiskTests(unittest.TestCase):
    block_size = 1024

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="cubit-desktop-disk.")
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.base = self.root / "base.img"
        self.output = self.root / "scratch.img"
        stage = self.root / "root"
        stage.mkdir()
        (stage / "keep.txt").write_text("untouched base file")
        (stage / "init.ccl").write_text("old startup")
        with self.base.open("wb") as stream:
            stream.truncate(16 * 1024 * 1024)
        subprocess.run(["mke2fs", "-q", "-t", "ext2", "-b", str(self.block_size),
                        "-F", "-d", str(stage), str(self.base)], check=True)
        self.original = disk.digest(self.base)
        self.payload = self.root / 'source with "quotes".ccl'
        self.payload.write_text("(startup v1)")

    def tearDown(self):
        self.assertEqual(disk.digest(self.base), self.original)

    def extract(self, name):
        target = self.root / "dumped"
        target.unlink(missing_ok=True)
        subprocess.run(["debugfs", "-R", f'dump /{name} "{target}"', str(self.output)],
                       check=True, capture_output=True)
        return target.read_bytes()

    def test_overlay_and_preservation(self):
        disk.prepare(self.base, self.output, [("init.ccl", self.payload),
                     ("work/nested/sample.ccl", self.payload)])
        self.assertEqual(self.extract("init.ccl"), self.payload.read_bytes())
        self.assertEqual(self.extract("work/nested/sample.ccl"), self.payload.read_bytes())
        self.assertEqual(self.extract("keep.txt"), b"untouched base file")

    def test_growth_for_large_worker(self):
        worker = self.root / "worker.svc"
        with worker.open("wb") as stream:
            for index in range(24):
                stream.write(bytes([index + 1]) * 1024 * 1024)
        disk.prepare(self.base, self.output, [("config-storage.svc", worker)])
        self.assertEqual(self.extract("config-storage.svc"), worker.read_bytes())
        self.assertGreater(self.output.stat().st_size, self.base.stat().st_size)

    def test_existing_output_requires_explicit_replace(self):
        self.output.write_bytes(b"previous output")
        with self.assertRaises(FileExistsError):
            disk.prepare(self.base, self.output, [("init.ccl", self.payload)])
        self.assertEqual(self.output.read_bytes(), b"previous output")
        disk.prepare(self.base, self.output, [("init.ccl", self.payload)], replace=True)
        self.assertEqual(self.extract("init.ccl"), self.payload.read_bytes())

    def test_bad_inputs_never_publish(self):
        cases = [[("../escape", self.payload)], [("/absolute", self.payload)],
                 [("a\nb", self.payload)], [("a", self.payload), ("a", self.payload)],
                 [("a", self.payload), ("a/b", self.payload)],
                 [("missing", self.root / "missing")]]
        for files in cases:
            with self.subTest(files=files), self.assertRaises(ValueError):
                disk.prepare(self.base, self.output, files)
        self.assertFalse(self.output.exists())
        with self.assertRaises(ValueError):
            disk.prepare(self.base, self.base, [], replace=True)
        self.output.hardlink_to(self.base)
        with self.assertRaises(ValueError):
            disk.prepare(self.base, self.output, [], replace=True)

    def test_success_exit_without_written_payload_does_not_publish(self):
        self.output.write_bytes(b"previous output")
        run = subprocess.run

        def fail_write(command, **kwargs):
            if command[0] == "debugfs" and any(arg.startswith("write ") for arg in command):
                return subprocess.CompletedProcess(command, 0, "", "ENOSPC")
            return run(command, **kwargs)

        with patch.object(disk.subprocess, "run", side_effect=fail_write):
            with self.assertRaises(RuntimeError):
                disk.prepare(self.base, self.output, [("init.ccl", self.payload)], replace=True)
        self.assertEqual(self.output.read_bytes(), b"previous output")

    def test_same_length_corruption_does_not_publish(self):
        self.output.write_bytes(b"previous output")
        run = subprocess.run

        def corrupt_write(command, **kwargs):
            if command[0] == "debugfs":
                request = command[command.index("-R") + 1]
                if request.startswith("write "):
                    staged = Path(kwargs["cwd"]) / request.split()[1]
                    staged.write_bytes(b"X" * staged.stat().st_size)
            return run(command, **kwargs)

        with patch.object(disk.subprocess, "run", side_effect=corrupt_write):
            with self.assertRaises(RuntimeError):
                disk.prepare(self.base, self.output, [("init.ccl", self.payload)], replace=True)
        self.assertEqual(self.output.read_bytes(), b"previous output")

    def test_final_fsck_failure_does_not_publish(self):
        self.output.write_bytes(b"previous output")
        run = subprocess.run

        def fail_final_check(command, **kwargs):
            if command[0] == "e2fsck" and command[-1] == "disk.img":
                raise subprocess.CalledProcessError(4, command)
            return run(command, **kwargs)

        with patch.object(disk.subprocess, "run", side_effect=fail_final_check):
            with self.assertRaises(subprocess.CalledProcessError):
                disk.prepare(self.base, self.output, [("init.ccl", self.payload)], replace=True)
        self.assertEqual(self.output.read_bytes(), b"previous output")


class DesktopFourKiBDiskTests(DesktopDiskTests):
    block_size = 4096

    def test_browser_sized_payload_keeps_four_k_blocks(self):
        browser = self.root / "browser.app"
        with browser.open("wb") as stream:
            for index in range(85):
                stream.write(bytes([index + 1]) * 1024 * 1024)
        disk.prepare(self.base, self.output, [("browser.app", browser)])
        self.assertEqual(self.extract("browser.app"), browser.read_bytes())
        # ext2 s_log_block_size: 1024 << 2 = 4096. Growing an existing
        # filesystem must preserve its geometry, not silently reformat it.
        with self.output.open("rb") as stream:
            stream.seek(1024 + 24)
            self.assertEqual(int.from_bytes(stream.read(4), "little"), 2)


if __name__ == "__main__":
    unittest.main()
