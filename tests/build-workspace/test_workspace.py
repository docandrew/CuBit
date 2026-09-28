"""Hosted regression tests; run with nix develop -c python3 this-file."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("workspace", Path(__file__).resolve().parents[2] /
                                               "tools/build-workspace.py")
workspace = importlib.util.module_from_spec(spec)
spec.loader.exec_module(workspace)


class WorkspaceTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="workspace-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        (self.root / "source.adb").write_text("uncommitted source\n")
        (self.root / "extra.ads").write_text("untracked source\n")
        (self.root / "build-old").mkdir()
        (self.root / "build-old/noise").write_text("not source")

        def fake_git(root, *args):
            if args == ("rev-parse", "HEAD"):
                return b"test-base-commit\n"
            if args == ("ls-files", "-z"):
                return b"source.adb\0deleted.adb\0"
            return b"extra.ads\0build-old/noise\0"
        self.mock_git = patch.object(workspace, "git", side_effect=fake_git)
        self.mock_git.start()
        self.addCleanup(self.mock_git.stop)

    def create(self):
        return workspace.create(self.root, "test")

    def test_copies_working_sources_and_records_hashes(self):
        dest = self.create()
        report = json.loads((dest / workspace.MARKER).read_text())
        self.assertTrue(report["complete"])
        self.assertEqual({e["path"] for e in report["inputs"]}, {"source.adb", "extra.ads"})
        self.assertFalse((dest / "deleted.adb").exists())
        self.assertEqual((dest / "source.adb").read_text(), "uncommitted source\n")

    def test_copies_have_independent_inodes(self):
        first, second = self.create(), self.create()
        for dest in (first, second):
            self.assertNotEqual((dest / "source.adb").stat().st_ino,
                                (self.root / "source.adb").stat().st_ino)
        (first / "source.adb").write_text("private edit")
        self.assertEqual((second / "source.adb").read_bytes(),
                         (self.root / "source.adb").read_bytes())

    def test_run_does_not_need_main_lock(self):
        dest = self.create()
        with workspace.locked(self.root / "coordination/build.lock"):
            result = workspace.run(dest, [sys.executable, "-c",
                "import os,pathlib; pathlib.Path('result').write_text(os.environ['TMPDIR'])"])
        self.assertEqual(result, 0)
        self.assertEqual((dest / "result").read_text(), str(dest / "tmp"))
        self.assertFalse((self.root / "result").exists())

    def test_each_workspace_has_own_exclusion(self):
        first, second = self.create(), self.create()
        with workspace.locked(first / "coordination/build.lock"):
            with self.assertRaisesRegex(RuntimeError, "busy"):
                workspace.run(first, ["true"])
            self.assertEqual(workspace.run(second, ["true"]), 0)

    def test_create_requires_short_shared_lock(self):
        with workspace.locked(self.root / "coordination/build.lock"):
            with self.assertRaisesRegex(RuntimeError, "busy"):
                self.create()

    def test_reject_symlinks_and_path_escape(self):
        dest = self.root / "dest"
        dest.mkdir()
        (self.root / "link").symlink_to("source.adb")
        for name in ("link", "../outside", "/etc/passwd"):
            with self.assertRaises(ValueError):
                workspace.copy_input(self.root, dest, name, "source")

    def test_mutation_rejected_and_incomplete_snapshot_retained(self):
        original = workspace.shutil.copy2
        def mutate(source, target):
            original(source, target)
            source.write_text("changed during copy")
        with patch.object(workspace.shutil, "copy2", side_effect=mutate):
            with self.assertRaisesRegex(RuntimeError, "changed"):
                self.create()
        dest = next((self.root / ".build-workspaces").iterdir())
        with self.assertRaisesRegex(ValueError, "completed"):
            workspace.run(dest, ["true"])

    def test_requires_nix_and_valid_label(self):
        dest = self.create()
        with patch.dict(os.environ, {"IN_NIX_SHELL": ""}):
            with self.assertRaisesRegex(RuntimeError, "nix develop"):
                workspace.run(dest, ["true"])
        for name in ("../escape", "", "x" * 49):
            with self.assertRaises(ValueError):
                workspace.create(self.root, name)

    def test_command_status_propagates(self):
        self.assertEqual(workspace.run(self.create(), [sys.executable, "-c", "exit(23)"]), 23)


if __name__ == "__main__":
    if not os.environ.get("IN_NIX_SHELL"):
        raise SystemExit("Run through nix develop")
    unittest.main()
