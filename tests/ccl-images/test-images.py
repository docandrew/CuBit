#!/usr/bin/env python3
"""Pure CCL planning, realization-boundary, and archive regression tests."""
import importlib.util
import json
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
TOOL = ROOT / "userspace/ccl/build/image/ccl-image"
ADAPTER = ROOT / "userspace/ccl/tools/ccl-image/realize.py"
spec = importlib.util.spec_from_file_location("ccl_image_realizer", ADAPTER)
realizer = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = realizer
spec.loader.exec_module(realizer)

CATALOG = '''(image-artifacts v1 (catalog "test-v1")
  (artifact "a" repository "a.app")
  (artifact "settings" repository "settings.ccl")
  (bootstrap-requires "resident" "a" "a.app"))'''
PROFILE = '''(system-image v1 (catalog "test-v1")
  (layout bootstrap-only) (provider "resident") (settings "settings")
  (file bootstrap "a" "a.app"))'''

SAMPLES = {path.name for path in (ROOT / "userspace/ccl/samples").glob("*.ccl")}


class Images(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="ccl-images-test.")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.catalog = self.root / "catalog.ccl"
        self.profile = self.root / "image.ccl"
        self.catalog.write_text(CATALOG)
        self.profile.write_text(PROFILE)
        (self.root / "a.app").write_bytes(b"fixture executable")
        (self.root / "settings.ccl").write_text('(system-config v1 (setting "answer" (* 6 7)))')

    def compile(self, profile=PROFILE, catalog=CATALOG):
        self.catalog.write_text(catalog)
        self.profile.write_text(profile)
        result = subprocess.run([TOOL, self.catalog, self.profile],
                                text=True, capture_output=True, timeout=10)
        self.assertNotIn("raised ", result.stderr)
        return result

    def reject(self, profile=PROFILE, catalog=CATALOG, diagnostic=None):
        result = self.compile(profile, catalog)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout, "", "failed evaluation emitted a partial plan")
        if diagnostic:
            self.assertIn(diagnostic, result.stderr)

    def prepare(self, inputs=None, **kwargs):
        return realizer.prepare(self.catalog, self.profile, inputs or {}, root=self.root, **kwargs)

    def test_existing_membership_preserved(self):
        stage1 = {"devmgr.svc", "filesystem.svc", "ata.drv", "nvme.drv",
                  "netstack.svc", "virtio-net.drv", "virtio-gpu.drv", "hda.drv",
                  "mixer.svc", "ps2.drv", "xhci.drv", "config.svc", "netmgr.svc", "procmgr.svc"}
        stage2 = {"logstore.svc", "clock.svc", "display.svc", "desktop.svc",
                  "ccl-workbench.app", "devices.app", "files.app", "storage-check.app", "doom.elf"}
        for name in ("development-initrd", "laptop-initrd", "laptop-usb"):
            with self.subTest(profile=name):
                header, rows, _ = realizer.compile_plan(
                    ROOT / "images/artifacts.ccl", ROOT / f"images/{name}.ccl")
                bootstrap = {row[5] for row in rows if row[0] == "BOOTSTRAP"}
                if name == "development-initrd":
                    self.assertEqual(bootstrap, stage1 | {"system.ccl"})
                elif name == "laptop-initrd":
                    self.assertEqual(bootstrap, stage1 | stage2 |
                                     {"system.ccl", "init.ccl", "live-rw.ext2", "doom1.wad"})
                else:
                    self.assertEqual(header[1], "OPTICAL_IMAGE")
                    self.assertEqual(bootstrap, {"devmgr.svc", "filesystem.svc", "ps2.drv",
                                                "xhci.drv", "system.ccl", "init.ccl", "live-rw.ext2"})
                    apps = {row[5].removeprefix("apps/") for row in rows if row[5].startswith("apps/")}
                    self.assertEqual(apps, (stage1 | stage2 | {"sameboy.app", "sameboy/00.gb", "doom1.wad"})
                                     - {"devmgr.svc", "filesystem.svc", "ps2.drv", "xhci.drv",
                                        "ata.drv", "nvme.drv", "storage-check.app"})
                    self.assertFalse(any("network-check" in row[2] or "ccl-control" in row[2] for row in rows))

    def test_ordinary_ccl_expressions(self):
        result = self.compile(PROFILE.replace('"a.app"', '(concat "a" ".app")'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("\ta.app\n", result.stdout)

    def test_live_ccl_samples(self):
        self.assertTrue(SAMPLES, "the LiveCD must include runnable CCL examples")
        _, rows, _ = realizer.compile_plan(
            ROOT / "images/artifacts.ccl", ROOT / "images/laptop-usb.ccl")
        samples = [row for row in rows if row[5].startswith("samples/ccl/")]
        self.assertEqual({Path(row[5]).name for row in samples}, SAMPLES)
        self.assertEqual(len(samples), len(SAMPLES))
        for region, role, _, kind, source, destination in samples:
            with self.subTest(sample=destination):
                self.assertEqual((region, role, kind), ("OPTICAL", "CONTENT", "REPOSITORY_FILE"))
                original = (ROOT / source).read_bytes()
                self.assertLessEqual(len(original), 1024)
                # Check actual filesystem bytes, not just recipe membership.
                copied = subprocess.run(
                    ["debugfs", "-R", f"cat /work/{Path(destination).name}",
                     ROOT / "kernel/laptop_live_rw.img"], capture_output=True, check=True)
                self.assertEqual(copied.stdout, original)

    def test_bootstrap_and_catalog_checks(self):
        self.reject(PROFILE.replace('"test-v1"', '"wrong-v1"'), diagnostic="CATALOG_MISMATCH")
        self.reject(PROFILE.replace('(file bootstrap "a" "a.app")', ""),
                    diagnostic="MISSING_BOOTSTRAP_DEPENDENCY")
        self.reject(PROFILE.replace('"resident"', '"unknown"'), diagnostic="UNKNOWN_PROVIDER")
        self.reject(PROFILE.replace('"a.app"', '"renamed.app"'), diagnostic="MISSING_BOOTSTRAP_DEPENDENCY")
        self.reject(catalog=CATALOG.replace('"resident" "a"', '"resident" "missing"'),
                    diagnostic="UNKNOWN_ARTIFACT")

    def test_paths_and_duplicate_destinations(self):
        for name in ("", "../a", "/a", "a/../b", "./a", "a//b", "a/", "a.", "a./b", "a\\b", "a\tb", "a;b", "a$HOME"):
            with self.subTest(path=name):
                self.reject(PROFILE.replace('"a.app"', json.dumps(name)), diagnostic="INVALID_PATH")
        for name in ("system.ccl", "system.ccl/child"):
            self.reject(PROFILE.replace('"a.app"', json.dumps(name)), diagnostic="DUPLICATE_DESTINATION")
        self.reject(PROFILE.replace('(settings "settings")', '(settings "settings") (settings "settings")'),
                    diagnostic="DUPLICATE_FIELD")
        self.reject(catalog=CATALOG.replace('(artifact "a"', '(artifact "a" repository "a.app") (artifact "a"'),
                    diagnostic="DUPLICATE_NAME")

    def test_reserved_archive_and_tree_placement(self):
        catalog = CATALOG[:-1] + '''
          (artifact "kernel" repository "kernel")
          (artifact "menu" repository "menu")
          (artifact "start" repository "start.ccl")
          (artifact "tree" supplied-tree "tree"))'''
        profile = PROFILE.replace("bootstrap-only", "optical")[:-1] + '''
          (kernel "kernel") (boot-menu "menu") (startup "start")
          (file optical "a" "boot/initrd.img"))'''
        self.reject(profile, catalog, "INVALID_PLACEMENT")
        self.reject(PROFILE[:-1] + '(file bootstrap "tree" "tree"))', catalog, "INVALID_PLACEMENT")

    def test_missing_unknown_and_trailing_fields(self):
        self.reject(PROFILE.replace('(settings "settings")', ""), diagnostic="MISSING_FIELD")
        self.reject(PROFILE.replace('(layout bootstrap-only)', '(layout potato)'), diagnostic="INVALID_VALUE")
        self.reject(PROFILE + " trailing", diagnostic="INVALID_SYNTAX")
        self.reject(PROFILE.replace('(catalog "test-v1")', '(catalog (clock.monotonic-ms))'),
                    diagnostic="INVALID_SYNTAX")
        self.reject(PROFILE[:-1] + '(run "anything"))', diagnostic="UNKNOWN_DECLARATION")
        self.reject(PROFILE.replace("(system-image v1", "(system-image true"), diagnostic="UNSUPPORTED_VERSION")
        self.reject(PROFILE[:-1] + '(file bootstrap "unknown" "unknown.app"))', diagnostic="UNKNOWN_ARTIFACT")

    def test_bounds_and_malformed_inputs(self):
        self.reject("#" * 8193)
        self.reject(PROFILE.replace('"a.app"', '"' + "a" * 193 + '"'), diagnostic="INVALID_PATH")
        catalog = '(image-artifacts v1 (catalog "test-v1") ' + " ".join(
            f'(artifact "a{i}" repository "a{i}")' for i in range(65)) + ')'
        self.reject(catalog=catalog, diagnostic="TOO_MANY_ITEMS")
        for end in range(0, len(PROFILE), 3):
            self.reject(PROFILE[:end])
        rng = random.Random(2718)
        for _ in range(50):
            result = self.compile("".join(rng.choice('()#"abc 12\n') for _ in range(rng.randrange(100))))
            self.assertEqual(result.returncode, 1)

    def test_symbolic_format_versions(self):
        for token in ("1", "2", "v2", "V1", '"v1"', "(+ 0 1)"):
            with self.subTest(token=token):
                self.reject(PROFILE.replace("system-image v1", "system-image " + token))
                self.reject(catalog=CATALOG.replace("image-artifacts v1", "image-artifacts " + token))

    def test_snapshots_and_hashes(self):
        files, report = self.prepare()
        (self.root / "a.app").write_bytes(b"changed after validation")
        directory = self.root / "stage"
        directory.mkdir()
        bootstrap, _ = realizer.stage(files, directory)
        self.assertEqual((bootstrap / "a.app").read_bytes(), b"fixture executable")
        recorded = next(f for f in report["files"] if f["artifact"] == "a")
        self.assertEqual(recorded["sha256"], realizer.digest(b"fixture executable"))
        self.assertFalse(report["private_inputs"])

    def test_repository_symlink_escape(self):
        (self.root / "a.app").unlink()
        (self.root / "a.app").symlink_to("/etc/hostname")
        with self.assertRaisesRegex(ValueError, "escapes repository"):
            self.prepare()

    def test_case_aliases_rejected_at_realization(self):
        self.profile.write_text(PROFILE[:-1] + '(file bootstrap "a" "A.APP"))')
        with self.assertRaisesRegex(ValueError, "overlapping"):
            self.prepare()

    def test_config_and_startup_checked_before_realization(self):
        (self.root / "settings.ccl").write_text('(startup v1 (start "a.app" (priority 5)))')
        with self.assertRaisesRegex(ValueError, "profile kind mismatch"):
            self.prepare()
        (self.root / "settings.ccl").write_text('(system-config v1 (setting "x" 1))')
        self.catalog.write_text(CATALOG[:-1] + '(artifact "start" repository "start.ccl"))')
        self.profile.write_text(PROFILE[:-1] + '(startup "start"))')
        (self.root / "start.ccl").write_text('(startup v1 (start "missing.app" (priority 5)))')
        with self.assertRaisesRegex(ValueError, "executable absent"):
            self.prepare()

    def test_explicit_input_and_release_rules(self):
        self.catalog.write_text(CATALOG.replace('"a" repository "a.app"', '"a" supplied-file "external"'))
        with self.assertRaisesRegex(ValueError, "missing explicit input"):
            self.prepare()
        self.prepare({"external": self.root / "a.app"})
        with self.assertRaisesRegex(ValueError, "unused supplied"):
            self.prepare({"external": self.root / "a.app", "extra": self.root / "a.app"})
        with self.assertRaisesRegex(ValueError, "release images"):
            self.prepare({"external": self.root / "a.app"}, private_roms=self.root, release=True)

    def test_archive_and_atomic_failure(self):
        files, report = self.prepare()
        output = self.root / "initrd.img"
        realizer.realize(files, report, output)
        realizer.check_cpio(output, files)
        self.assertEqual(json.loads(output.with_name(output.name + ".plan.json").read_text())["output_sha256"],
                         realizer.digest(output.read_bytes()))
        original = output.read_bytes()
        with patch.object(realizer.subprocess, "run", side_effect=subprocess.CalledProcessError(1, "cpio")):
            with self.assertRaises(subprocess.CalledProcessError):
                realizer.realize(files, report, output)
        self.assertEqual(output.read_bytes(), original)

    def optical_fixture(self):
        self.catalog.write_text(CATALOG[:-1] + '''
          (artifact "start" repository "start.ccl")
          (artifact "sample" repository "sample.gb")
          (artifact "notices" supplied-tree "notices"))''')
        self.profile.write_text(PROFILE.replace("bootstrap-only", "optical")[:-1] + '''
          (kernel "a") (boot-menu "a") (startup "start")
          (file optical "sample" "apps/sameboy/00.gb")
          (file optical "notices" "licenses/notices"))''')
        (self.root / "start.ccl").write_text('(startup v1 (start "a.app" (priority 5)))')
        (self.root / "sample.gb").write_bytes(b"T" * 0x200)
        notices = self.root / "notices"
        notices.mkdir()
        (notices / "LICENSE").write_text("fixture notice")
        return {"notices": notices}

    def test_private_cartridges_are_explicit_and_optical(self):
        inputs = self.optical_fixture()
        private = self.root / "private"
        private.mkdir()
        (private / "a private title.gb").write_bytes(b"P" * 0x200)
        files, report = self.prepare(inputs)
        self.assertFalse(report["private_inputs"])
        self.assertFalse(any(f.private for f in files))
        files, report = self.prepare(inputs, private_roms=private)
        cartridge = next(f for f in files if f.private)
        self.assertEqual((cartridge.region, cartridge.destination),
                         ("OPTICAL", "apps/sameboy/01.gb"))
        self.assertEqual(cartridge.data, b"P" * 0x200)
        self.assertNotIn("a private title", json.dumps(report))
        with self.assertRaisesRegex(ValueError, "release images"):
            self.prepare(inputs, private_roms=private, release=True)
        (private / "a private title.gb").write_bytes(b"short")
        with self.assertRaisesRegex(ValueError, "too small"):
            self.prepare(inputs, private_roms=private)

    def test_supplied_tree_escape_and_conflicts(self):
        inputs = self.optical_fixture()
        (inputs["notices"] / "escape").symlink_to("/etc/hostname")
        with self.assertRaisesRegex(ValueError, "symlinks"):
            self.prepare(inputs)
        (inputs["notices"] / "escape").unlink()
        self.profile.write_text(self.profile.read_text()[:-1] +
                                '(file optical "a" "licenses/notices/extra"))')
        with self.assertRaisesRegex(ValueError, "DUPLICATE_DESTINATION"):
            self.prepare(inputs)


if __name__ == "__main__":
    unittest.main()
