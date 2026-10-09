"""Hosted wrapper routing; fake realization, no native build or image claim."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class Profiles(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="live-profile-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        for path in ("kernel", "bin", "notices", "candidate", "images",
                     "userspace/mesa/build", "userspace/ccl/tools/ccl-image",
                     "tests/mesa-teapot", "tests/hardware", "tests/usb-optical", "tools"):
            (self.root / path).mkdir(parents=True, exist_ok=True)
        for path in ("tests/usb-optical/build-live.sh",
                     "tests/hardware/verify-desktop-mesa-startup.py",
                     "tools/verify_desktop_vulkan_compositor.py"):
            shutil.copy2(ROOT / path, self.root / path)
        (self.root / "notices/MESA-SOURCE.tar.gz").write_bytes(b"fixture")
        (self.root / "userspace/mesa/build/notice-path").write_text(str(self.root / "notices"))
        (self.root / "tests/mesa-teapot/teapot-control-points.h").write_text("fixture")
        for tool, body in {"make": 'touch "$PROFILE_TEST_ROOT/make-called"',
                           "nix": 'echo fixture-input'}.items():
            path = self.root / "bin" / tool
            path.write_text("#!/bin/sh\n" + body + "\n")
            path.chmod(0o755)
        (self.root / "userspace/ccl/tools/ccl-image/realize.py").write_text(
            "import json, os, pathlib, sys\n"
            "pathlib.Path(os.environ['PROFILE_TEST_ROOT'], 'arguments.json').write_text(json.dumps(sys.argv[1:]))\n")
        data = b"\x7fELFfixture"
        candidate = self.root / "candidate"
        (candidate / "desktop.svc").write_bytes(data)
        (candidate / "source.adb").write_text("fixture source")
        manifest = json.dumps({"source.adb": hashlib.sha256(b"fixture source").hexdigest()}).encode()
        (candidate / "sources.json").write_bytes(manifest)
        self.record = dict(status="LINKED", backend="vulkan-runtime-dispatch",
                           gpu_drawing_enabled=True, binary="desktop.svc",
                           binary_bytes=len(data), binary_sha256=hashlib.sha256(data).hexdigest(),
                           source_manifest="sources.json", source_manifest_sha256=hashlib.sha256(manifest).hexdigest())
        (candidate / "compositor-result.json").write_text(json.dumps(self.record))
        shutil.copy2(candidate / "desktop.svc", candidate / "desktop-vulkan-link.svc")
        legacy = dict(self.record, gpu_drawing_enabled=False,
                      admitted_device_startup_enabled=True, admitted_startup=True, optional_render_probe=True)
        (candidate / "result.json").write_text(json.dumps(legacy))
        self.env = dict(os.environ, PATH=str(self.root / "bin") + os.pathsep + os.environ["PATH"],
                        PROFILE_TEST_ROOT=str(self.root), TMPDIR=str(self.root),
                        CUBIT_GRUB_EFI_DIR="fixture-efi", SAMEBOY_SRC="fixture-sameboy",
                        SAMEBOY_LIBM_NOTICES="fixture-libm",
                        CUBIT_DESKTOP_VULKAN_DIR=str(candidate), CUBIT_DESKTOP_MESA_DIR=str(candidate))
        self.env.pop("CUBIT_LIVE_OUTPUT", None)
        self.env.pop("SAMEBOY_ROMS_DIR", None)

    def run_profile(self, option=None):
        return subprocess.run(["bash", str(self.root / "tests/usb-optical/build-live.sh"),
                               "fixture.wad", "--uefi"] + ([option] if option else []),
                              env=self.env, capture_output=True, text=True, timeout=10)

    def test_routes(self):
        for option, profile, output, binary in (
            (None, "laptop-usb", "cubit_live_uefi.img", None),
            ("--desktop-mesa-startup", "desktop-mesa-startup", "cubit_live_desktop_mesa_startup.img", "desktop-vulkan-link.svc"),
            ("--desktop-vulkan-compositor", "desktop-mesa-startup", "cubit_live_desktop_vulkan_compositor.img", "desktop.svc")):
            with self.subTest(option=option):
                result = self.run_profile(option)
                self.assertEqual(result.returncode, 0, result.stderr)
                args = json.loads((self.root / "arguments.json").read_text())
                self.assertEqual(args[0], f"../images/{profile}.ccl")
                self.assertEqual(args[-2:], ["--output", output])
                supplied = [v for v in args if v.startswith("desktop-mesa-startup=")]
                self.assertEqual(supplied, [] if binary is None else
                                 [f"desktop-mesa-startup={self.root / 'candidate' / binary}"])

    def test_drawing_disabled_rejected_before_build(self):
        self.record["gpu_drawing_enabled"] = False
        (self.root / "candidate/compositor-result.json").write_text(json.dumps(self.record))
        result = self.run_profile("--desktop-vulkan-compositor")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("gpu_drawing_enabled", result.stderr)
        self.assertFalse((self.root / "make-called").exists())
        self.assertFalse((self.root / "arguments.json").exists())


if __name__ == "__main__":
    unittest.main()
