#!/usr/bin/env python3
"""Pure CCL planning, realization-boundary, and archive regression tests."""
import importlib.util
import json
from pathlib import Path
import random
import re
import shutil
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
  (builder host-development (import "test-ca") (identities "*") (ceiling hardware-and-system))
  (file bootstrap "a" "a.app"))'''
BUILDER = '(builder host-development (import "test-ca") (identities "*") (ceiling hardware-and-system))'
SIGN_TOOL = ROOT / "userspace/ccl/build/sign/ccl-sign"

SAMPLES = {path.name for path in (ROOT / "userspace/ccl/samples").glob("*.ccl")}


class Images(unittest.TestCase):
    def test_efi_generated_loader_allowlist(self):
        for name in ('efi.img', 'efi/boot/bootx64.efi', 'mach_kernel',
                     'System/Library/CoreServices/boot.efi',
                     '.disk/2026-09-26-17-42-25-00.uuid'):
            self.assertFalse(realizer.generated_loader(name))
            self.assertTrue(realizer.generated_loader(name, efi=True))
        for name in ('efi/evil.app', 'System/Library/CoreServices/evil.app',
                     '.disk/evil.uuid', '.disk/2026-09-26-17-42-25-00.uuid.app',
                     'apps/evil.app'):
            self.assertFalse(realizer.generated_loader(name, efi=True))

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
        (self.root / "test-ca.der").write_bytes(b"not used: the fixture holds no executable")
        self.imports = {"test-ca": self.root / "test-ca.der"}

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
        kwargs.setdefault("imports", self.imports)
        return realizer.prepare(self.catalog, self.profile, inputs or {}, root=self.root, **kwargs)

    def test_existing_membership_preserved(self):
        stage1 = {"devmgr.svc", "filesystem.svc", "ramdisk.drv", "ata.drv", "nvme.drv",
                  "netstack.svc", "virtio-net.drv", "virtio-gpu.drv", "hda.drv",
                  "mixer.svc", "ps2.drv", "xhci.drv", "config.svc", "netmgr.svc", "procmgr.svc"}
        stage2 = {"logstore.svc", "clock.svc", "display.svc", "desktop.svc",
                  "ccl-workbench.app", "devices.app", "files.app", "config-inspector.app",
                  "storage-check.app", "doom.elf"}
        for name in ("development-initrd", "laptop-initrd", "laptop-usb"):
            with self.subTest(profile=name):
                header, rows, _ = realizer.compile_plan(
                    ROOT / "images/artifacts.ccl", ROOT / f"images/{name}.ccl")
                bootstrap = {row[5] for row in rows if row[0] == "BOOTSTRAP"}
                if name == "development-initrd":
                    # Every driver is in every image (HW-002), and devmgr binds
                    # them by the generated driver index.
                    self.assertEqual(bootstrap, stage1 | {"system.ccl", "virtio-input.drv", "intel-gpu.drv",
                                                         "generic-check.drv", "gpio-launch-check.drv",
                                                         "hid-launch-check.drv"})
                elif name == "laptop-initrd":
                    self.assertEqual(bootstrap, stage1 | stage2 |
                                     {"system.ccl", "init.ccl", "live-rw.ext2", "doom1.wad",
                                      "virtio-input.drv", "intel-gpu.drv",
                                      # The touchpad (SUP-001).
                                      "gpio.drv", "i2c-dw.drv", "hid-i2c.drv"})
                else:
                    self.assertEqual(header[1], "OPTICAL_IMAGE")
                    self.assertEqual(bootstrap, {"devmgr.svc", "filesystem.svc", "ramdisk.drv", "ps2.drv",
                                                "xhci.drv", "ata.drv", "nvme.drv", "virtio-input.drv",
                                                "system.ccl", "init.ccl", "live-rw.ext2"})
                    apps = {row[5].removeprefix("apps/") for row in rows if row[5].startswith("apps/")}
                    self.assertEqual(apps, (stage1 | stage2 | {"sameboy.app", "sameboy/00.gb", "doom1.wad", "config-storage.svc", "cubitshell.app",
                                                            "intel-gpu.drv", "firmware/intel/tgl_guc_70.bin", "teapots.app",
                                                            "boot-logs.app", "logs.app", "ccl-console.app", "gpu-viewer.app",
                                                            "calculator.app", "lexicon.svc", "dictionary.app", "bible.app",
                                                            # The agent path (AGENT-002).
                                                            "tls.svc", "timesync.svc", "secrets.svc", "llm.svc",
                                                            "agent.app", "tls/roots.der",
                                                            "Assets/cubit-wallpapers/2",
                                                            "Assets/cubit-icons/5/applications.qoi",
                                                            "Assets/cubit-lexicon/1/english.lexicon",
                                                            "Assets/cubit-lexicon/1/LEXICON-NOTICE",
                                                            "Assets/cubit-bible/1"})
                                     - {"devmgr.svc", "filesystem.svc", "ramdisk.drv", "ps2.drv", "xhci.drv",
                                        "ata.drv", "nvme.drv", "storage-check.app"})
                    self.assertFalse(any("network-check" in row[2] or "ccl-control" in row[2] for row in rows))
                    firmware = [row for row in rows if row[2] in
                                {"intel-guc", "intel-firmware-license"}]
                    self.assertEqual({row[5] for row in firmware},
                                     {"apps/firmware/intel/tgl_guc_70.bin", "licenses/Intel-GPU.txt"})
                    self.assertEqual(len(firmware), 2)
                    self.assertTrue(all(row[0] == "OPTICAL" for row in firmware))
                    mesa = [row for row in rows if row[2] in {"teapots", "mesa-notices"}]
                    self.assertEqual({row[5] for row in mesa}, {"apps/teapots.app", "licenses/mesa"})
                    self.assertEqual(len(mesa), 2)
                    self.assertTrue(all(row[0] == "OPTICAL" for row in mesa))

    def test_build_images_place_every_laptop_app(self):
        # The laptop-build images are laptop-usb plus the build tools; an app
        # the launch menu offers but the image omits fails with "the program
        # file was not found" (Bible on image 9, 2026-11-09).
        def apps(name):
            _, rows, _ = realizer.compile_plan(ROOT / "images/artifacts.ccl", ROOT / f"images/{name}.ccl")
            return {row[5] for row in rows if row[5].startswith("apps/")}
        usb = apps("laptop-usb")
        for name in ("laptop-build", "laptop-build-test"):
            with self.subTest(profile=name):
                self.assertEqual(usb - apps(name), set())

    def test_touchpad_drivers_and_one_pnp0c50_driver(self):
        # SUP-001: the laptop images carry the touchpad's three drivers, and
        # no image carries two PNP0C50 drivers (devmgr would refuse the
        # ambiguous match): hid-launch-check.drv is the development
        # image's, hid-i2c.drv the hardware images'.
        def names(profile):
            _, rows, _ = realizer.compile_plan(ROOT / "images/artifacts.ccl", ROOT / f"images/{profile}.ccl")
            return {row[2] for row in rows}
        for profile in ("laptop-build", "laptop-build-test", "laptop-install", "laptop-initrd"):
            with self.subTest(profile=profile):
                self.assertLessEqual({"gpio", "i2c-dw", "hid-i2c"}, names(profile))
        for profile in sorted((ROOT / "images").glob("*.ccl")):
            if profile.name == "artifacts.ccl":
                continue
            with self.subTest(profile=profile.name):
                self.assertFalse({"hid-i2c", "hid-launch-check"} <= names(profile.stem))

    def test_graphics_profiles_include_diagnostics_and_console(self):
        for name in ('laptop-usb', 'render-session', 'mesa-device',
                     'mesa-triangle', 'mesa-triangle-window', 'desktop-mesa-startup'):
            with self.subTest(profile=name):
                _, rows, _ = realizer.compile_plan(
                    ROOT / 'images/artifacts.ccl', ROOT / f'images/{name}.ccl')
                for artifact in ('logs', 'ccl-console'):
                    matches = [row for row in rows if row[2] == artifact]
                    self.assertEqual(len(matches), 1)
                    self.assertEqual((matches[0][0], matches[0][5]),
                                     ('OPTICAL', f'apps/{artifact}.app'))

    def test_every_image_profile_plans(self):
        # Every driver is in every image (HW-002), so placement counts grow
        # with the driver set; the realizer must not cap them.
        catalog = ROOT / "images/artifacts.ccl"
        for profile in sorted((ROOT / "images").glob("*.ccl")):
            if profile.name == "artifacts.ccl":
                continue
            with self.subTest(profile=profile.name):
                _, rows, _ = realizer.compile_plan(catalog, profile)
                self.assertTrue(rows)

    def test_icon_atlas_placed_where_desktop_reads_it(self):
        # Desktop reads its icon atlas from the exact package version it was
        # built against (Desktop_Icon_Assets); a profile that places another
        # version leaves every icon as the fallback glyph (laptop image 5).
        spec = (ROOT / "userspace/services/desktop/desktop_icon_assets.ads").read_text()
        constant = lambda name: re.search(rf'{name}\s*: constant String := "([^"]*)"', spec)[1]
        expected = f"apps/Assets/{constant('Package_Path')}{constant('File_Name')}"
        catalog = ROOT / "images/artifacts.ccl"
        placed = 0
        for profile in sorted((ROOT / "images").glob("*.ccl")):
            if profile.name == "artifacts.ccl":
                continue
            _, rows, _ = realizer.compile_plan(catalog, profile)
            for row in rows:
                if row[2] == "icons-applications":
                    placed += 1
                    with self.subTest(profile=profile.name):
                        self.assertEqual(row[5], expected)
        self.assertTrue(placed)

    def test_mesa_device_profile_is_opt_in(self):
        catalog = ROOT / "images/artifacts.ccl"
        _, normal, _ = realizer.compile_plan(catalog, ROOT / "images/laptop-usb.ccl")
        _, session, _ = realizer.compile_plan(catalog, ROOT / "images/render-session.ccl")
        _, mesa, _ = realizer.compile_plan(catalog, ROOT / "images/mesa-device.ccl")
        self.assertFalse(any(row[2] == "mesa-logical-device" for row in normal + session))
        device, = [row for row in mesa if row[2] == "mesa-logical-device"]
        self.assertEqual(device[0], "OPTICAL")
        self.assertEqual(device[5], "apps/mesa-logical-device.app")
        self.assertFalse(any(row[2] == "render-session" for row in mesa))
        self.assertTrue(any(row[2] == "init-mesa-device" for row in mesa))
        # Only the selected fixture/startup differ; retain firmware and licenses.
        excluded = {"render-session", "mesa-logical-device",
                    "init-render-session", "init-mesa-device"}
        self.assertEqual([r for r in session if r[2] not in excluded],
                         [r for r in mesa if r[2] not in excluded])

    def test_ordinary_ccl_expressions(self):
        result = self.compile(PROFILE.replace('"a.app"', '(concat "a" ".app")'))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("\ta.app\n", result.stdout)

    def test_mesa_triangle_profile_is_opt_in(self):
        catalog = ROOT / "images/artifacts.ccl"
        _, device, _ = realizer.compile_plan(catalog, ROOT / "images/mesa-device.ccl")
        _, triangle, _ = realizer.compile_plan(catalog, ROOT / "images/mesa-triangle.ccl")
        draw, = [row for row in triangle if row[2] == "mesa-triangle"]
        self.assertEqual((draw[0], draw[5]), ("OPTICAL", "apps/mesa-triangle.app"))
        self.assertFalse(any(row[2] == "mesa-triangle" for row in device))
        self.assertFalse(any(row[2] == "mesa-logical-device" for row in triangle))
        self.assertTrue(any(row[2] == "init-mesa-triangle" for row in triangle))
        excluded = {"mesa-logical-device", "mesa-triangle",
                    "init-mesa-device", "init-mesa-triangle"}
        self.assertEqual([r for r in device if r[2] not in excluded],
                         [r for r in triangle if r[2] not in excluded])

    def test_mesa_window_profile_is_opt_in(self):
        catalog = ROOT / "images/artifacts.ccl"
        _, offscreen, _ = realizer.compile_plan(catalog, ROOT / "images/mesa-triangle.ccl")
        _, window, _ = realizer.compile_plan(catalog, ROOT / "images/mesa-triangle-window.ccl")
        draw, = [row for row in window if row[2] == "mesa-triangle-window"]
        self.assertEqual((draw[0], draw[5]), ("OPTICAL", "apps/mesa-triangle-window.app"))
        self.assertFalse(any(row[2] == "mesa-triangle-window" for row in offscreen))
        self.assertFalse(any(row[2] == "mesa-triangle" for row in window))
        self.assertTrue(any(row[2] == "init-mesa-triangle-window" for row in window))
        excluded = {"mesa-triangle", "mesa-triangle-window",
                    "init-mesa-triangle", "init-mesa-triangle-window"}
        self.assertEqual([r for r in offscreen if r[2] not in excluded],
                         [r for r in window if r[2] not in excluded])

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
          (artifact "tree" supplied-tree "tree")
          (artifact "package" repository-tree "package"))'''
        profile = PROFILE.replace("bootstrap-only", "optical")[:-1] + '''
          (kernel "kernel") (boot-menu "menu") (startup "start")
          (file optical "a" "boot/initrd.img"))'''
        self.reject(profile, catalog, "INVALID_PLACEMENT")
        self.reject(PROFILE[:-1] + '(file bootstrap "tree" "tree"))', catalog, "INVALID_PLACEMENT")
        # A repository tree (an asset package the build wrote) is optical too.
        self.reject(PROFILE[:-1] + '(file bootstrap "package" "package"))', catalog, "INVALID_PLACEMENT")

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
            f'(artifact "a{i}" repository "a{i}")' for i in range(129)) + ')'
        self.reject(catalog=catalog, diagnostic="TOO_MANY_ITEMS")
        for end in range(0, len(PROFILE), 3):
            self.reject(PROFILE[:end])
        rng = random.Random(2718)
        for _ in range(50):
            result = self.compile("".join(rng.choice('()#"abc 12\n') for _ in range(rng.randrange(100))))
            self.assertEqual(result.returncode, 1)

    def test_exact_catalog_capacity(self):
        catalog = '(image-artifacts v1 (catalog "test-v1") ' + " ".join(
            f'(artifact "a{i}" repository "a{i}")' for i in range(128)) + (
                ' (bootstrap-requires "resident" "a0" "a0.app"))')
        profile = ('(system-image v1 (catalog "test-v1") '
                   '(layout bootstrap-only) (provider "resident") (settings "a127") '
                   + BUILDER + ' (file bootstrap "a0" "a0.app"))')
        result = self.compile(profile, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("\ta0.app\n", result.stdout)

    def test_symbolic_format_versions(self):
        for token in ("1", "2", "v2", "V1", '"v1"', "(+ 0 1)"):
            with self.subTest(token=token):
                self.reject(PROFILE.replace("system-image v1", "system-image " + token))
                self.reject(catalog=CATALOG.replace("image-artifacts v1", "image-artifacts " + token))

    def test_trust_list_is_typed_and_required(self):
        """SIGN-001: an image names its builders; no test anchor; identities
        are reverse-DNS prefixes or "*"; each builder's CA comes from an
        explicit import."""
        self.reject(PROFILE.replace(BUILDER, ""), diagnostic="MISSING_BUILDER")
        self.reject(PROFILE.replace("host-development", "test"), diagnostic="TEST_BUILDER")
        self.reject(PROFILE.replace("host-development", "potato"), diagnostic="INVALID_VALUE")
        self.reject(PROFILE.replace("hardware-and-system", "everything"), diagnostic="INVALID_VALUE")
        for identity in ("", "Com.Example", "com..example", ".com", "com.", "com/example", "**", "a" * 65):
            with self.subTest(identity=identity):
                self.reject(PROFILE.replace('(identities "*")', f'(identities {json.dumps(identity)})'))
        self.reject(PROFILE.replace('(identities "*")', "(identities)"), diagnostic="MISSING_FIELD")
        self.reject(PROFILE.replace(BUILDER, BUILDER + " " + BUILDER), diagnostic="DUPLICATE_NAME")
        result = self.compile(PROFILE.replace('(identities "*")', '(identities "com.example" "org.other-app")')
                              .replace("hardware-and-system", "applications-only"))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("BUILDER\tHOST_DEVELOPMENT\ttest-ca\tAPPLICATIONS_ONLY\tcom.example,org.other-app\n",
                      result.stdout)
        self.profile.write_text(PROFILE)
        with self.assertRaisesRegex(ValueError, "needs --import test-ca="):
            self.prepare(imports={})
        with self.assertRaisesRegex(ValueError, "unused imports"):
            self.prepare(imports={**self.imports, "other": self.root / "test-ca.der"})
        _, report = self.prepare()
        self.assertEqual(report["builders"][0]["import"], "test-ca")
        self.assertEqual(report["builders"][0]["identities"], ["*"])
        for image in sorted((ROOT / "images").glob("*.ccl")):
            if image.name == "artifacts.ccl":
                continue
            with self.subTest(image=image.name):
                _, _, builders, _ = realizer.compile_plan_with_builders(ROOT / "images/artifacts.ccl", image)
                self.assertEqual(builders, [["HOST_DEVELOPMENT", "cubit-signing-ca", "HARDWARE_AND_SYSTEM", "*"]])

    @unittest.skipUnless(SIGN_TOOL.exists(), "ccl-sign is not built (make -C kernel ccl-sign)")
    def test_gate_refuses_unsigned_and_untrusted_executables(self):
        """SIGN-001: ccl-image stages no executable that does not verify
        against the image's own trust list."""
        def init(name):
            subprocess.run([SIGN_TOOL, "init", "--signing", self.root / name, "--name", name],
                           check=True, capture_output=True)
        init("keys")
        init("rogue")
        imports = {"test-ca": self.root / "keys/builder-ca.der"}
        elf = bytearray(b"\x7fELF\x02\x01\x01" + bytes(9) + (2).to_bytes(2, "little") + bytes(46))
        (self.root / "a.app").write_bytes(bytes(elf))
        with self.assertRaisesRegex(ValueError, "do not verify.*bootstrap/a.app.*NOT_SIGNED"):
            self.prepare(imports=imports)
        unsigned = self.root / "unsigned.app"
        unsigned.write_bytes(bytes(elf))
        subprocess.run([SIGN_TOOL, "sign", "--signing", self.root / "rogue", unsigned, self.root / "a.app"],
                       check=True, capture_output=True)
        with self.assertRaisesRegex(ValueError, "UNTRUSTED_BUILDER"):
            self.prepare(imports=imports)
        subprocess.run([SIGN_TOOL, "sign", "--signing", self.root / "keys", unsigned, self.root / "a.app"],
                       check=True, capture_output=True)
        files, _ = self.prepare(imports=imports)
        self.assertTrue(any(f.destination == "a.app" for f in files))
        signed = (self.root / "a.app").read_bytes()
        flipped = bytearray(signed)
        flipped[20] ^= 1
        (self.root / "a.app").write_bytes(bytes(flipped))
        with self.assertRaisesRegex(ValueError, "DIGEST_MISMATCH"):
            self.prepare(imports=imports)
        # Staging keeps a signed executable's bytes: stripping would drop the envelope.
        sys.path.insert(0, str(ROOT / "tools"))
        import image_debug
        splitter = image_debug.Splitter(self.root / "debug", "test")
        self.assertIs(splitter.split("a.app", signed), signed)

    @unittest.skipUnless(shutil.which("cc") and shutil.which("objcopy"), "needs cc and objcopy (nix develop)")
    def test_split_twice_keeps_the_debug_file(self):
        """A program with an allocated .debug_* section (rustc's
        .debug_gdb_scripts) splits once: its stripped copy has nothing to
        separate, so a second split (the sign step after an image recipe,
        or two builds) keeps the debug file the first one wrote."""
        sys.path.insert(0, str(ROOT / "tools"))
        import image_debug
        source = self.root / "gdb-scripts.c"
        source.write_text('__attribute__((used, section(".debug_gdb_scripts"))) '
                          'static const char scripts[] = "\\1gdb";\n'
                          'void _start(void) { for (;;) { } }\n')
        program = self.root / "gdb-scripts.app"
        subprocess.run(["cc", "-g", "-O0", "-static", "-nostdlib", "-no-pie", "-Wl,--build-id",
                        source, "-o", program], check=True, capture_output=True)
        data = program.read_bytes()
        self.assertGreater(image_debug.debug_bytes(data), 0)
        splitter = image_debug.Splitter(self.root / "debug", "test")
        stripped = splitter.split("gdb-scripts.app", data)
        debug_file = splitter.debug_path(image_debug.build_id(data))
        separated = debug_file.read_bytes()
        self.assertIn(b".debug_info", separated)
        self.assertIn(b".debug_gdb_scripts", stripped)
        self.assertEqual(image_debug.debug_bytes(stripped), 0)
        self.assertIs(splitter.split("gdb-scripts.app", stripped), stripped)
        self.assertEqual(debug_file.read_bytes(), separated)

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

    def test_staging_removed_after_success_kept_after_failure(self):
        # Each retained staging directory was a full image copy (2+ GB); 826
        # of them filled the build host's disk (2026-11-09).
        files, report = self.prepare()
        output = self.root / "initrd.img"
        staging = self.root / "staging"
        staging.mkdir()
        with patch.object(realizer.tempfile, "tempdir", str(staging)):
            realizer.realize(files, report, output)
            self.assertEqual(list(staging.iterdir()), [])
            realizer.realize(files, report, output, keep_staging=True)
            kept = list(staging.iterdir())
            self.assertEqual(len(kept), 1)
            self.assertTrue((kept[0] / "plan.json").is_file())
            with patch.object(realizer.subprocess, "run",
                              side_effect=subprocess.CalledProcessError(1, "cpio")):
                with self.assertRaises(subprocess.CalledProcessError):
                    realizer.realize(files, report, output)
            self.assertEqual(len(list(staging.iterdir())), 2)

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

    def test_repository_tree_places_whatever_the_build_wrote(self):
        inputs = self.optical_fixture()
        package = self.root / "build" / "package"
        package.mkdir(parents=True)
        (package / "catalog.ccl").write_text("first\n")
        (package / "one.qoi").write_bytes(b"1")
        self.catalog.write_text(self.catalog.read_text()[:-1] +
                                '(artifact "package" repository-tree "build/package"))')
        self.profile.write_text(self.profile.read_text()[:-1] +
                                '(file optical "package" "apps/Assets/package/2"))')
        files, _ = self.prepare(inputs)
        placed = {f.destination: f.data for f in files if f.artifact == "package"}
        self.assertEqual(placed, {"apps/Assets/package/2/catalog.ccl": b"first\n",
                                  "apps/Assets/package/2/one.qoi": b"1"})
        # A second picture in the package needs no image declaration change.
        (package / "two.qoi").write_bytes(b"2")
        files, _ = self.prepare(inputs)
        self.assertEqual(len([f for f in files if f.artifact == "package"]), 3)
        # Not a supplied input: binding one is an unused input.
        with self.assertRaisesRegex(ValueError, "unused supplied inputs"):
            self.prepare({**inputs, "package": package})
        for child in package.iterdir():
            child.unlink()
        with self.assertRaisesRegex(ValueError, "empty tree input"):
            self.prepare(inputs)

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
