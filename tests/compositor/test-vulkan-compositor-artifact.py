import hashlib
import json
from pathlib import Path
import tempfile
import unittest
import importlib.util

module_path = Path(__file__).resolve().parents[2] / "tools/verify_desktop_vulkan_compositor.py"
spec = importlib.util.spec_from_file_location("compositor_guard", module_path)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
verify = module.verify


class GuardTests(unittest.TestCase):
    def test_identity(self):
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            data = b"\x7fELFfixture only"
            (root / "desktop.svc").write_bytes(data)
            (root / "source.adb").write_bytes(b"source")
            manifest = json.dumps({"source.adb": hashlib.sha256(b"source").hexdigest()}).encode()
            (root / "sources.json").write_bytes(manifest)
            record = dict(status="LINKED", backend="vulkan-runtime-dispatch",
                          gpu_drawing_enabled=True, binary="desktop.svc",
                          binary_bytes=len(data), binary_sha256=hashlib.sha256(data).hexdigest(),
                          source_manifest="sources.json", source_manifest_sha256=hashlib.sha256(manifest).hexdigest())
            def save(value):
                (root / "compositor-result.json").write_text(json.dumps(value))
            save(record)
            self.assertEqual(verify(root), root / "desktop.svc")
            for key, value in [("gpu_drawing_enabled", False), ("gpu_drawing_enabled", 1),
                               ("backend", "startup-only"), ("status", "BUILDING"),
                               ("binary_bytes", 0), ("binary_sha256", "bad"),
                               ("source_manifest_sha256", "bad"),
                               ("binary", "../desktop.svc"), ("binary", "/tmp/desktop.svc")]:
                with self.subTest(key=key, value=value):
                    save(dict(record, **{key: value}))
                    with self.assertRaises(ValueError): verify(root)
            save(record)
            (root / "source.adb").write_bytes(b"changed")
            with self.assertRaises(ValueError): verify(root)
            (root / "source.adb").unlink()
            (root / "source.adb").symlink_to(root / "desktop.svc")
            with self.assertRaises(ValueError): verify(root)


if __name__ == "__main__":
    unittest.main()
