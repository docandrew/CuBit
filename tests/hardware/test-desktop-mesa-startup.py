"""Host artifact guard regression; does not boot or execute a candidate."""
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "guard", Path(__file__).with_name("verify-desktop-mesa-startup.py"))
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)


class ArtifactTests(unittest.TestCase):
    def test_accept_and_reject(self):
        data = b"\x7fELFtest fixture, never executable"
        record = dict(status="LINKED", admitted_device_startup_enabled=True,
                      admitted_startup=True, optional_render_probe=True,
                      gpu_drawing_enabled=False, binary_bytes=len(data),
                      binary_sha256=hashlib.sha256(data).hexdigest())
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            binary = root / "desktop-vulkan-link.svc"
            metadata = root / "result.json"
            binary.write_bytes(data)
            metadata.write_text(json.dumps(record))
            self.assertEqual(guard.verify(root), binary)
            for field, value in dict(status="INCOMPLETE",
                                     admitted_device_startup_enabled=False,
                                     admitted_startup=False,
                                     optional_render_probe=False,
                                     gpu_drawing_enabled=True,
                                     binary_bytes=0, binary_sha256="bad").items():
                with self.subTest(field=field):
                    metadata.write_text(json.dumps({**record, field: value}))
                    with self.assertRaises(ValueError):
                        guard.verify(root)
            metadata.write_text(json.dumps(record))
            binary.write_bytes(data + b"changed")
            with self.assertRaises(ValueError):
                guard.verify(root)
            binary.write_bytes(b"not ELF")
            with self.assertRaises(ValueError):
                guard.verify(root)


if __name__ == "__main__":
    unittest.main()
