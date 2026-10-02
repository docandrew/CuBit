import importlib.util
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("inventory", ROOT / "userspace/mesa/link-inventory.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
HEADER = "Archive member included to satisfy reference by file (symbol)\n\n"


class InventoryTests(unittest.TestCase):
    def test_rejects_unknown_empty_and_truncated_maps(self):
        for text in ("", "wrong header", HEADER, HEADER + "Merging object attributes",
                     HEADER + "relative.a(x.o)\nDiscarded input sections"):
            with self.subTest(text=text), self.assertRaises(ValueError):
                module.extracted_members(text)

    def test_classification_and_unmapped_preserved(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source, build = root / "source", root / "build"
            source.mkdir()
            build.mkdir()
            (source / "original.c").write_text("/* original notice */")
            (build / "generated.c").write_text("/* generated notice */")
            commands = [{"directory": str(build), "output": "a.o", "file": "../source/original.c"},
                        {"directory": str(build), "output": "b.o", "file": "generated.c"}]
            text = HEADER + f"{build}/a.o\n  referring.o (symbol)\n{build}/b.o\n/runtime/libc.a(x.o)\nDiscarded input sections\n/ignored.o"
            result = module.inventory(text, commands, source, build)
            self.assertEqual(result["counts"], {"upstream-source": 1, "generated-source": 1,
                                              "unmapped-runtime-or-other": 1})
            self.assertEqual(len(result["members"]), 3)
            self.assertEqual(sum("sha256" in r for r in result["members"]), 2)
            with self.assertRaises(ValueError):
                module.inventory(text, commands[:1], source, build)
            with self.assertRaises(ValueError):
                module.inventory(text, commands + [{**commands[0], "file": "generated.c"}], source, build)
            (source / "original.c").unlink()
            with self.assertRaises(FileNotFoundError):
                module.inventory(text, commands, source, build)


if __name__ == "__main__":
    unittest.main()
