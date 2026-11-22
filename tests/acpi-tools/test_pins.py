"""Behavioral controls for the pin editor, using only temporary input trees."""

import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).with_name("pins.py")


def sha(data):
    return hashlib.sha256(data).hexdigest()


class PinsTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.catalog = "tests/acpi-example/sources.json"
        self.put("shared/example.ads", b"source\n")

    def put(self, name, data):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
        return path

    def doc(self, value, name=None):
        return self.put(name or self.catalog, (json.dumps(value) + "\n").encode())

    def run_pins(self, *arguments, status=0):
        result = subprocess.run([sys.executable, "-I", str(SCRIPT), "--root", str(self.root),
                                 *arguments], capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, status, result.stdout + result.stderr)
        return result

    def test_rows_maps_closures_and_paired_engines(self):
        self.put("tests/acpi-example/engine.mjs", b"engine")
        doc = {"inputs": [{"path": "shared/example.ads", "sha256": "0" * 64, "kind": "production"}],
               "closure": {"main": {"sources": {"shared/example.ads": "0" * 64}}},
               "engine": "tests/acpi-example/engine.mjs", "engine_sha256": "0" * 64,
               "expected": {"transcript_sha256": "a" * 64}, "checks": 17}
        path = self.doc(doc)
        before = path.read_bytes()
        result = self.run_pins("--check", self.catalog, status=1)
        self.assertEqual(json.loads(result.stdout)["pins"], 3)
        self.assertEqual(before, path.read_bytes())
        self.run_pins("--write", self.catalog)
        actual = json.loads(path.read_bytes())
        self.assertEqual(actual["inputs"][0]["sha256"], sha(b"source\n"))
        self.assertEqual(actual["checks"], 17)
        self.assertEqual(actual["expected"], doc["expected"])
        self.assertEqual(actual["engine_sha256"], sha(b"engine"))
        self.run_pins("--check", self.catalog)

    def test_noop_is_byte_identical_and_preserves_mode(self):
        path = self.doc({"shared/example.ads": sha(b"source\n")})
        path.chmod(0o640)
        before = path.read_bytes()
        self.run_pins("--write", self.catalog)
        self.assertEqual(before, path.read_bytes())
        self.assertEqual(path.stat().st_mode & 0o777, 0o640)
        self.put("shared/example.ads", b"changed")
        self.run_pins("--write", self.catalog)
        self.assertEqual(path.stat().st_mode & 0o777, 0o640)

    def test_missing_file_prevents_all_writes(self):
        one = self.doc({"shared/example.ads": "0" * 64})
        second = "tests/acpi-example/closure.json"
        two = self.doc({"shared/missing.ads": "0" * 64}, second)
        before = (one.read_bytes(), two.read_bytes())
        self.run_pins("--write", self.catalog, second, status=2)
        self.assertEqual(before, (one.read_bytes(), two.read_bytes()))

    def test_symlink_file_and_parent_are_rejected(self):
        for relative, target in [("shared/link.ads", "example.ads"), ("linked", "shared")]:
            with self.subTest(relative=relative):
                (self.root / relative).symlink_to(target)
                key = relative if relative.endswith(".ads") else relative + "/example.ads"
                path = self.doc({key: "0" * 64})
                before = path.read_bytes()
                self.run_pins("--write", self.catalog, status=2)
                self.assertEqual(before, path.read_bytes())

    def test_traversal_and_absolute_external_inputs(self):
        self.doc({"../outside.ads": "0" * 64})
        self.run_pins("--check", self.catalog, status=2)
        with tempfile.NamedTemporaryFile() as external:
            external.write(b"tool")
            external.flush()
            self.doc({"tools": {"compiler": {"path": external.name, "sha256": "0" * 64}}})
            self.run_pins("--check", self.catalog, status=2)
            self.run_pins("--write", "--allow-external", self.catalog)
            self.run_pins("--check", "--allow-external", self.catalog)

    def test_duplicate_json_keys_are_rejected(self):
        self.put(self.catalog, b'{"x":1,"x":2}')
        self.run_pins("--write", self.catalog, status=2)

    def test_explicit_add_and_remove(self):
        path = self.doc({"inputs": [{"path": "shared/deleted.ads", "sha256": "0" * 64}],
                         "sentinel": 41})
        arguments = ("--remove", "/inputs=shared/deleted.ads", "--add", "/inputs=shared/example.ads")
        before = path.read_bytes()
        self.run_pins("--check", *arguments, self.catalog, status=1)
        self.assertEqual(before, path.read_bytes())
        self.run_pins("--write", *arguments, self.catalog)
        actual = json.loads(path.read_bytes())
        self.assertEqual(actual, {"inputs": [{"path": "shared/example.ads", "sha256": sha(b"source\n")}],
                                  "sentinel": 41})
        self.run_pins("--write", "--add", "/inputs=shared/example.ads", self.catalog, status=2)

    def test_add_remove_hash_maps_and_escaped_pointer(self):
        path = self.doc({"input/map": {"shared/removed.ads": "0" * 64}})
        self.run_pins("--write", "--remove", "/input~1map=shared/removed.ads",
                      "--add", "/input~1map=shared/example.ads", self.catalog)
        self.assertEqual(json.loads(path.read_bytes())["input/map"], {"shared/example.ads": sha(b"source\n")})

    def test_manifest_dependencies_use_new_bytes_in_any_argument_order(self):
        for order in [False, True]:
            with self.subTest(reverse=order):
                source_manifest = "tests/acpi-example/source-manifest.json"
                one = self.doc({"shared/example.ads": "0" * 64}, source_manifest)
                two = self.doc({"sources": {source_manifest: "0" * 64}})
                args = [source_manifest, self.catalog]
                self.run_pins("--write", *(reversed(args) if order else args))
                self.assertEqual(json.loads(two.read_bytes())["sources"][source_manifest], sha(one.read_bytes()))
                self.run_pins("--check", *args)

    def test_cycles_are_rejected_without_mutation(self):
        second = "tests/acpi-example/closure.json"
        one = self.doc({second: "0" * 64})
        two = self.doc({self.catalog: "0" * 64}, second)
        before = (one.read_bytes(), two.read_bytes())
        self.run_pins("--write", self.catalog, second, status=2)
        self.assertEqual(before, (one.read_bytes(), two.read_bytes()))

    def test_local_helpers_assets_and_named_fixtures(self):
        self.put("tests/acpi-example/helper.ads", b"helper")
        self.put("tests/acpi-example/fixtures/captured.dat", b"firmware")
        self.doc({"helpers": [{"path": "helper.ads", "sha256": "0" * 64}],
                  "assets": {"helper.ads": "0" * 64},
                  "fixtures": [{"name": "captured.dat", "sha256": "0" * 64},
                               {"path": "fixtures/captured.dat", "sha256": "0" * 64}]})
        result = self.run_pins("--write", self.catalog)
        self.assertEqual(json.loads(result.stdout)["pins"], 4)
        self.run_pins("--check", self.catalog)

    def test_explicit_subtree_base_and_repository_tools(self):
        self.put("elsewhere/file.ads", b"local")
        self.doc({"inputs": [{"path": "file.ads", "sha256": "0" * 64}],
                  "tools": {"build": {"repository_path": "shared/example.ads", "sha256": "0" * 64}}})
        self.run_pins("--write", "--base", "/inputs=elsewhere", self.catalog)
        self.run_pins("--check", "--base", "/inputs=elsewhere", self.catalog)

    def test_invalid_digest_and_empty_catalog_fail(self):
        for document in [{"inputs": [{"path": "shared/example.ads", "sha256": "bad"}]}, {"checks": 99}]:
            with self.subTest(document=document):
                self.doc(document)
                self.run_pins("--write", self.catalog, status=2)

    def test_invalid_edit_and_external_manifest_are_rejected(self):
        path = self.doc({"shared/example.ads": "0" * 64})
        before = path.read_bytes()
        for edit in ["not-an-assignment", "/bad~2pointer=shared/example.ads", "/inputs="]:
            with self.subTest(edit=edit):
                self.run_pins("--write", "--add", edit, self.catalog, status=2)
                self.assertEqual(before, path.read_bytes())
        with tempfile.NamedTemporaryFile() as external:
            external.write(b"not even a JSON manifest")
            external.flush()
            result = self.run_pins("--write", "--allow-external", external.name, status=2)
            self.assertIn("manifests must be inside", result.stderr)


if __name__ == "__main__":
    unittest.main()
