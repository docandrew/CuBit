"""Tests of the independent SQLite oracle, not simulated native persistence."""
import contextlib
import importlib.util
import io
import pathlib
import sqlite3
import unittest

spec = importlib.util.spec_from_file_location("disk_check", pathlib.Path(__file__).with_name("check-disk.py"))
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


class DiskOracleTests(unittest.TestCase):
    def fixture(self, count):
        db = sqlite3.connect(":memory:")
        self.addCleanup(db.close)
        db.executescript("""
            CREATE TABLE config_format(version); INSERT INTO config_format VALUES(4);
            CREATE TABLE object_types(namespace,profile,schema_key,declaration,management);
            CREATE TABLE collections(namespace,profile,revision);
            CREATE TABLE revisions(namespace,profile,revision,schema_version,payload_schema,payload);
        """)
        db.execute("INSERT INTO collections VALUES('com.cubit.desktop','laptop',?)", (count,))
        db.execute("INSERT INTO collections VALUES('org.cubit.publication','test',?)", (count,))
        schema = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
        db.execute("INSERT INTO object_types VALUES('org.cubit.publication','test',?,?,1)",
                   (schema, b"\x84\x01\x58\x20" + schema + b"\x01\x80"))
        golden = pathlib.Path(__file__).parent.parent / "fixtures/scalar-profile.hex"
        envelope = bytes.fromhex(golden.read_text().strip())[:36]
        for revision in range(1, count + 1):
            scale = bytes([0x18, 125 if revision == 1 else 150])
            payload = envelope + b'\xa3\x65scale' + scale + b'\x65theme\x65Alloy\x67enabled\xf5'
            db.execute("INSERT INTO revisions VALUES('com.cubit.desktop','laptop',?,1,?,?)",
                       (revision, envelope[4:], payload))
            typed = b"\x84\x01\x58\x20" + schema + b"\x81\x82\x18" + bytes([40 + revision]) + b"\x00\x40"
            db.execute("INSERT INTO revisions VALUES('org.cubit.publication','test',?,1,?,?)",
                       (revision, schema, typed))
        return db

    def test_accepts_both_complete_histories(self):
        for revision in (1, 2):
            with contextlib.redirect_stdout(io.StringIO()):
                checker.check_tables(self.fixture(revision), revision)

    def test_rejects_wrong_boot_phase(self):
        for actual, expected in ((1, 2), (2, 1)):
            with self.assertRaises(AssertionError):
                checker.check_tables(self.fixture(actual), expected)

    def test_rejects_corrupt_or_incomplete_history(self):
        for mutation in (
            "DELETE FROM revisions WHERE revision=1",
            "UPDATE revisions SET payload=X'00' WHERE revision=1",
            "UPDATE revisions SET payload_schema=X'00' WHERE revision=2",
            "UPDATE revisions SET schema_version=9 WHERE revision=2",
            "UPDATE collections SET revision=1",
            "UPDATE config_format SET version=99",
            "DELETE FROM revisions WHERE namespace='org.cubit.publication'",
            "UPDATE revisions SET payload=X'00' WHERE namespace='org.cubit.publication' AND revision=2",
            "DELETE FROM collections WHERE namespace='org.cubit.publication'",
            "DELETE FROM object_types",
            "UPDATE object_types SET declaration=X'00'",
            "UPDATE object_types SET schema_key=X'00'",
        ):
            with self.subTest(mutation=mutation), contextlib.redirect_stdout(io.StringIO()):
                db = self.fixture(2)
                db.execute(mutation)
                with self.assertRaises(AssertionError):
                    checker.check_tables(db, 2)


if __name__ == "__main__":
    unittest.main()
