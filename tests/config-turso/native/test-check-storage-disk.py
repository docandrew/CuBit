"""Check the oracle itself against independent SQLite fixtures, not CuBit I/O."""
import importlib.util
import pathlib
import sqlite3
import unittest

spec = importlib.util.spec_from_file_location("storage_check", pathlib.Path(__file__).with_name("check-storage-disk.py"))
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


class StorageOracleTests(unittest.TestCase):
    def fixture(self, populated=False):
        db = sqlite3.connect(":memory:")
        self.addCleanup(db.close)
        db.executescript("""
            CREATE TABLE config_format(version); INSERT INTO config_format VALUES(4);
            CREATE TABLE object_types(namespace,profile,schema_key,declaration,management);
            CREATE TABLE collections(namespace,profile,revision);
            CREATE TABLE revisions(namespace,profile,revision,schema_version,payload_schema,payload);
        """)
        if populated:
            key = bytes.fromhex("0000000000000001000000000000000200000000000000030000000000000004")
            envelope = bytes.fromhex("84015820") + key
            db.execute("INSERT INTO object_types VALUES(?,?,?,?,1)",
                       ("org.cubit.publication", "machine", key, envelope + bytes.fromhex("0180")))
            db.execute("INSERT INTO collections VALUES('org.cubit.publication','machine',2)")
            for revision, ending in [(1, "818218290040"), (2, "8182182a0040")]:
                db.execute("INSERT INTO revisions VALUES('org.cubit.publication','machine',?,1,?,?)",
                           (revision, key, envelope + bytes.fromhex(ending)))
            for fixture in (checker.nested_rows, checker.discovered_rows):
                definition, collection, history = fixture()
                db.execute("INSERT INTO object_types VALUES(?,?,?,?,?)", definition)
                db.execute("INSERT INTO collections VALUES(?,?,?)", collection)
                db.executemany("INSERT INTO revisions VALUES(?,?,?,?,?,?)", history)
        return db

    def test_empty(self):
        checker.check_tables(self.fixture())

    def test_exact_native_history(self):
        checker.check_tables(self.fixture(True), True)

    def test_wrong_mode(self):
        with self.assertRaises(AssertionError):
            checker.check_tables(self.fixture(True))
        with self.assertRaises(AssertionError):
            checker.check_tables(self.fixture(), True)

    def test_reject_corruption(self):
        changes = [
            "UPDATE object_types SET management=2",
            "UPDATE object_types SET management=NULL",
            "UPDATE config_format SET version=3",
            "UPDATE config_format SET version=2",
            "DELETE FROM object_types",
            "UPDATE object_types SET profile='test'",
            "UPDATE object_types SET declaration=x'00'",
            "UPDATE object_types SET schema_key=x'00'",
            "UPDATE collections SET revision=1",
            "DELETE FROM revisions WHERE revision=1",
            "UPDATE revisions SET payload=x'00' WHERE revision=2",
            "UPDATE revisions SET schema_version=2",
            "INSERT INTO revisions SELECT * FROM revisions WHERE revision=2",
            "CREATE TABLE unexpected(secret)",
            "DELETE FROM object_types WHERE namespace LIKE '%.preferences'",
            "UPDATE object_types SET declaration=x'00' WHERE namespace LIKE '%.preferences'",
            "DELETE FROM revisions WHERE namespace LIKE '%.preferences' AND revision=1",
            "UPDATE revisions SET payload=zeroblob(length(payload)) WHERE namespace LIKE '%.preferences' AND revision=2",
            "UPDATE collections SET revision=1 WHERE namespace LIKE '%.preferences'",
            "DELETE FROM object_types WHERE namespace LIKE '%.readings'",
            "UPDATE object_types SET declaration=x'00' WHERE namespace LIKE '%.readings'",
            "DELETE FROM revisions WHERE namespace LIKE '%.readings' AND revision=1",
            "UPDATE revisions SET payload=x'00' WHERE namespace LIKE '%.readings' AND revision=2",
        ]
        for sql in changes:
            with self.subTest(sql=sql):
                db = self.fixture(True)
                db.execute(sql)
                with self.assertRaises(AssertionError):
                    checker.check_tables(db, True)

    def test_golden_encoder_boundaries(self):
        for value, expected in [(23, "17"), (24, "1818"), (255, "18ff"),
                                (256, "190100"), (65536, "1a00010000"),
                                (2**63, "1b8000000000000000"),
                                (2**64 - 1, "1bffffffffffffffff")]:
            self.assertEqual(checker.cbor(value), bytes.fromhex(expected))
        self.assertEqual(checker.cbor([1, b"A"]), bytes.fromhex("82014141"))
        self.assertEqual(checker.cbor(bytes(8192))[:3], bytes.fromhex("592000"))
        for value in [-1, 2**64]:
            with self.assertRaises(AssertionError): checker.cbor(value)

    def test_benchmark_history(self):
        db = self.fixture(True)
        db.execute("DELETE FROM object_types WHERE namespace != 'org.cubit.publication'")
        db.execute("DELETE FROM collections WHERE namespace != 'org.cubit.publication'")
        db.execute("UPDATE collections SET revision=129")
        db.execute("DELETE FROM revisions")
        key = bytes.fromhex("0000000000000001000000000000000200000000000000030000000000000004")
        for revision in range(1, 130):
            # Independent fixture encoding spans CBOR's 23/24 boundary.
            integer = bytes([revision]) if revision < 24 else b"\x18" + bytes([revision])
            payload = b"\x84\x01\x58\x20" + key + b"\x81\x82" + integer + b"\x00\x40"
            db.execute("INSERT INTO revisions VALUES('org.cubit.publication','machine',?,1,?,?)",
                       (revision, key, payload))
        checker.check_tables(db, True, True)
        with self.assertRaises(AssertionError):
            checker.check_tables(db, True)
        for sql in ["DELETE FROM revisions WHERE revision=64",
                    "UPDATE revisions SET payload=x'00' WHERE revision=24",
                    "UPDATE collections SET revision=128",
                    "INSERT INTO revisions SELECT * FROM revisions WHERE revision=129"]:
            with self.subTest(sql=sql):
                db.execute("SAVEPOINT corruption")
                db.execute(sql)
                with self.assertRaises(AssertionError):
                    checker.check_tables(db, True, True)
                db.execute("ROLLBACK TO corruption")
                db.execute("RELEASE corruption")


if __name__ == "__main__":
    unittest.main()
