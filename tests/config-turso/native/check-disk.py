#!/usr/bin/env python3
"""Independent Linux check of a CLOSED native-test disk; never modifies it."""
import argparse
import pathlib
import shutil
import sqlite3
import subprocess
import tempfile


def check_tables(db, revision):
    assert db.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert db.execute("SELECT version FROM config_format").fetchall() == [(4,)]
    schema = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
    assert db.execute("SELECT * FROM object_types").fetchall() == [
        ("org.cubit.publication", "test", schema, b"\x84\x01\x58\x20" + schema + b"\x01\x80", 1)
    ]
    assert db.execute("SELECT * FROM collections ORDER BY namespace,profile").fetchall() == [
        ("com.cubit.desktop", "laptop", revision),
        ("org.cubit.publication", "test", revision),
    ]
    rows = db.execute(
        "SELECT namespace,profile,revision,schema_version,payload_schema,payload FROM revisions WHERE namespace='com.cubit.desktop' ORDER BY revision"
    ).fetchall()
    assert len(rows) == revision
    golden = pathlib.Path(__file__).resolve().parent.parent / "fixtures/scalar-profile.hex"
    envelope = bytes.fromhex(golden.read_text().strip())[:36]
    for expected, row in enumerate(rows, 1):
        namespace, profile, saved, version, saved_key, payload = row
        assert (namespace, profile, saved, version) == ("com.cubit.desktop", "laptop", expected, 1)
        assert saved_key == envelope[4:36]
        scale = 125 if expected == 1 else 150
        expected_map = (bytes.fromhex("a3657363616c6518") + bytes([scale]) +
                        bytes.fromhex("657468656d6565416c6c6f7967656e61626c6564f5"))
        assert payload == envelope + expected_map
        print(f"Verified revision {saved}: theme='Alloy', scale={scale}, enabled=true")
    typed = db.execute("SELECT profile,revision,schema_version,payload_schema,payload FROM revisions WHERE namespace='org.cubit.publication' ORDER BY revision").fetchall()
    assert typed == [
        ("test", n, 1, schema, b"\x84\x01\x58\x20" + schema + b"\x81\x82\x18" + bytes([40 + n]) + b"\x00\x40")
        for n in range(1, revision + 1)
    ]
    assert db.execute("SELECT COUNT(*) FROM revisions").fetchone() == (2 * revision,)
    print(f"Verified native Ada worker history: {revision} revision(s), exact CCL CBOR")
    print("SQLite tables: config_format, collections, revisions, object_types")


def check(image, revision=1, export_dir=None):
    assert revision in (1, 2)
    subprocess.run(["e2fsck", "-fn", str(image)], check=True)
    with tempfile.TemporaryDirectory(prefix="cubit-turso-check-") as directory:
        database = pathlib.Path(directory) / "profile.sqlite"
        # debugfs can exit zero on failure; validate the output, not just status.
        subprocess.run(
            ["debugfs", "-R", f"dump /turso-native/profile.sqlite {database}", str(image)],
            check=True,
        )
        assert database.is_file() and database.stat().st_size > 0
        with sqlite3.connect(f"file:{database}?mode=ro&immutable=1", uri=True) as db:
            check_tables(db, revision)
        if export_dir is not None:
            # Exclusive directory creation; never replace the user's files.
            export_dir.mkdir()
            shutil.copyfile(database, export_dir / "profile.sqlite")
            shutil.copyfile(image, export_dir / "disk.img")
            print(f"Validated disk and SQLite exported to: {export_dir}")
    print("TURSO-NATIVE: Linux SQLite integrity/typed payload/ext2 PASS")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("image", type=pathlib.Path)
    parser.add_argument("--revision", type=int, choices=(1, 2), default=1)
    parser.add_argument("--export-dir", default="")
    args = parser.parse_args()
    check(args.image.resolve(strict=True), args.revision,
          pathlib.Path(args.export_dir).absolute() if args.export_dir else None)
