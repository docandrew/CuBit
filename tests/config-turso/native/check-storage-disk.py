#!/usr/bin/env python3
"""Check an idle worker's disposable disk after QEMU has stopped; never mount it."""
import argparse
import pathlib
import shutil
import sqlite3
import subprocess
import tempfile


def unsigned_cbor(value):
    """Independent small positive-integer oracle, not the production codec."""
    assert 0 <= value <= 255
    return bytes([value]) if value < 24 else bytes([24, value])


def cbor(value):
    """Small independent golden-vector encoder, never used by the guest."""
    def head(major, length):
        assert 0 <= length < 2**64
        if length < 24:
            return bytes([major * 32 + length])
        for limit, width, tag in ((256, 1, 24), (65536, 2, 25), (2**32, 4, 26), (2**64, 8, 27)):
            if length < limit:
                return bytes([major * 32 + tag]) + length.to_bytes(width, "big")
        raise AssertionError(length)
    if isinstance(value, int):
        return head(0, value)
    if isinstance(value, bytes):
        return head(2, len(value)) + value
    assert isinstance(value, (list, tuple))
    return head(4, len(value)) + b"".join(cbor(item) for item in value)


def nested_rows():
    name, context = "org.cubit.publication.preferences", "machine"
    key = b"".join(word.to_bytes(8, "big") for word in (5, 6, 7, 8))
    declaration = cbor([1, key, 9, [
        [b"ActiveData", 1, [[b"enabled", 2], [b"note", 3]]],
        [b"Mode", 2, [[b"Inactive", 6], [b"Active", 7]]],
        [b"Preferences", 1, [[b"name", 3], [b"mode", 8], [b"score", 1]]],
    ]])
    text = bytes(32 + n % 95 for n in range(8192))
    first = cbor([1, key, [[3, 0], [0, 5], [2, 0], [2, 0], [1, 0], [5, 5000], [2**63, 0]],
                  b"Cubie" + text[:5000]])
    second = cbor([1, key, [[3, 0], [0, 8192], [1, 0], [0, 0], [2**63 - 1, 0]], text])
    return (name, context, key, declaration, 1), (name, context, 2), [
        (name, context, 1, 1, key, first), (name, context, 2, 1, key, second)]


def discovered_rows():
    name, context = "org.cubit.publication.readings", "machine"
    key = b"".join(word.to_bytes(8, "big") for word in (9, 10, 11, 12))
    declaration = cbor([1, key, 7, [
        [b"Reading", 2, [[b"Value", 1], [b"Unavailable", 6]]],
    ]])
    first = cbor([1, key, [[1, 0], [42, 0]], b""])
    second = cbor([1, key, [[2, 0], [0, 0]], b""])
    return (name, context, key, declaration, 1), (name, context, 2), [
        (name, context, 1, 1, key, first), (name, context, 2, 1, key, second)]


def check_tables(db, objects=False, benchmark=False):
    assert not benchmark or objects
    assert db.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    tables = {row[0] for row in db.execute("SELECT name FROM sqlite_schema WHERE type='table'")}
    assert tables == {"config_format", "collections", "revisions", "object_types"}, tables
    assert db.execute("SELECT version FROM config_format").fetchall() == [(4,)]
    if objects:
        key = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
        envelope = b"\x84\x01\x58\x20" + key
        definitions = [("org.cubit.publication", "machine", key, envelope + b"\x01\x80", 1)]
        revision = 129 if benchmark else 2
        collections = [("org.cubit.publication", "machine", revision)]
        revisions = [
            ("org.cubit.publication", "machine", n, 1, key,
             envelope + b"\x81\x82" + unsigned_cbor(n if benchmark else 40 + n) + b"\x00\x40")
            for n in range(1, revision + 1)]
        if not benchmark:
            for fixture in (nested_rows, discovered_rows):
                definition, collection, history = fixture()
                definitions.append(definition)
                collections.append(collection)
                revisions.extend(history)
        assert db.execute("SELECT * FROM object_types ORDER BY namespace,profile").fetchall() == definitions
        assert db.execute("SELECT * FROM collections ORDER BY namespace,profile").fetchall() == collections
        assert db.execute("SELECT namespace,profile,revision,schema_version,payload_schema,payload FROM revisions ORDER BY namespace,profile,revision").fetchall() == revisions
    else:
        assert db.execute("SELECT count(*) FROM object_types").fetchone() == (0,)
        assert db.execute("SELECT count(*) FROM collections").fetchone() == (0,)
        assert db.execute("SELECT count(*) FROM revisions").fetchone() == (0,)


def check(image, objects=False, export_dir=None, benchmark=False):
    # Quiescent initialization/publication, NOT arbitrary power-loss consistency.
    # The runner requires ready AND, for objects, the app's completion marker.
    subprocess.run(["e2fsck", "-fn", str(image)], check=True)
    with tempfile.TemporaryDirectory(prefix="cubit-config-storage-db.") as directory:
        database = pathlib.Path(directory) / "system-config.sqlite"
        for suffix in ("", "-wal"):
            output = pathlib.Path(str(database) + suffix)
            subprocess.run(["debugfs", "-R",
                            f"dump /system-config.sqlite{suffix} {output}", str(image)],
                           check=True, capture_output=True, text=True)
            # debugfs may report a missing inode while still exiting zero.
            assert output.is_file() and output.stat().st_size > 0, output
        # WAL-aware reader over extracted copies. immutable=1 would ignore WAL
        # and could silently inspect a stale main file. No live guest files here.
        with sqlite3.connect(f"file:{database}?mode=ro", uri=True) as db:
            check_tables(db, objects, benchmark)
        if export_dir is not None:
            # Never overwrite a prior test result or the user's source disk.
            export_dir.mkdir()
            shutil.copyfile(image, export_dir / "disk.img")
            shutil.copyfile(database, export_dir / database.name)
            shutil.copyfile(pathlib.Path(str(database) + "-wal"), export_dir / (database.name + "-wal"))
    print("CONFIG-STORAGE: independent SQLite format/WAL/ext2 check PASS")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("image", type=pathlib.Path)
    parser.add_argument("--objects", action="store_true")
    parser.add_argument("--benchmark", action="store_true")
    parser.add_argument("--export-dir", type=pathlib.Path)
    args = parser.parse_args()
    check(args.image.resolve(strict=True), args.objects, args.export_dir, args.benchmark)
