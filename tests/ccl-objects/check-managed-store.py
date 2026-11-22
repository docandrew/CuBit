"""Independent read-only oracle after two fresh hosted Config service lifetimes."""
import pathlib
import sqlite3
import sys

path = pathlib.Path(sys.argv[1]).resolve()
key = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
with sqlite3.connect(path.as_uri() + "?mode=ro", uri=True) as db:
    assert db.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert db.execute("SELECT version FROM config_format").fetchall() == [(4,)]
    assert db.execute("SELECT * FROM object_types").fetchall() == [
        ("org.cubit.managed", "machine", key, b"\x84\x01\x58\x20" + key + b"\x01\x80", 2)
    ]
    for table in ("collections", "revisions"):
        assert db.execute(f"SELECT count(*) FROM {table}").fetchone() == (0,)
print("Independent SQLite: managed class preserved, no unauthorized values/revisions PASS")
