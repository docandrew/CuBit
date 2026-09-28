"""Independent SQLite read after Turso checkpoint/close; hosted evidence only."""
import pathlib
import sqlite3
import sys

database, fixture, output = map(pathlib.Path, sys.argv[1:])
expected = bytes.fromhex(fixture.read_text())
with sqlite3.connect(database.resolve().as_uri() + "?mode=ro", uri=True) as connection:
    assert connection.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert connection.execute("SELECT * FROM collections").fetchall() == [("org.cubit.ccl", "test", 1)]
    rows = connection.execute("SELECT schema_version, payload_schema, payload FROM revisions").fetchall()
    assert rows == [(1, bytes(range(32)), expected)]
    with output.open("x") as stream:
        stream.write(rows[0][2].hex() + "\n")
print("Independent SQLite -> CCL payload extraction: PASS")
