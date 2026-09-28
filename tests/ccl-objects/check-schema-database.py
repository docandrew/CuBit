"""Independent SQLite schema/value check, then extraction for the Ada decoder."""
import pathlib
import sqlite3
import sys

database, fixture, output = map(pathlib.Path, sys.argv[1:])
schema = pathlib.Path(str(fixture) + ".schema").read_bytes()
value = pathlib.Path(str(fixture) + ".value").read_bytes()
key = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
with sqlite3.connect(database.resolve().as_uri() + "?mode=ro", uri=True) as connection:
    assert connection.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert connection.execute("SELECT * FROM config_format").fetchall() == [(4,)]
    assert connection.execute("SELECT * FROM object_types").fetchall() == [("org.cubit.schema", "machine", key, schema, 1)]
    assert connection.execute("SELECT * FROM collections").fetchall() == [("org.cubit.schema", "machine", 1)]
    rows = connection.execute("SELECT namespace,profile,revision,schema_version,payload_schema,payload FROM revisions").fetchall()
    assert rows == [("org.cubit.schema", "machine", 1, 1, key, value)]
    for suffix, data in (("schema", schema), ("value", rows[0][-1])):
        with pathlib.Path(str(output) + "." + suffix).open("xb") as stream:
            stream.write(data)
print("Independent SQLite: exact persisted type and value, one revision PASS")
