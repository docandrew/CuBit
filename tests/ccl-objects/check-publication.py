"""Independent, read-only check of the hosted lost-ack recovery database."""
import pathlib
import sqlite3
import sys

database = pathlib.Path(sys.argv[1]).resolve()
context = sys.argv[2] if len(sys.argv) > 2 else "test"
schema = b"".join(word.to_bytes(8, "big") for word in (1, 2, 3, 4))
def payload(value):
    # Documented CCL cell envelope for an integer, no text. Independent of Ada
    # and Rust encoders; 41/42 have the shortest two-byte unsigned encoding.
    return b"\x84\x01\x58\x20" + schema + b"\x81\x82\x18" + bytes([value]) + b"\x00\x40"

with sqlite3.connect(database.as_uri() + "?mode=ro", uri=True) as connection:
    assert connection.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert connection.execute("SELECT version FROM config_format").fetchall() == [(4,)]
    expected_types = [
        ("org.cubit.publication", context, schema,
         b"\x84\x01\x58\x20" + schema + b"\x01\x80", 1)
    ]
    assert connection.execute("SELECT * FROM object_types").fetchall() == expected_types
    assert connection.execute("SELECT * FROM collections").fetchall() == [("org.cubit.publication", context, 2)]
    assert connection.execute("SELECT revision, payload_schema, payload FROM revisions ORDER BY revision").fetchall() == [
        (1, schema, payload(41)), (2, schema, payload(42)),
    ]
print("Independent SQLite: exactly two committed revisions, no lost-ack retry PASS")
