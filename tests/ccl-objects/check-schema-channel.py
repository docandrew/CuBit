"""Independent oracle: four declarations with immutable class, no initial values."""
import pathlib
import sqlite3
import sys

path = pathlib.Path(sys.argv[1]).resolve()
def row(name, words, root, declarations=b"\x80", management=1):
    key = b"".join(word.to_bytes(8, "big") for word in words)
    return name, "machine", key, b"\x84\x01\x58\x20" + key + bytes([root]) + declarations, management

with sqlite3.connect(path.as_uri() + "?mode=ro", uri=True) as db:
    assert db.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
    assert db.execute("SELECT version FROM config_format").fetchall() == [(4,)]
    assert db.execute("SELECT * FROM object_types ORDER BY namespace").fetchall() == [
        row("org.cubit.created", (1, 2, 3, 4), 1),
        row("org.cubit.lostack", (5, 6, 7, 8), 2),
        row("org.cubit.managed", (1, 2, 3, 4), 1, management=2),
        # The original A/B/Pair declaration must remain byte-for-byte intact;
        # retries supplied Noise/B/A/Pair with shifted references, never stored.
        row("org.cubit.nominal", (9, 10, 11, 12), 9,
            bytes.fromhex("838341410180834142018083445061697201828241610782416208")),
    ]
    for table in ("collections", "revisions"):
        assert db.execute(f"SELECT count(*) FROM {table}").fetchone() == (0,)
print("Independent SQLite: four original declarations with exact management classes, zero values/revisions PASS")
