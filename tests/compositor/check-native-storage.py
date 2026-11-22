"""Check the registered CPU-target allocation ledger in a native test log.

Modes are the tightly packed virtual displays configured by the headless fixture.
This does not measure Mesa, fonts, client allocations, or process RSS.
"""
import argparse
from pathlib import Path
import re

parser = argparse.ArgumentParser()
parser.add_argument("serial", type=Path)
parser.add_argument("modes", nargs="+", help="fixture output modes, WIDTHxHEIGHT")
parser.add_argument("--retired", action="store_true")
args = parser.parse_args()
text = args.serial.read_text(errors="replace")
expected = []
for mode in args.modes:
    width, height = map(int, mode.split("x"))
    assert width > 0 and height > 0
    page_bytes = ((width * height * 4 + 4095) // 4096) * 4096
    expected.extend([page_bytes] * 3)
assert text.count("desktop: native scene allocation bytes=0") == 1, "missing/duplicate native session"
assert re.search(rf"desktop: active outputs=\s*{len(args.modes)}\b", text), "unexpected output count"
records = [tuple(map(int, values)) for values in re.findall(
    r"desktop: pixel storage request=\s*(\d+) charged=\s*(\d+) limit=\s*(\d+)", text)]
assert [record[0] for record in records] == expected, (records, expected)
charged = 0
for size, observed, limit in records:
    charged += size
    assert observed == charged <= limit, (size, observed, charged, limit)
if args.retired:
    releases = [tuple(map(int, values)) for values in re.findall(
        r"desktop: pixel storage released=\s*(\d+) charged=\s*(\d+)", text)]
    assert sorted(size for size, _ in releases) == sorted(expected), releases
    for size, observed in releases:
        charged -= size
        assert observed == charged, (size, observed, charged)
    assert charged == 0 and "desktop: pixel teardown charged= 0" in text
print(f"PASS native pixel ledger: {len(records)} registered targets, "
      f"{sum(expected)} bytes, no scene/drag allocation" +
      (", every target retired and charge returned to zero" if args.retired else ""))
