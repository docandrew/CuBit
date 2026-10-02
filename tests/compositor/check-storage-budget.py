"""Check live Desktop admission against the single/mixed native pixel fixtures."""
from pathlib import Path
import re
import sys


def check(text, mixed, limited=False, retired=False):
    page = 4096
    def allocation(payload):
        # Owned allocations are page aligned; charge the rounded payload only.
        return ((payload - 1) // page + 1) * page
    expected = [allocation(1024 * 768 * 4)] * 3
    if mixed:
        expected += [allocation(1280 * 720 * 4)] * 3
        expected += [allocation((1024 + 1280) * (768 + 720) * 4)] * 2
    else:
        expected += [allocation(1024 * 768 * 4)] * 2
    limit = 8 * allocation(16 * 1024 * 1024)
    if limited:
        if not mixed:
            raise ValueError("limited fixture requires mixed outputs")
        expected.pop()  # Optional drag layer is refused before allocation.
        limit = 35 * 1024 * 1024
    rows = re.findall(r"desktop: pixel storage request=\s*(\d+) charged=\s*(\d+) limit=\s*(\d+)", text)
    if len(rows) != len(expected):
        raise ValueError("wrong allocation count")
    total = 0
    for (request, charged, ceiling), size in zip(rows, expected):
        total += size
        if tuple(map(int, (request, charged, ceiling))) != (size, total, limit) or total > limit:
            raise ValueError("incorrect allocation charge or ceiling")
    if text.count("pixel storage budget exhausted") != int(limited):
        raise ValueError("unexpected admission failure count")
    if limited and "desktop: retained drag layer unavailable" not in text:
        raise ValueError("missing optional-layer fallback")
    if "scene allocation failed" in text:
        raise ValueError("fixture failed admission")
    if retired:
        releases = list(re.finditer(r"desktop: pixel storage released=\s*(\d+) charged=\s*(\d+)", text))
        if len(releases) != len(expected):
            raise ValueError("wrong release count")
        remaining = total
        renderer = text.find("desktop: renderer targets retired")
        readers = text.find("desktop: output readers retired= 0")
        if not 0 <= renderer < readers < releases[0].start():
            raise ValueError("release preceded confirmed reader retirement")
        if mixed:
            second = text.find("desktop: output readers retired= 1")
            if not releases[2].start() < second < releases[3].start():
                raise ValueError("second output release preceded reader retirement")
        for release, size in zip(releases, expected):
            remaining -= size
            if tuple(map(int, release.groups())) != (size, remaining):
                raise ValueError("incorrect release charge or refund")
        end = text.find("desktop: pixel teardown charged= 0")
        if end <= releases[-1].start():
            raise ValueError("missing zero-charge teardown")
        if "uncertain" in text or "pixel allocation failed" in text or "identity mismatch" in text:
            raise ValueError("uncertain lifetime in successful teardown fixture")
    return total, limit


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4) or sys.argv[2] not in ("single", "mixed", "limited") or (len(sys.argv) == 4 and sys.argv[3] != "--retired"):
        raise SystemExit("usage: check-storage-budget.py SERIAL_LOG single|mixed|limited [--retired]")
    try:
        total, limit = check(Path(sys.argv[1]).read_text(), sys.argv[2] != "single", sys.argv[2] == "limited", len(sys.argv) == 4)
        print(f"storage budget: PASS {total} charged bytes, {limit} byte ceiling")
    except (OSError, ValueError) as error:
        raise SystemExit(f"storage budget: FAIL {error}")
