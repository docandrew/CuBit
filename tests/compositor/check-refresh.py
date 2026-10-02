"""Check that native Desktop completed and published a bounded menu refresh."""
import re
import sys
from pathlib import Path


def check(text):
    pending = None
    published = 0
    for line in text.splitlines():
        if any(marker in line for marker in (
            "menu refresh uncertain", "completion queue unavailable",
            "unknown presentation completion", "asynchronous transfer quarantined"
        )):
            raise ValueError("uncertain refresh or presentation ownership")
        match = re.search(r"desktop: menu refresh (ready|published) count=\s*(\d+)", line)
        if not match:
            continue
        kind, count = match[1], int(match[2])
        if not 1 <= count <= 14:
            raise ValueError("no usable bounded menu in refresh evidence")
        if kind == "ready":
            if pending is not None:
                raise ValueError("another candidate replaced an unpublished menu")
            pending = count
        else:
            if pending != count:
                raise ValueError("publication without matching complete candidate")
            pending = None
            published += 1
    if published == 0:
        raise ValueError("no completed menu publication")
    return published


if __name__ == "__main__":
    try:
        count = check(Path(sys.argv[1]).read_text())
        print(f"refresh: PASS {count} native menu publications")
    except (ValueError, OSError) as error:
        raise SystemExit(f"refresh evidence rejected: {error}")
