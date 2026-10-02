"""Require native direct rendering, released frames, repair work, and no staging copies."""
import re
import sys
from pathlib import Path


def check(text):
    for marker in ("desktop: direct pooled rendering active",
                   "desktop: asynchronous frame released"):
        if marker not in text:
            raise ValueError(f"missing {marker}")
    if any(marker in text for marker in (
        "quarantined", "TEST: FAIL", "EXCEPTION:", "unknown presentation completion",
        "invalid presentation transition", "asynchronous submission unavailable"
    )):
        raise ValueError("failed or uncertain native rendering")
    counters = re.findall(
        r"GRAPHICS: stage=desktop_staging bytes=\s*(\d+) regions=\s*(\d+) overflow=(\d+)", text
    )
    if not counters or any(tuple(map(int, item)) != (0, 0, 0) for item in counters):
        raise ValueError("missing zero-copy staging evidence or nonzero staging counter")
    submitted = sum(map(int, re.findall(r"\bsubmit=(\d+)", text)))
    repair = sum(map(int, re.findall(r"\brepair_px=(\d+)", text)))
    if submitted < 2 or repair == 0:
        raise ValueError("no repeated submission and slot-repair evidence")
    return submitted, repair


if __name__ == "__main__":
    try:
        submitted, repair = check(Path(sys.argv[1]).read_text())
        print(f"direct: PASS zero staging copies; {submitted} submissions; {repair} repair pixels")
    except (ValueError, OSError) as error:
        raise SystemExit(f"direct evidence rejected: {error}")
