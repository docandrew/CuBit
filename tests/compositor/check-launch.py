"""Require input dispatch between submission/completion of the same Desktop launch."""
import re
import sys
from pathlib import Path

def check(text):
    submitted, input_seen, complete = {}, {}, {}
    for line_no, line in enumerate(text.splitlines()):
        if any(marker in line for marker in (
            "launch submission uncertain", "launch completion uncertain",
            "unknown presentation completion", "asynchronous transfer quarantined")):
            raise ValueError("uncertain launch or presentation ownership")
        for pattern, target in (
            (r"desktop: launch submitted token=(\d+)", submitted),
            (r"desktop: input during launch token=(\d+)", input_seen),
            (r"desktop: launch complete token=(\d+) pid=[1-9]\d*", complete)):
            match = re.search(pattern, line)
            if match:
                token = int(match[1])
                if token in target or not 0 < token < 2**64-1:
                    raise ValueError("reused or invalid launch token")
                target[token] = line_no
    if not any(token in input_seen and token in complete and
               start < input_seen[token] < complete[token]
               for token, start in submitted.items()):
        raise ValueError("no input dispatch during a matching pending launch")
    return len(complete)

if __name__ == "__main__":
    try:
        count = check(Path(sys.argv[1]).read_text())
        print(f"launch: PASS input during pending request; {count} completed launches")
    except (ValueError, OSError) as error:
        raise SystemExit(f"launch evidence rejected: {error}")
