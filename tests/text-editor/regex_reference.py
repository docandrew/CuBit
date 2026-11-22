"""Regex reference corpus for text_core_tests (tests/text-editor/README.md).

Writes random patterns and texts with Python re's first match (a mature
backtracking matcher, so leftmost-first semantics like the Pike VM's
priorities). Usage: regex_reference.py OUT [COUNT] [SEED]. One case per line,
tab-separated: pattern, text, case-sensitive (1/0), found (1/0), first
(1-based), length, loose (1/0: an empty-matching group is repeated).
"""
import random
import re
import signal
import sys

ATOMS = ["a", "b", ".", "[ab]", "c", r"\w", "[^a]", "A", " ", r"\b", r"\d", "[a-c]", r"\s", "x"]
QUANTIFIERS = ["", "", "", "", "*", "+", "?", "*?", "+?", "??"]
ALPHABET = "abAB c1x"


def pattern(rng, depth, loose):
    """A random pattern. loose[0] becomes True when a repeated group can
    match the empty string: there backtracking engines end the loop on an
    empty iteration while a Pike VM (like RE2) prefers the consuming path,
    so only whether and where a match starts must agree."""
    parts = []
    for _ in range(rng.randint(1, 4)):
        if depth > 0 and rng.random() < 0.2:
            inner = pattern(rng, depth - 1, loose)
            if rng.random() < 0.5:
                inner += "|" + pattern(rng, depth - 1, loose)
            atom = "(" + inner + ")"
            quantifier = rng.choice(QUANTIFIERS)
            # Nullable in some context: assertions (\b ^ $) may hold there.
            bare = inner.replace(r"\b", "").replace("^", "").replace("$", "")
            if quantifier[:1] in ("*", "+") and re.fullmatch(bare, "") is not None:
                loose[0] = True
            atom += quantifier
        else:
            atom = rng.choice(ATOMS)
            if atom != r"\b":
                atom += rng.choice(QUANTIFIERS)
        parts.append(atom)
    text = "".join(parts)
    roll = rng.random()
    if roll < 0.1:
        text = "^" + text
    elif roll < 0.2:
        text = text + "$"
    return text


class Slow(Exception):
    pass


def on_alarm(signum, frame):
    raise Slow()


def main():
    # Backtracking can be exponential: a case slower than 50 ms is skipped.
    signal.signal(signal.SIGALRM, on_alarm)
    out = sys.argv[1]
    count = int(sys.argv[2]) if len(sys.argv) > 2 else 20000
    rng = random.Random(int(sys.argv[3]) if len(sys.argv) > 3 else 20261014)
    with open(out, "w", encoding="ascii") as f:
        written = 0
        while written < count:
            loose = [False]
            p = pattern(rng, 2, loose)
            t = "".join(rng.choice(ALPHABET) for _ in range(rng.randint(0, 24)))
            sensitive = rng.random() < 0.5
            try:
                signal.setitimer(signal.ITIMER_REAL, 0.05)
                m = re.search(p, t, 0 if sensitive else re.IGNORECASE)
            except (re.error, Slow):
                continue
            finally:
                signal.setitimer(signal.ITIMER_REAL, 0)
            if m:
                f.write(f"{p}\t{t}\t{int(sensitive)}\t1\t{m.start() + 1}\t{m.end() - m.start()}\t{int(loose[0])}\n")
            else:
                f.write(f"{p}\t{t}\t{int(sensitive)}\t0\t0\t0\t{int(loose[0])}\n")
            written += 1


if __name__ == "__main__":
    main()
