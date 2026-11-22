#!/usr/bin/env python3
"""Per-process system call rates from one census serial log (kernel built
with SYSCALL_CENSUS=1). Each report round starts with `CENSUS: round ms=N`;
a process's line carries cumulative counts and appears only in rounds where
they changed, so a missing line is a round with no calls.

  rates.py [--from MS] [--to MS] [--top N] [--repo DIR] SERIAL

Prints, per process and call, the total calls in the window and the median,
minimum and maximum calls a second over the window's rounds.
"""
import argparse, collections, os, re, statistics, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from census import call_names  # noqa: E402

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default=os.path.join(os.path.dirname(__file__), "../.."))
    ap.add_argument("--from", dest="start", type=int, default=0)
    ap.add_argument("--to", dest="end", type=int, default=1 << 62)
    ap.add_argument("--top", type=int, default=40)
    ap.add_argument("serial")
    a = ap.parse_args()
    names = call_names(a.repo)
    rounds = []          # [(ms, {(id, name): {call: cumulative}})]
    for line in open(a.serial, errors="replace"):
        m = re.search(r"CENSUS: round ms=(\d+)", line)
        if m:
            rounds.append((int(m.group(1)), {}))
            continue
        m = re.search(r"CENSUS: id=(\d+) name=(\S+) final=0((?: \d+=\d+)*)", line)
        if m and rounds:
            counts = {int(k): int(v) for k, v in re.findall(r"(\d+)=(\d+)", m.group(3))}
            rounds[-1][1][(m.group(1), m.group(2).strip("_"))] = counts
    if len(rounds) < 2:
        sys.exit("no census rounds (kernel without the round line?)")
    last = {}            # process -> cumulative counts so far
    per = collections.defaultdict(list)   # (process, call) -> [rate per round]
    total = collections.Counter()
    seen = set()
    window = [r for r in rounds if a.start <= r[0] <= a.end]
    prev_ms = None
    for ms, lines in rounds:
        inside = a.start <= ms <= a.end and prev_ms is not None
        dt = (ms - prev_ms) / 1000.0 if prev_ms is not None else None
        deltas = collections.defaultdict(dict)
        for proc, counts in lines.items():
            old = last.get(proc, {})
            # Counts only grow: a line that shrinks one was cut by other
            # serial output; skip it (the next line carries the total).
            if any(n < old.get(c, 0) for c, n in counts.items()):
                continue
            for c, n in counts.items():
                d = n - old.get(c, 0)
                if d:
                    deltas[proc][c] = d
            last[proc] = counts
        if inside and dt:
            for proc in set(deltas) | {p for p, _ in seen}:
                for c, d in deltas.get(proc, {}).items():
                    seen.add((proc, c))
                    total[(proc, c)] += d
            for key in seen:
                per[key].append(deltas.get(key[0], {}).get(key[1], 0) / dt)
        prev_ms = ms
    span = (window[-1][0] - window[0][0]) / 1000.0 if len(window) > 1 else 0
    print(f"# window {window[0][0] if window else '-'}..{window[-1][0] if window else '-'} ms "
          f"({span:.1f} s, {len(window)} rounds)")
    print("process\tcall\tnumber\ttotal\tmedian/s\tmin/s\tmax/s")
    for (proc, c), n in total.most_common(a.top):
        rs = per[(proc, c)]
        # rounds before the pair's first call are not in rs; pad with zeros
        rs = [0.0] * (max(0, len(window) - 1 - len(rs))) + rs
        print(f"{proc[1]}\t{names.get(c, '?')}\t{c}\t{n}\t{statistics.median(rs):.0f}"
              f"\t{min(rs):.0f}\t{max(rs):.0f}")

if __name__ == "__main__":
    main()
