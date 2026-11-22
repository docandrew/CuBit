#!/usr/bin/env python3
"""Merge CENSUS lines (kernel built with SYSCALL_CENSUS=1, src/syscall_census.ads)
from serial logs into one table: per system call, its calls and the programs
that made them. Calls no run made are listed with 0.

  census.py [--repo DIR] [--tsv OUT] SERIAL...
"""
import argparse, collections, glob, os, re, sys

def call_names(repo):
    """Raw number -> SYSCALL_ name, from the dispatcher's decodeSyscall."""
    src = open(os.path.join(repo, "kernel/src/syscall.adb")).read()
    body = src[src.index("procedure decodeSyscall"):src.index("end decodeSyscall")]
    consts = {}
    for path in glob.glob(os.path.join(repo, "kernel/src/*.ads")) + \
                glob.glob(os.path.join(repo, "shared/*/*.ads")):
        pkg = os.path.basename(path)[:-4]
        for m in re.finditer(r"System_Call_Number\s*:\s*constant\s*:=\s*(\d+)", open(path).read()):
            consts[pkg.lower()] = int(m.group(1))
    names = {}
    for m in re.finditer(r"when\s+([\w.]+)\s*=>\s*number\s*:=\s*(SYSCALL_\w+)", body, re.S):
        key, name = m.group(1), m.group(2)
        if key.isdigit():
            names[int(key)] = name
        elif key.endswith(".System_Call_Number"):
            names[consts[key.split(".")[0].lower()]] = name
    return names

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default=os.path.join(os.path.dirname(__file__), "../.."))
    ap.add_argument("--tsv")
    ap.add_argument("serials", nargs="+")
    a = ap.parse_args()
    names = call_names(a.repo)
    # Per (log, identity): the last line wins (cumulative counts).
    lives = {}
    for path in a.serials:
        for line in open(path, errors="replace"):
            m = re.search(r"CENSUS: id=(\d+) name=(\S+) final=([01])((?: \d+=\d+)*)", line)
            if not m:
                continue
            counts = {int(k): int(v) for k, v in re.findall(r"(\d+)=(\d+)", m.group(4))}
            key = (path, m.group(1))
            old = lives.get(key)
            if old is None or old[1] != "1":
                lives[key] = (m.group(2).strip("_"), m.group(3), counts)
    total = collections.Counter()
    who = collections.defaultdict(collections.Counter)
    for (path, _), (prog, _, counts) in lives.items():
        for num, n in counts.items():
            total[num] += n
            who[num][prog] += n
    rows = []
    for num, name in sorted(names.items()):
        progs = ", ".join(f"{p}:{n}" for p, n in who[num].most_common())
        rows.append((name, num, total[num], progs))
    unknown = sorted(set(total) - set(names))
    out = sys.stdout if not a.tsv else open(a.tsv, "w")
    print("call\tnumber\tcalls\tprograms (calls)", file=out)
    for r in rows:
        print(f"{r[0]}\t{r[1]}\t{r[2]}\t{r[3]}", file=out)
    for num in unknown:
        print(f"?\t{num}\t{total[num]}\t{', '.join(who[num])}", file=out)
    print(f"# {len(lives)} process lives from {len(a.serials)} logs; "
          f"{sum(1 for r in rows if r[2] == 0)} of {len(rows)} calls never made", file=sys.stderr)

if __name__ == "__main__":
    main()
