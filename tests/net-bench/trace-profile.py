#!/usr/bin/env python3
"""Where one CPU's time goes in a LATENCY-TRACE timeline (kernels built with
LATENCY_TRACE=1; tests/net-bench dumps one mid-download and one after the
round trips). Time between consecutive events is charged to the process of
the earlier event; pid 0 is idle. Also counts system calls by number and
process switches.

usage: trace-profile.py SERIAL_LOG [DUMP_INDEX]   (0 = first dump)
"""
import re, sys
from collections import Counter, defaultdict

log = open(sys.argv[1], errors="replace").read().splitlines()
which = int(sys.argv[2]) if len(sys.argv) > 2 else 0
dumps, cur = [], None
pat = re.compile(r"LATENCY-TRACE: tsc=(\d+) pid=(\d+) event=(\S+) a=(\d+) b=(\d+)")
names = {}
for line in log:
    if "LATENCY-TRACE: cpu=" in line:
        cur = []; dumps.append(cur); continue
    m = pat.search(line)
    if m and cur is not None:
        cur.append((int(m[1]), int(m[2]), m[3], int(m[4]), int(m[5])))
    n = re.search(r"procmgr: (?:started|launched) (\S+).*pid[= ](\d+)", line)
    if n: names[int(n[2])] = n[1]
if not dumps:
    sys.exit("no LATENCY-TRACE dump in log")
ev = dumps[which]
span = ev[-1][0] - ev[0][0]
time = Counter(); calls = defaultdict(Counter); switches = 0; last_pid = None
for (t0, p0, e0, a0, b0), (t1, *_ ) in zip(ev, ev[1:]):
    time[p0] += t1 - t0
for t, p, e, a, b in ev:
    if e == "syscall_enter": calls[p][a] += 1
    if e == "schedule_run":
        if last_pid is not None and p != last_pid: switches += 1
        last_pid = p
print(f"dump {which}: {len(ev)} events over {span} cycles, {switches} switches")
# Stalls: a process stops, something is (or becomes) ready, and nothing is
# scheduled for over 20 us. On a kernel with one-shot/wakeup scheduling these
# end at a late timer (2026-09-27, docs/netstack-redesign.md).
TSC_PER_US = 3770
stall_total = 0
for i, (t, p, e, a, b) in enumerate(ev):
    if e != "schedule_stop": continue
    j = i + 1
    while j < len(ev) and ev[j][2] != "schedule_run": j += 1
    if j == len(ev): continue
    gap = ev[j][0] - t
    readied = [x[3] for x in ev[max(0, i - 3):j] if x[2] == "ready"]
    if gap > 20 * TSC_PER_US and readied:
        late = [x[3] for x in ev[i:j] if x[2] == "timer_late"]
        stall_total += gap
        print(f"  stall {gap / TSC_PER_US:6.1f} us after pid {p} stopped; readied {readied}; timer_late {late}")
print(f"  stalls: {100 * stall_total / span:.1f}% of the snapshot")
for p, c in time.most_common():
    top = ", ".join(f"{n}x{k}" for n, k in calls[p].most_common(6))
    print(f"  pid {p:>3} {names.get(p, ''):<14} {100*c/span:5.1f}%  {c:>10} cycles  syscalls: {top}")
