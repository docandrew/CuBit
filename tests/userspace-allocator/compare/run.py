#!/usr/bin/env python3
"""Run alloc-bench binaries (build.sh) in interleaved rounds and report medians.

  run.py --bin NAME=PATH ... [--rounds 5] [--cpu 6] [--pairs 1000000]
         [--threads 1,2,4,8] [--thread-rounds 256] [--replay TRACE ...]
         [--output DIR]

Every (binary, case) job of a round runs once per round, in a shuffled
order, each in its own process; single-thread cases are pinned to one CPU,
thread cases to as many CPUs as threads, starting at --cpu. Reported: the
median over rounds of each run's ns/pair (or ns/op), the medians of its
sampled p50/p99/p99.9, peak RSS, and the live rounding waste. Hold
coordination/build.lock while it runs. Linux-hosted only.
"""
import argparse
import json
import os
import pathlib
import random
import statistics
import subprocess

TRACES = ['fixed64', 'fixed256', 'small', 'mixed', 'boundary', 'bimodal', 'medium', 'medium-full']
p = argparse.ArgumentParser()
p.add_argument('--bin', action='append', required=True, help='NAME=PATH of an alloc-bench binary')
p.add_argument('--rounds', type=int, default=5)
p.add_argument('--cpu', type=int, default=6)
p.add_argument('--pairs', type=int, default=1_000_000)
p.add_argument('--traces', default=','.join(TRACES))
p.add_argument('--threads', default='1,2,4,8')
p.add_argument('--thread-rounds', type=int, default=256)
p.add_argument('--split', type=int, default=10_000)
p.add_argument('--strings', type=int, default=0, help='appends of the string-building trace (0: none)')
p.add_argument('--replay', action='append', default=[])
p.add_argument('--output', type=pathlib.Path, required=True)
a = p.parse_args()
bins = dict(b.split('=', 1) for b in a.bin)
cases = [('single', t) for t in a.traces.split(',') if t]
if a.split:
    cases.append(('split', None))
if a.strings:
    cases.append(('strings', None))
for n in [int(x) for x in a.threads.split(',') if x]:
    cases.append(('threads', ('local', n)))
    if n > 1:
        cases.append(('threads', ('remote', n)))
cases += [('replay', r) for r in a.replay]


def command(path, mode, arg):
    if mode == 'single':
        return [path, 'single', arg, str(a.pairs)], 1
    if mode == 'split':
        return [path, 'split', str(a.split)], 1
    if mode == 'strings':
        return [path, 'strings', str(a.strings)], 1
    if mode == 'threads':
        return [path, 'threads', arg[0], str(arg[1]), str(a.thread_rounds)], arg[1]
    return [path, 'replay', arg], 1


results = []
for r in range(a.rounds):
    jobs = [(name, mode, arg) for name in bins for mode, arg in cases]
    random.Random(1000 + r).shuffle(jobs)
    for name, mode, arg in jobs:
        argv, cpus = command(bins[name], mode, arg)
        out = subprocess.run(['taskset', '-c', f'{a.cpu}-{a.cpu + cpus - 1}'] + argv,
                             capture_output=True, text=True, timeout=600)
        if out.returncode:
            raise SystemExit(f'{name} {mode} {arg} failed: {out.stdout}{out.stderr}')
        row = json.loads(out.stdout)
        row.update(name=name, round=r, case=f'{mode}:{arg[0] + str(arg[1]) if mode == "threads" else arg}')
        results.append(row)
a.output.mkdir(parents=True, exist_ok=True)
(a.output / 'results.json').write_text(json.dumps(results, indent=1) + '\n')


def med(rows, key):
    v = [x[key] for x in rows if key in x]
    return statistics.median(v) if v else float('nan')


lines = [f'Rounds: {a.rounds}; CPU {a.cpu}; medians over rounds.', '',
         '| Case | ' + ' | '.join(bins) + ' |', '|---|' + '---:|' * len(bins)]
for case in dict.fromkeys(r['case'] for r in results):
    cells = []
    for name in bins:
        rows = [x for x in results if x['case'] == case and x['name'] == name]
        if case.startswith('split'):
            cells.append(f'{med(rows, "alloc_ns"):.1f}+{med(rows, "free_ns"):.1f}')
        else:
            key = 'ns_per_op' if case.startswith('replay') else 'ns_per_pair'
            cells.append(f'{med(rows, key):.1f} ({med(rows, "p99_ns"):.0f}; {med(rows, "peak_rss_kib"):.0f}K)')
    lines.append(f'| {case} | ' + ' | '.join(cells) + ' |')
lines += ['', 'Cells: ns per pair (or op) (sampled p99 ns; peak RSS KiB). split: alloc+free ns.', '',
          '| Trace | ' + ' | '.join(f'{n} waste %' for n in bins) + ' |', '|---|' + '---:|' * len(bins)]
for case in dict.fromkeys(r['case'] for r in results if r['mode'] == 'single'):
    cells = []
    for name in bins:
        rows = [x for x in results if x['case'] == case and x['name'] == name]
        cells.append(f'{statistics.median(100 * (x["live_usable"] / x["live_requested"] - 1) for x in rows):.1f}')
    lines.append(f'| {case} | ' + ' | '.join(cells) + ' |')
(a.output / 'summary.md').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
