#!/usr/bin/env python3
"""CuAlloc's routing and miss-scan costs against the references, each run in a
fresh CPU-pinned process (tests/userspace-allocator/scaling_bench.rs)."""
import argparse
import datetime
import json
import os
import pathlib
import random
import statistics
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser()
p.add_argument('--iterations', type=int, default=200000)
p.add_argument('--repetitions', type=int, default=3)
p.add_argument('--cpu', type=int)
p.add_argument('--output', type=pathlib.Path, default=ROOT / 'tests/performance/results/allocator-scaling')
args = p.parse_args()
allowed = os.sched_getaffinity(0)
cpu = min(allowed) if args.cpu is None else args.cpu
if cpu not in allowed:
    p.error(f'CPU {cpu} is outside {sorted(allowed)}')
libraries = {'cualloc': None, 'glibc': None,
             'mimalloc': os.environ['CUBIT_BENCH_MIMALLOC'], 'jemalloc': os.environ['CUBIT_BENCH_JEMALLOC']}
cases = [('routing', n) for n in [0, 64, 1024, 8192]] + [('miss', k) for k in [1, 4, 16, 32]]
jobs = [(e, m, n, r) for e in libraries for m, n in cases for r in range(args.repetitions)]
random.Random(44).shuffle(jobs)
results = []
for engine, mode, scale, rep in jobs:
    env = {k: v for k, v in os.environ.items()
           if k not in ('LD_PRELOAD', 'GLIBC_TUNABLES') and not k.startswith(('MALLOC_', 'MIMALLOC_', 'JE_'))}
    if libraries[engine]:
        env['LD_PRELOAD'] = libraries[engine]
    run = subprocess.run(['taskset', '-c', str(cpu), str(ROOT / 'tests/userspace-allocator/build/scaling_bench'),
                          engine, mode, str(scale), str(args.iterations)],
                         env=env, capture_output=True, text=True, timeout=600)
    if run.returncode:
        raise SystemExit(f'{engine}/{mode}/{scale}: {run.stderr}')
    row = json.loads(run.stdout)
    row['repetition'] = rep
    results.append(row)
args.output.mkdir(parents=True, exist_ok=True)
(args.output / 'results.json').write_text(json.dumps({
    'timestamp_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(), 'cpu': cpu,
    'load_average': os.getloadavg(), 'libraries': libraries, 'results': results}, indent=2) + '\n')
lines = ['# CuAlloc scaling costs', '',
         'routing N: 64-byte churn with N huge blocks live (CuAlloc: N directory records, binary-search free).',
         'miss K: 4096-byte churn over K full arenas of live blocks (CuAlloc: each miss scans arenas from the first).',
         'pair = free + allocate, no payload touches; medians across repetitions.', '',
         '| Case | Scale | Allocator | ns / pair | sampled p50 ns | sampled p99 ns | sampled p99.9 ns |',
         '|---|---:|---|---:|---:|---:|---:|']
for mode, scale in cases:
    for engine in libraries:
        rows = [r for r in results if r['engine'] == engine and r['mode'] == mode and r['scale'] == scale]
        vals = [statistics.median(r[k] for r in rows) for k in ['ns_per_pair', 'sampled_p50_ns', 'sampled_p99_ns', 'sampled_p999_ns']]
        lines.append(f'| {mode} | {scale} | {engine} | ' + ' | '.join(f'{v:.1f}' for v in vals) + ' |')
(args.output / 'summary.md').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
