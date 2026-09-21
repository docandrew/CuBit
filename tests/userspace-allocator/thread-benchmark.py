#!/usr/bin/env python3
"""Baselines only: never race the explicitly single-owner SPARK prototype."""
import argparse
import datetime
import hashlib
import json
import os
import pathlib
import random
import statistics
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser()
p.add_argument('--rounds', type=int, default=256)
p.add_argument('--repetitions', type=int, default=5)
p.add_argument('--output', type=pathlib.Path, default=ROOT / 'tests/performance/results/allocator-threads')
args = p.parse_args()
if args.rounds < 1 or args.repetitions < 1:
    p.error('rounds and repetitions must be positive')
allowed = sorted(os.sched_getaffinity(0))
# Prefer separate physical cores over SMT siblings. Do not alter host settings.
cores = {}
for cpu in allowed:
    topology = pathlib.Path(f'/sys/devices/system/cpu/cpu{cpu}/topology')
    identity = tuple((topology / name).read_text().strip() for name in ['physical_package_id', 'core_id'])
    cores.setdefault(identity, cpu)
cpus = list(cores.values())[:4]
counts = [n for n in [1, 2, 4] if n <= len(cpus)]
libraries = {'glibc': None, 'mimalloc': os.environ['CUBIT_BENCH_MIMALLOC'],
             'jemalloc': os.environ['CUBIT_BENCH_JEMALLOC'],
             'tcmalloc-gperftools': os.environ['CUBIT_BENCH_TCMALLOC']}
jobs = [(e, w, n, rep) for e in libraries for w in ['local', 'remote']
        for n in counts if w != 'remote' or n > 1 for rep in range(args.repetitions)]
random.Random(43).shuffle(jobs)
results = []
for engine, work, count, rep in jobs:
    env = os.environ.copy()
    for name in list(env):
        if name in ['LD_PRELOAD', 'GLIBC_TUNABLES'] or name.startswith(('MALLOC_', 'MIMALLOC_', 'TCMALLOC_')):
            env.pop(name)
    if libraries[engine]: env['LD_PRELOAD'] = libraries[engine]
    run = subprocess.run(['taskset', '-c', ','.join(map(str, cpus[:count])),
                          str(ROOT / 'tests/userspace-allocator/build/thread_bench'), engine,
                          work, str(count), str(args.rounds)], env=env,
                         capture_output=True, text=True, timeout=120)
    if run.returncode: raise SystemExit(f'{engine}/{work}/{count}: {run.stderr}')
    result = json.loads(run.stdout); result['repetition'] = rep; results.append(result)
args.output.mkdir(parents=True, exist_ok=True)
(args.output / 'results.json').write_text(json.dumps({'cpus': cpus, 'libraries': libraries,
    'timestamp_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
    'load_average': os.getloadavg(),
    'binary_sha256': hashlib.sha256((ROOT / 'tests/userspace-allocator/build/thread_bench').read_bytes()).hexdigest(),
    'source_sha256': hashlib.sha256((ROOT / 'tests/userspace-allocator/thread_bench.rs').read_bytes()).hexdigest(),
    'note': 'Reference-only; whole group restricted to physical cores, individual workers not pinned. Remote includes channel transfer and synchronization.',
    'results': results}, indent=2) + '\n')
lines = ['# Allocator concurrency references', '',
         'Throughput experiment, NOT individual operation latency. Remote includes channel costs.',
         'The SPARK pilot does not yet support shared heaps or remote free and is deliberately excluded.', '',
         '| Workload | Threads | Allocator | median ns / pair (aggregate throughput) |',
         '|---|---:|---|---:|']
for work in ['local', 'remote']:
    for count in counts:
        if work == 'remote' and count == 1: continue
        for engine in libraries:
            rows = [r['ns_per_pair'] for r in results if r['engine'] == engine and r['workload'] == work and r['threads'] == count]
            lines.append(f'| {work} | {count} | {engine} | {statistics.median(rows):.1f} |')
(args.output / 'summary.md').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
