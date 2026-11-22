#!/usr/bin/env python3
"""References and CuAlloc (a heap per thread) under threads. The old
single-owner slab prototype (cubit-slabs) is never raced."""
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
cpus = list(cores.values())[:8]
counts = [n for n in [1, 2, 4, 8] if n <= len(cpus)]
libraries = {'glibc': None, 'mimalloc': os.environ['CUBIT_BENCH_MIMALLOC'],
             'jemalloc': os.environ['CUBIT_BENCH_JEMALLOC'],
             'tcmalloc-gperftools': os.environ['CUBIT_BENCH_TCMALLOC'], 'cualloc': None}
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
    'note': 'Whole group restricted to physical cores, individual workers not pinned. Remote includes channel transfer and synchronization. cualloc: CuAlloc, each thread its own heap (hosted, Linux provider), called directly; Rust channels keep glibc malloc.',
    'results': results}, indent=2) + '\n')
lines = ['# Allocator concurrency', '',
         'ns / pair: elapsed wall time / all pairs (aggregate throughput cost). Remote includes channel costs.',
         'Pair p50/p99/p99.9: one extra allocate+free timed alone every 64th allocation, all threads pooled (medians across repetitions).', '',
         '| Workload | Threads | Allocator | ns / pair | pair p50 ns | pair p99 ns | pair p99.9 ns | peak RSS KiB |',
         '|---|---:|---|---:|---:|---:|---:|---:|']
for work in ['local', 'remote']:
    for count in counts:
        if work == 'remote' and count == 1: continue
        for engine in libraries:
            rows = [r for r in results if r['engine'] == engine and r['workload'] == work and r['threads'] == count]
            vals = [statistics.median(r[k] for r in rows)
                    for k in ['ns_per_pair', 'pair_p50_ns', 'pair_p99_ns', 'pair_p999_ns', 'peak_rss_kib']]
            lines.append(f'| {work} | {count} | {engine} | ' + ' | '.join(f'{v:.1f}' for v in vals) + ' |')
(args.output / 'summary.md').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
