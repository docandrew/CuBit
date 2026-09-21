#!/usr/bin/env python3
"""Identical traces in isolated, CPU-pinned subprocesses. No native OS mutation."""
import argparse
import datetime
import hashlib
import json
import os
import pathlib
import platform
import random
import statistics
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('--iterations', type=int, default=300000)
parser.add_argument('--repetitions', type=int, default=5)
parser.add_argument('--cpu', type=int, help='Allowed logical CPU for all benchmark subprocesses')
parser.add_argument('--baseline', type=pathlib.Path,
                    help='Previously built bench executable; interleave its CuBit runs with current/reference runs')
parser.add_argument('--output', type=pathlib.Path, default=ROOT / 'tests/performance/results/allocator')
args = parser.parse_args()
if args.iterations < 1000 or args.repetitions < 1:
    parser.error('iterations must be >=1000 and repetitions positive')
allowed_cpus = os.sched_getaffinity(0)
cpu = min(allowed_cpus) if args.cpu is None else args.cpu
if cpu not in allowed_cpus:
    parser.error(f'CPU {cpu} is outside the allowed affinity set {sorted(allowed_cpus)}')
os.sched_setaffinity(0, {cpu})
args.output.mkdir(parents=True, exist_ok=True)
libraries = {
    'glibc': None, 'cubit-slabs': None,
    'mimalloc': os.environ['CUBIT_BENCH_MIMALLOC'],
    'jemalloc': os.environ['CUBIT_BENCH_JEMALLOC'],
    'tcmalloc-gperftools': os.environ['CUBIT_BENCH_TCMALLOC'],
}
if args.baseline:
    args.baseline = args.baseline.resolve(strict=True)
    libraries['cubit-baseline'] = None
for library in libraries.values():
    if library and not pathlib.Path(library).is_file():
        raise SystemExit(f'Missing reference library: {library}')
workloads = ['fixed64', 'fixed256', 'small', 'mixed', 'boundary', 'bimodal']
jobs = [(engine, work, rep) for engine in libraries
        for work in workloads
        for rep in range(args.repetitions)]
random.Random(42).shuffle(jobs)
results = []
for engine, work, rep in jobs:
    env = os.environ.copy()
    for name in list(env):
        if name in ['LD_PRELOAD', 'GLIBC_TUNABLES', 'CUBIT_PERF_CONTROL', 'CUBIT_PERF_ACK'] or name.startswith(('MALLOC_', 'MIMALLOC_', 'TCMALLOC_')):
            env.pop(name)
    if libraries[engine]:
        env['LD_PRELOAD'] = libraries[engine]
    executable = args.baseline if engine == 'cubit-baseline' else ROOT / 'tests/userspace-allocator/build/bench'
    argument = 'cubit-slabs' if engine == 'cubit-baseline' else engine
    run = subprocess.run([str(executable), argument, work, str(args.iterations)], env=env,
                         capture_output=True, text=True, timeout=120)
    if run.returncode:
        raise SystemExit(f'{engine}/{work} failed ({run.returncode}):\n{run.stdout}\n{run.stderr}')
    row = json.loads(run.stdout)
    if row['engine'] != argument or row['workload'] != work:
        raise SystemExit(f'Unexpected benchmark identity: {row}')
    row['engine'] = engine
    row['repetition'] = rep
    results.append(row)
metadata = {'timestamp_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
            'platform': platform.platform(), 'cpu': cpu,
            'cpuinfo': next((block for block in pathlib.Path('/proc/cpuinfo').read_text().split('\n\n')
                             if block and block.splitlines()[0].split(':', 1)[-1].strip() == str(cpu)), ''),
            'load_average': os.getloadavg(), 'libraries': libraries,
            'git_revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
            'source_sha256': {str(f.relative_to(ROOT)): hashlib.sha256(f.read_bytes()).hexdigest()
                              for directory in ['userspace/allocator/src', 'userspace/allocator/host', 'tests/userspace-allocator']
                              for f in sorted((ROOT / directory).iterdir()) if f.is_file()},
            'binary_sha256': hashlib.sha256((ROOT / 'tests/userspace-allocator/build/bench').read_bytes()).hexdigest(),
            'baseline': ({'path': str(args.baseline),
                          'sha256': hashlib.sha256(args.baseline.read_bytes()).hexdigest()}
                         if args.baseline else None),
            'build_inputs_sha256': {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
                                   for name in ['flake.lock', 'flake.nix', 'userspace/allocator/allocator_host.gpr']},
            'glibc': subprocess.check_output(['getconf', 'GNU_LIBC_VERSION'], text=True).strip(),
            'rustc': subprocess.check_output(['rustc', '--version'], text=True).strip(),
            'gnat': subprocess.check_output(['alr', 'exec', '--', 'gnatls', '--version'], cwd=ROOT / 'kernel', text=True).splitlines()[0],
            'note': 'Working-tree prototype; single owner; pair=free+allocate+two byte touches. Shared 16MiB arena with empty-slab reassignment, not a production heap.'}
(args.output / 'results.json').write_text(json.dumps({'metadata': metadata, 'results': results}, indent=2) + '\n')
lines = ['# Hosted allocator pilot', '',
         'Medians across isolated runs. Latency samples include timer overhead. RSS is whole-process peak, not fragmentation.', '',
         '| Workload | Allocator | ns / pair | sampled p50 ns | sampled p99 ns | peak RSS KiB | live rounding overhead % |',
         '|---|---|---:|---:|---:|---:|---:|']
for work in workloads:
    for engine in libraries:
        rows = [r for r in results if r['engine'] == engine and r['workload'] == work]
        vals = [statistics.median(r[key] for r in rows) for key in
                ['ns_per_pair', 'sampled_p50_ns', 'sampled_p99_ns', 'process_peak_rss_kib']]
        vals.append(statistics.median(100 * (r['live_usable_bytes'] / r['live_requested_bytes'] - 1) for r in rows))
        lines.append(f'| {work} | {engine} | ' + ' | '.join(f'{v:.1f}' for v in vals) + ' |')
lines += ['', '## Jemalloc target', '',
          'Current CuBit median / jemalloc median on each trace; target <= 1.10. Not an overall allocator ranking.', '',
          '| Workload | Ratio | Within 10% |', '|---|---:|---|']
for work in workloads:
    ours = statistics.median(r['ns_per_pair'] for r in results
                             if r['engine'] == 'cubit-slabs' and r['workload'] == work)
    reference = statistics.median(r['ns_per_pair'] for r in results
                                  if r['engine'] == 'jemalloc' and r['workload'] == work)
    ratio = ours / reference
    lines.append(f'| {work} | {ratio:.3f} | {"yes" if ratio <= 1.10 else "no"} |')
(args.output / 'summary.md').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
