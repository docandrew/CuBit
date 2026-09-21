#!/usr/bin/env python3
"""Nix-hosted, userspace-only perf profiles of the existing churn benchmark.

Build first with benchmark.sh. Never changes perf permissions or host settings.
Profiled timings are diagnostic only, not replacement benchmark results.
"""
import argparse
import hashlib
import json
import os
import pathlib
import random
import shutil
import signal
import subprocess
import tempfile
import time

ROOT = pathlib.Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--cpu', type=int, default=7)
parser.add_argument('--iterations', type=int, default=10_000_000)
parser.add_argument('--repetitions', type=int, default=3)
parser.add_argument('--baseline', type=pathlib.Path,
                    help='Preserved benchmark with perf FIFO support; interleave its CuBit runs')
parser.add_argument('--record-only', action='store_true')
parser.add_argument('--sample-event', choices=['cycles:u', 'branch-misses:u'], default='cycles:u')
parser.add_argument('--period', type=int, default=100003)
parser.add_argument('--output', type=pathlib.Path, required=True)
args = parser.parse_args()
if args.cpu not in os.sched_getaffinity(0) or args.iterations < 1000 or args.repetitions < 1 or args.period < 1:
    parser.error('Require an allowed CPU, >=1000 iterations, and positive repetitions')
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=False)
bench = output / 'bench'
shutil.copy2(ROOT / 'tests/userspace-allocator/build/bench', bench)
libraries = {'cubit-slabs': None, 'mimalloc': os.environ['CUBIT_BENCH_MIMALLOC'],
             'jemalloc': os.environ['CUBIT_BENCH_JEMALLOC']}
baseline = None
if args.baseline:
    baseline = output / 'baseline-bench'
    shutil.copy2(args.baseline.resolve(strict=True), baseline)
    libraries['cubit-baseline'] = None
workloads = ['fixed64', 'small', 'mixed', 'bimodal']
groups = {'branches': '{cycles:u,instructions:u,branches:u,branch-misses:u}',
          'cache': '{cycles:u,L1-dcache-loads:u,L1-dcache-load-misses:u}'}
sources = [p for d in ['userspace/allocator/src', 'userspace/allocator/host']
           for p in sorted((ROOT / d).iterdir()) if p.is_file()]
sources += [ROOT / 'tests/userspace-allocator/bench.rs', pathlib.Path(__file__).resolve(),
            ROOT / 'userspace/allocator/allocator_host.gpr', ROOT / 'flake.lock']
metadata = {'time_utc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
            'cpu': args.cpu, 'iterations': args.iterations, 'repetitions': args.repetitions,
            'sample_event': args.sample_event, 'period': args.period,
            'record_only': args.record_only,
            'libraries': libraries, 'load_average_start': os.getloadavg(),
            'perf_version': subprocess.check_output(['perf', 'version'], text=True).strip(),
            'cpuinfo': (pathlib.Path('/proc/cpuinfo').read_text()),
            'binary_sha256': hashlib.sha256(bench.read_bytes()).hexdigest(),
            'baseline_sha256': hashlib.sha256(baseline.read_bytes()).hexdigest() if baseline else None,
            'source_sha256': {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest()
                              for p in sources},
            'scope': 'FIFO-gated churn only, plus constant handshake overhead; free + malloc + two byte touches + harness. CPU pinned, not isolated. No call stacks.'}
(output / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')


def run(mode, engine, work, label):
    stem = output / f'{engine}-{work}-{label}'
    env = os.environ.copy()
    for name in list(env):
        if name in ['LD_PRELOAD', 'GLIBC_TUNABLES', 'CUBIT_PERF_CONTROL', 'CUBIT_PERF_ACK'] or name.startswith(('MALLOC_', 'MIMALLOC_', 'TCMALLOC_')):
            env.pop(name)
    # Apply the reference preload only to the benchmark, never perf itself.
    child_env = ['env']
    if libraries[engine]:
        child_env.append(f'LD_PRELOAD={libraries[engine]}')
    with tempfile.TemporaryDirectory(prefix='cubit-perf-') as temporary:
        control, ack = [pathlib.Path(temporary) / name for name in ['control', 'ack']]
        for fifo in [control, ack]:
            os.mkfifo(fifo, 0o600)
        child_env += [f'CUBIT_PERF_CONTROL={control}', f'CUBIT_PERF_ACK={ack}']
        command = ['perf', mode, '--delay=-1', f'--control=fifo:{control},{ack}']
        if mode == 'stat':
            command += ['-x', ';', '-e', groups[label.rsplit('-', 1)[0]], '-o', str(stem) + '.stat']
        else:
            command += ['--no-buildid-cache', '-e', args.sample_event, '-c', str(args.period), '-o', str(stem) + '.data']
        command += ['--', 'taskset', '-c', str(args.cpu)] + child_env
        argument = 'cubit-slabs' if engine == 'cubit-baseline' else engine
        executable = baseline if engine == 'cubit-baseline' else bench
        command += [str(executable), argument, work, str(args.iterations)]
        with open(str(stem) + '.stderr', 'w') as error:
            process = subprocess.Popen(command, env=env, stdout=subprocess.PIPE, stderr=error,
                                       text=True, start_new_session=True)
            try:
                stdout, _ = process.communicate(timeout=120)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.communicate()
                raise
        (pathlib.Path(str(stem) + '.json')).write_text(stdout)
        if process.returncode:
            raise SystemExit(f'Failed {stem}; see stderr')
        row = json.loads(stdout)
        assert row['engine'] == argument and row['workload'] == work
        if mode == 'stat':
            # Fail visibly on unsupported counters or multiplexed groups.
            rows = [line.split(';') for line in pathlib.Path(str(stem) + '.stat').read_text().splitlines()
                    if line and not line.startswith('#')]
            expected = groups[label.rsplit('-', 1)[0]].strip('{}').split(',')
            assert [r[2] for r in rows] == expected, rows
            assert all(float(r[0]) >= 0 and float(r[4]) == 100.0 for r in rows), rows
        if mode == 'record':
            report = subprocess.check_output(['perf', 'report', '--stdio', '--no-children',
                                               '--percent-limit', '0.5', '-i', str(stem) + '.data'],
                                              env=env, text=True)
            pathlib.Path(str(stem) + '.report').write_text(report)
    print(f'{mode}: {engine}/{work}/{label}', flush=True)


jobs = [(e, w, f'{g}-{r}') for e in libraries for w in workloads
        for g in groups for r in range(args.repetitions)]
random.Random(42).shuffle(jobs)
if not args.record_only:
    for engine, work, label in jobs:
        run('stat', engine, work, label)
for engine in libraries:
    for work in workloads:
        run('record', engine, work, args.sample_event.split(':')[0])
metadata['load_average_end'] = os.getloadavg()
(output / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
