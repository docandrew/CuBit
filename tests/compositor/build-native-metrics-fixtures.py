#!/usr/bin/env python3
"""Build native metrics fixtures with explicit runtime provenance in private output.

Run in the pinned Nix environment. Source and platform inputs must be stable
snapshots or protected by the shared build lock. This builds fixtures only;
it does not boot, stage an image, validate hardware, or rebuild the runtime.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source-root', type=Path, required=True)
parser.add_argument('--platform-root', type=Path, required=True,
                    help='matched built runtime, C startup object and manifest compiler')
parser.add_argument('--toolchain-root', type=Path, required=True,
                    help='CuBit checkout with kernel Alire environment')
parser.add_argument('--output', type=Path, required=True, help='new private output directory')
parser.add_argument('--transfer-observer', action='store_true',
                    help='use software-fallback byte-metadata/zero-transfer observer')
parser.add_argument('--workload-observer', action='store_true',
                    help='byte-metadata observer with explicit producer-loss accounting')
parser.add_argument('--profiling-observer', action='store_true',
                    help='require completion-dispatch and timing-on diagnostic-output samples')
args = parser.parse_args()
if sum((args.transfer_observer, args.workload_observer, args.profiling_observer)) > 1:
    parser.error('Select only one observer variant')
if not os.environ.get('IN_NIX_SHELL'):
    raise SystemExit('Run inside the pinned CuBit Nix environment')
source, platform, toolchain, output = [p.resolve() for p in
    (args.source_root, args.platform_root, args.toolchain_root, args.output)]
output.mkdir(parents=True, exist_ok=False)
work = output / 'platform'
seed = output / 'seed'
seed.mkdir()
inputs = {}
commands = []

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def record(path):
    inputs[str(path)] = digest(path)

def copy(path, target):
    data = path.read_bytes()
    inputs[str(path)] = hashlib.sha256(data).hexdigest()
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)

record(Path(__file__).resolve())
for path in sorted((platform / 'userspace/runtime').rglob('*')):
    if path.is_file():
        copy(path, work / path.relative_to(platform))
for rel in ('userspace/c/link.ld', 'userspace/c/build/crt0.o'):
    copy(platform / rel, work / rel)
fixtures = (
    ('userspace/services/metricsvc', 'metricsvc.gpr', 'metrics.svc'),
    ('tests/compositor/' + ('metrics-profiling-observer' if args.profiling_observer else 'metrics-workload-observer' if args.workload_observer else 'metrics-transfer-observer' if args.transfer_observer else 'metrics-observer'), 'observer.gpr', 'desktop-metrics-observer.app'),
    ('tests/compositor/metrics-stall', 'stall.gpr', 'desktop-metrics-stall.svc'),
)
for folder, project, binary in fixtures:
    for path in sorted((source / folder).iterdir()):
        if path.is_file() and path.suffix in ('.ads', '.adb', '.gpr', '.ccl'):
            copy(path, work / folder / path.name)
compiler = platform / 'userspace/ccl/build/manifest/ccl-manifest'
catalog = platform / 'userspace/ccl/catalogs/native-runtime-services.ccl'
schema = platform / 'userspace/ccl/interfaces/executable-manifest.ccl'
for path in (compiler, catalog, schema):
    record(path)
(output / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
(output / 'result.json').write_text(json.dumps({'status': 'INCOMPLETE'}) + '\n')

def run(argv, cwd, stdout=None):
    argv = list(map(str, argv))
    commands.append({'argv': argv, 'cwd': str(cwd)})
    (output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
    subprocess.run(argv, cwd=cwd, stdout=stdout, check=True)

for folder, project, binary in fixtures:
    directory = work / folder
    build = directory / 'build'
    (build / 'generated').mkdir(parents=True)
    with (build / 'manifest.S').open('w') as out:
        run([compiler, catalog, directory / 'manifest.ccl', '--schema', schema,
             '--ada-output', build / 'generated/ccl_manifest_bindings.ads'], directory, out)
    run(['alr', 'exec', '--', 'gcc', '-c', build / 'manifest.S', '-o', build / 'manifest.o'],
        toolchain / 'kernel')
    run(['alr', 'exec', '--', 'gprbuild', '-p', '-P', directory / project], toolchain / 'kernel')
    shutil.copy2(build / binary, seed / binary)
for path, expected in inputs.items():
    if digest(Path(path)) != expected:
        raise SystemExit('Input changed during build: ' + path)
(output / 'result.json').write_text(json.dumps({
    'status': 'BUILD_PASS', 'scope': 'Native fixture build only; runtime reused from explicit platform',
    'source_root': str(source), 'platform_root': str(platform),
    'binaries': {p.name: digest(p) for p in sorted(seed.iterdir())}
}, indent=2) + '\n')
print(seed)
