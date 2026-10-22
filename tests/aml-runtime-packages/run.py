#!/usr/bin/env python3
"""Bounded opt-in runtime package, capacity and adapter checks; no oracle regeneration."""
import sys
if not __debug__: raise SystemExit("Python optimization disables required guards")
from pathlib import Path
import argparse, hashlib, json, os, resource, shutil, signal, sys, tempfile, time
from hosted_capture import capture, CaptureInterrupted
from source_guards import inventory, compiler_lines, ali_source
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
p = argparse.ArgumentParser()
p.add_argument('--mode', choices=['release', 'checked'], default='release')
p.add_argument('--validate-only', action='store_true')
p.add_argument('--selection', nargs='+', metavar='LABEL', help='Explicit unique closure labels; default: all 25')
a = p.parse_args()
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
manifest_file = HERE / 'source-manifest.json'
manifest_digest = sha(manifest_file)
manifest = json.loads(manifest_file.read_text())
references = json.loads((HERE / 'reference-manifest.json').read_text())
closures = json.loads((HERE / 'closure.json').read_text())
selected = list(closures) if a.selection is None else a.selection
if not selected or any(not label for label in selected):
    p.error('--selection must contain nonempty labels')
if len(selected) != len(set(selected)):
    p.error('--selection labels must be unique')
unknown = sorted(set(selected) - set(closures))
if unknown:
    p.error('unknown selection labels: ' + ', '.join(unknown))
def verify_files(items):
    for rel, digest in items.items():
        path = ROOT / rel
        assert not Path(rel).is_absolute() and '..' not in Path(rel).parts
        assert not path.is_symlink() and path.resolve().is_relative_to(ROOT)
        assert sha(path) == digest, rel

def verify_inputs():
    assert sha(manifest_file) == manifest_digest
    verify_files(manifest)
    verify_files(references)
    for c in closures.values():
        inventory(ROOT, c['sources'])
verify_inputs()
assert len(closures) == 25
if a.validate_only:
    print('All 25 source inventories and source/reference pins verified; selected: ' + ', '.join(selected) + '; no compile.')
    raise SystemExit(0)
assert os.environ.get('TMPDIR') == '/home/doc/cubit-build-tmp'
resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024,) * 2)
if os.getpriority(os.PRIO_PROCESS, 0) < 19:
    os.nice(19 - os.getpriority(os.PRIO_PROCESS, 0))
def interrupted(signum, frame):
    raise CaptureInterrupted('Runtime package validation interrupted')
signal.signal(signal.SIGTERM, interrupted)
signal.signal(signal.SIGINT, interrupted)
log_root = HERE / 'results'
log_root.mkdir(exist_ok=True)
OUT = Path(tempfile.mkdtemp(prefix=a.mode + '-', dir=log_root))
tools = {}
for name in ['gprbuild', 'gnatbind', 'gnatlink']:
    path = Path(shutil.which(name)).resolve(strict=True)
    tools[name] = {'path': str(path), 'sha256': sha(path)}
path = Path(shutil.which('gnatbind')).parent / 'gcc'
tools['ada-gcc'] = {'path': str(path), 'sha256': sha(path)}
path = Path(sys.executable).resolve(strict=True)
tools['python'] = {'path': str(path), 'sha256': sha(path)}
runtime_log = OUT / 'runtime-directory.log'
assert capture([tools['ada-gcc']['path'], '-print-file-name=adainclude'], ROOT, runtime_log, 5) == 0
runtime_dir = Path(runtime_log.read_text().strip())
assert runtime_dir.is_absolute() and runtime_dir == runtime_dir.resolve(strict=True)
assert str(runtime_dir).startswith('/nix/store/')
runtime = {p.name: {'path': str(p), 'sha256': sha(p)} for p in runtime_dir.iterdir() if p.suffix in ['.ads', '.adb']}
assert runtime and 'system.ads' in runtime
rows, binaries, dependencies = [], {}, {}
print(OUT, flush=True)
def verify():
    verify_inputs()
    for item in list(tools.values()) + list(runtime.values()):
        assert sha(Path(item['path'])) == item['sha256']
    for name, digest in binaries.items():
        assert sha(Path(name)) == digest

def stage(label, command, limit):
    verify()
    start = time.monotonic()
    status = capture(command, ROOT, OUT / (label + '.log'), limit)
    rows.append({'stage': label, 'command': command, 'status': status, 'seconds': time.monotonic() - start, 'limit': limit})
    (OUT / 'report.json').write_text(json.dumps(rows, indent=2) + '\n')
    print(label, status, flush=True)
    verify()
    if status != 0:
        raise SystemExit(1)
try:
    for label in selected:
        c = closures[label]
        build = OUT / label
        pinned = inventory(ROOT, c['sources'])
        main = c['main']
        project = HERE / (label + '_' + a.mode + '.gpr')
        stage(label + '-build', [tools['gprbuild']['path'], '-v', '-p', '-j1'] + (['-c', '-u'] if c['kind'] == 'semantic' else []) + ['-P', str(project), '-XRUNTIME_PACKAGE_BUILD=' + str(build), main], 180)
        paths = compiler_lines((OUT / (label + '-build.log')).read_text(), tools['ada-gcc']['path'], pinned, build / 'obj', [main])
        assert str(pinned[main]) in paths
        deps = {}
        for ali in (build / 'obj').glob('*.ali'):
            entries = []
            for line in ali.read_text().splitlines():
                if line.startswith('D '):
                    unit = line.split()[1]
                    kind, path = ali_source(unit, pinned, build / 'obj', [main], runtime)
                    entries.append({'unit': unit, 'kind': kind, 'path': str(path), 'sha256': sha(path), 'ali_entry': line})
            deps[ali.name] = {'sha256': sha(ali), 'dependencies': entries}
        assert deps
        dependencies[label] = {'compiled': paths, 'ALI': deps}
        (OUT / 'ali-dependencies.json').write_text(json.dumps(dependencies, indent=2) + '\n')
        if c['kind'] == 'runtime':
            binary = build / 'bin' / Path(main).stem
            binaries[str(binary)] = sha(binary)
            for invocation in c['runs']:
                stage(label + '-' + invocation['label'], [str(binary), *invocation['args']], 30)
finally:
    verify()
    (OUT / 'audit.json').write_text(json.dumps({'mode': a.mode, 'selection': selected, 'registered_closures': list(closures), 'inputs': manifest, 'source_manifest_sha256': manifest_digest, 'references': references, 'tools': tools, 'runtime_sources': runtime, 'binaries': binaries, 'artifacts': {str(p.relative_to(OUT)): sha(p) for p in OUT.rglob('*') if p.is_file() and p.name != 'audit.json'}, 'assumptions': 'Pinned Nix toolchain; generated temporary GNAT configurations not retained.'}, indent=2) + '\n')
