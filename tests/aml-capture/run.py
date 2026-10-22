#!/usr/bin/env python3
"""Bounded hosted capture regression; no native builds or oracle regeneration."""
from pathlib import Path
import argparse, hashlib, json, os, resource, shutil, signal, sys, tempfile, time
from hosted_capture import capture, CaptureInterrupted
from source_guards import inventory, compiler_lines, ali_source
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['release', 'checked'], default='release')
parser.add_argument('--selection', choices=['capture', 'legacy', 'startup'], default='capture')
args = parser.parse_args()
assert os.environ.get('TMPDIR') == '/home/doc/cubit-build-tmp'
resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024,) * 2)
if os.getpriority(os.PRIO_PROCESS, 0) < 19:
    os.nice(19 - os.getpriority(os.PRIO_PROCESS, 0))
def interrupted(signum, frame):
    raise CaptureInterrupted('Capture validation interrupted')
signal.signal(signal.SIGTERM, interrupted)
signal.signal(signal.SIGINT, interrupted)
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
manifest_file = HERE / 'source-manifest.json'
manifest_digest = sha(manifest_file)
manifest = json.loads(manifest_file.read_text())
references = json.loads((HERE / 'reference-manifest.json').read_text())
closure = json.loads((HERE / 'closure.json').read_text())[args.selection]
# All paths are repository-relative; reject traversal and changed/symlinked inputs.
def verify_files(items):
    for rel, digest in items.items():
        path = ROOT / rel
        assert not Path(rel).is_absolute() and '..' not in Path(rel).parts
        assert not path.is_symlink() and path.resolve().is_relative_to(ROOT)
        assert sha(path) == digest, rel
verify_files(manifest)
verify_files(references)
pinned_sources = inventory(ROOT, closure['sources'])
log_root = HERE / 'results'
log_root.mkdir(exist_ok=True)
OUT = Path(tempfile.mkdtemp(prefix=args.selection + '-' + args.mode + '-', dir=log_root))
BUILD = OUT / 'build'
project = HERE / (args.selection + '_' + args.mode + '.gpr')
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
rows, binaries, compiled = [], {}, []
print(OUT, flush=True)
def verify():
    assert sha(manifest_file) == manifest_digest
    inventory(ROOT, closure['sources'])
    verify_files(manifest)
    verify_files(references)
    for tool in tools.values():
        assert sha(Path(tool['path'])) == tool['sha256']
    for item in runtime.values():
        assert sha(Path(item['path'])) == item['sha256']
    for name, digest in binaries.items():
        assert sha(Path(name)) == digest

def stage(label, command, limit, binary=None):
    if binary:
        binaries[str(binary)] = sha(binary)
    verify()
    start = time.monotonic()
    status = capture(command, ROOT, OUT / (label + '.log'), limit)
    rows.append({'stage': label, 'command': command, 'status': status,
                 'seconds': time.monotonic() - start, 'limit': limit})
    (OUT / 'report.json').write_text(json.dumps(rows, indent=2) + '\n')
    print(label, status, flush=True)
    verify()
    if status != 0:
        raise SystemExit(1)
try:
    for main in closure['mains']:
        label = Path(main).stem
        stage(label + '-build', [tools['gprbuild']['path'], '-v', '-p', '-j1',
              '-P', str(project), '-XCAPTURE_BUILD=' + str(BUILD), main], 180)
        paths = compiler_lines((OUT / (label + '-build.log')).read_text(),
            tools['ada-gcc']['path'], pinned_sources, BUILD / 'obj', closure['mains'])
        assert str(pinned_sources[main]) in paths
        compiled.extend(paths)
        binary = BUILD / 'bin' / Path(main).stem
        stage(label, [str(binary)], 30, binary)
    ali_dependencies = {}
    selected_units = set()
    for ali in (BUILD / 'obj').glob('*.ali'):
        local = []
        for line in ali.read_text().splitlines():
            if not line.startswith('D '):
                continue
            unit = line.split()[1]
            kind, path = ali_source(unit, pinned_sources, BUILD / 'obj', closure['mains'], runtime)
            if kind == 'local':
                assert sha(path) == closure['sources'][unit]['sha256'] == manifest[str(path.relative_to(ROOT))]
                selected_units.add(unit)
            local.append({'unit': unit, 'kind': kind, 'path': str(path), 'sha256': sha(path), 'ali_entry': line})
        ali_dependencies[ali.name] = {'sha256': sha(ali), 'local': local}
    required = {'aml_execute.adb', 'aml_execute-scalar_transport.adb', 'aml_namespace.adb'}
    if args.selection == 'startup':
        required |= {'acpi_bootstrap.adb', 'acpi_requests.adb', 'acpi_service_core.adb'}
    assert required <= selected_units
    for unit in required:
        assert str(pinned_sources[unit]) in compiled
    (OUT / 'ali-dependencies.json').write_text(json.dumps(ali_dependencies, indent=2) + '\n')
finally:
    verify()
    (OUT / 'audit.json').write_text(json.dumps({'selection': args.selection, 'mode': args.mode,
        'inputs': manifest, 'source_manifest_sha256': manifest_digest, 'references': references,
        'tools': tools, 'runtime_sources': runtime, 'binaries': binaries,
        'artifacts': {str(p.relative_to(OUT)): sha(p) for p in OUT.rglob('*')
                      if p.is_file() and p.name != 'audit.json'},
        'assumptions': 'Pinned Nix toolchain; temporary generated GNAT configs are not retained.'}, indent=2) + '\n')
