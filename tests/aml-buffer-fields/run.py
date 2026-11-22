#!/usr/bin/env python3
"""Portable hosted BufferField milestone tests; invoke inside the Nix shell."""
from pathlib import Path
import argparse
import hashlib
import json
import math
import resource
import shlex
import tempfile
import time
from hosted_capture import capture

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['release', 'checked'], default='release')
parser.add_argument('--test-timeout', type=float, default=180)
parser.add_argument('--skip-main', action='append', default=[])
args = parser.parse_args()
if not math.isfinite(args.test_timeout) or args.test_timeout <= 0:
    parser.error('--test-timeout must be finite and positive')
resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024,
                                         resource.getrlimit(resource.RLIMIT_STACK)[1]))
mode = args.mode
projects = [
    ('aml-buffer-fields', f'execute_{mode}.gpr',
     ['field_execute_tests', 'field_status_tests', 'copy_index_policy_tests']),
    ('aml-buffer-fields', f'{mode}.gpr', ['field_owner_tests']),
    ('aml-buffer-field-integration', f'{mode}.gpr',
     ['field_wrapper_tests', 'field_live_tests', 'field_package_tests',
      'field_package_rollback_tests', 'field_package_compat_tests']),
    ('aml-field-supplement', f'{mode}.gpr',
     ['copy_initializing_tests', 'copy_boundary_extended_tests'])]
known = {name + '.adb' for _, _, mains in projects for name in mains}
if set(args.skip_main) - known:
    parser.error('unknown --skip-main for BufferField group')
result_root = HERE / 'results'
result_root.mkdir(exist_ok=True)
logs = Path(tempfile.mkdtemp(prefix=mode + '-', dir=result_root))
report = {'mode': mode, 'state': 'running', 'outcomes': [],
          'scope': 'Hosted regressions and cached ACPICA comparison; no proof or native validation'}
source_dirs = ['userspace/services/acpi/lib', 'userspace/services/acpi', 'shared/firmware']
source_dirs += ['tests/' + name for name in
    ['aml-buffer-fields', 'aml-buffer-field-integration', 'aml-field-supplement',
     'aml-field-adapters', 'aml-core', 'aml-debug', 'aml-retention',
     'aml-references', 'acpi-integration']]
inputs = {}
for directory in source_dirs:
    for path in (ROOT / directory).rglob('*'):
        relative = path.relative_to(ROOT)
        if any(part.startswith('build-') or part.endswith('-obj') or
               part in ('results', 'reference', '__pycache__') for part in relative.parts):
            continue
        if path.is_file() and path.suffix in ('.adb', '.ads', '.gpr', '.py'):
            inputs[str(relative)] = hashlib.sha256(path.read_bytes()).hexdigest()
group_file = ROOT / 'tests/aml-field-adapters/groups.json'
inputs[str(group_file.relative_to(ROOT))] = hashlib.sha256(group_file.read_bytes()).hexdigest()
reference = HERE / 'reference'
manifest_path = reference / 'reference-manifest.json'
manifest_bytes = manifest_path.read_bytes()
inputs[str(manifest_path.relative_to(ROOT))] = hashlib.sha256(manifest_bytes).hexdigest()
for name, digest in json.loads(manifest_bytes).items():
    path = (reference / name).resolve()
    if not path.is_relative_to(reference.resolve()):
        raise RuntimeError('reference input escapes reference directory')
    if hashlib.sha256(path.read_bytes()).hexdigest() != digest:
        raise RuntimeError('reference input differs before build: ' + name)
    inputs[str(path.relative_to(ROOT))] = digest
(logs / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')

def check_inputs():
    if any(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() != digest
           for name, digest in inputs.items()):
        raise RuntimeError('hosted source inputs changed during run')

def save():
    (logs / 'results.json').write_text(json.dumps(report, indent=2) + '\n')

def run(label, command, timeout=180):
    check_inputs()
    row = {'label': label, 'command': command, 'state': 'running'}
    report['outcomes'].append(row)
    save()
    started = time.monotonic()
    code = capture(command, ROOT, logs / (label + '.log'), timeout)
    row.update(state='passed' if code == 0 else 'failed', exit_code=code,
               elapsed_seconds=time.monotonic() - started)
    save()
    if code != 0:
        raise RuntimeError(f'{label}: {code}')
    check_inputs()
    return row

try:
    run('versions', ['gprbuild', '--version'])
    for directory, project, mains in projects:
        folder = ROOT / 'tests' / directory
        label = directory + '-' + Path(project).stem
        run(label + '-build', ['gprbuild', '-j1', '-p', '-P', str(folder / project)])
        for main in mains:
            if main + '.adb' in args.skip_main:
                report['outcomes'].append({'label': main, 'state': 'skipped',
                                           'reason': 'explicit --skip-main'})
                save()
                continue
            run(main, [str(folder / ('build-' + mode) / main)], args.test_timeout)
    run('reference-protocol-tests', ['python3', '-B', str(reference / 'test_protocol.py')])
    run('cached-acpica-102', ['python3', '-B', str(reference / 'compare.py'),
        '--runner', str(HERE / ('build-' + mode) / 'field_runner'),
        '--output', str(logs / 'reference-results')], 102 * 30 + 60)
    adapters = ROOT / 'tests/aml-field-adapters'
    groups = json.loads((adapters / 'groups.json').read_text())
    if sum(len(group['sources']) for group in groups.values()) != 10:
        raise RuntimeError('expected ten explicit adapters')
    for name, group in groups.items():
        label = 'adapter-' + name
        row = run(label, ['gprbuild', '-j1', '-p', '-c', '-u', '-f', '-v', '-P',
            str(adapters / f'adapter_{name}_{mode}.gpr'),
            *[Path(source).name for source in group['sources']]])
        selected = set()
        for line in (logs / (label + '.log')).read_text().splitlines():
            tokens = shlex.split(line)
            if '-c' in tokens:
                selected.update(token for token in tokens if token.endswith('.adb'))
        expected = {str(ROOT / source) for source in group['sources']}
        row['selected_source_paths'] = sorted(selected)
        if not expected.issubset(selected):
            row['state'] = 'failed'
            raise RuntimeError('adapter compiler source selection mismatch')
        save()
    report['state'] = 'passed'
except BaseException as exc:
    report.update(state='failed', error=repr(exc))
    raise
finally:
    save()
print('Hosted BufferField results:', logs, flush=True)
