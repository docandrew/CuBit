#!/usr/bin/env python3
"""Replay the pinned static-count ACPICA observations against the hosted runner."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import resource
import subprocess
import tempfile
import time

HERE = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['release', 'checked'], required=True)
args = parser.parse_args()
reference = HERE / 'reference'
manifest = json.loads((reference / 'manifest.json').read_text())
def verify():
    for name, expected in manifest.items():
        path = reference / name
        if Path(name).name != name or hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise RuntimeError('Reference integrity failure: ' + name)
verify()
observations = json.loads((reference / 'report.json').read_text())
if len(observations) != 38 or sum(row['compile_exit'] == 0 for row in observations) != 36:
    raise RuntimeError('Unexpected reference inventory')
executable = HERE / ('oracle-' + args.mode) / 'static_buffer_oracle_runner'
if not executable.is_file():
    raise RuntimeError('Build the dedicated oracle project first: ' + str(executable))
results_dir = HERE / 'results'
results_dir.mkdir(exist_ok=True)
logs = Path(tempfile.mkdtemp(prefix=args.mode + '-oracle-', dir=results_dir))
report = {'mode': args.mode, 'state': 'running', 'rows': [],
          'executable_sha256': hashlib.sha256(executable.read_bytes()).hexdigest(),
          'reference_manifest_sha256': hashlib.sha256((reference / 'manifest.json').read_bytes()).hexdigest(),
          'scope': 'Cached ACPICA replay, not a fresh ACPICA execution or full conformance test'}
def save():
    (logs / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
def limits():
    resource.setrlimit(resource.RLIMIT_AS, (1024 * 1024 * 1024, 1024 * 1024 * 1024))
    resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024, 64 * 1024 * 1024))
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
print('Oracle logs:', logs, flush=True)
save()
try:
    for ref in observations:
        tag = f"{ref['case']}-{ref['revision']}"
        if ref['compile_exit'] != 0:
            report['rows'].append({'case': tag, 'classification': 'REFERENCE_COMPILER_REJECTED_NOT_RUNTIME_PARITY',
                                   'compile_exit': ref['compile_exit'], 'state': 'recorded'})
            save()
            continue
        aml = reference / (tag + '.aml')
        if aml.name not in manifest:
            raise RuntimeError('AML not present in checked manifest: ' + tag)
        row = {'case': tag, 'state': 'running', 'aml_sha256': manifest[aml.name],
               'command': [str(executable), str(aml)], 'timeout_seconds': 30}
        report['rows'].append(row)
        save()
        started = time.monotonic()
        try:
            process = subprocess.run(row['command'], capture_output=True, text=True,
                                     timeout=30, preexec_fn=limits)
            (logs / (tag + '.stdout')).write_text(process.stdout)
            (logs / (tag + '.stderr')).write_text(process.stderr)
            row['exit_code'] = process.returncode
            policy = ref['case'] == 'b_empty' or (ref['case'] == 's_width' and ref['revision'] == 1)
            if policy:
                expected_status = 'UNSUPPORTED_OPCODE' if ref['case'] == 'b_empty' else 'VALUE_LIMIT'
                actual_status = re.findall(r'^LOAD_STATUS ([A-Z_]+)$', process.stderr, re.M)
                row.update(classification='EXPECTED_BOUNDED_REJECTION_NOT_PARITY',
                           expected_load_status=expected_status, actual_load_status=actual_status,
                           matched=process.returncode > 0 and 'install: INVALID_AML' in process.stderr
                           and actual_status == [expected_status])
            else:
                expected = int(ref['integer_hex'][0], 16)
                actual = re.fullmatch(r'RESULT\s+(\d+)\s*', process.stdout)
                row.update(classification='INTEGER_DIFFERENTIAL', expected=expected,
                           matched=process.returncode == 0 and actual is not None and int(actual[1]) == expected)
            row['state'] = 'passed' if row['matched'] else 'failed'
        except subprocess.TimeoutExpired as exc:
            (logs / (tag + '.stdout')).write_bytes(exc.stdout or b'')
            (logs / (tag + '.stderr')).write_bytes(exc.stderr or b'')
            row.update(state='timeout', exit_code=None, matched=False)
        except OSError as exc:
            row.update(state='launch_failed', exit_code=None, matched=False, error=repr(exc))
        finally:
            row['elapsed_seconds'] = time.monotonic() - started
            save()
    verify()
    if any(row['state'] not in ('passed', 'recorded') for row in report['rows']):
        raise RuntimeError('One or more comparisons failed; see per-case outcomes')
    report['state'] = 'passed'
except BaseException as exc:
    report.update(state='failed', error=repr(exc))
    raise
finally:
    save()
print('STATIC-BUFFER-ORACLE PASS: 33 integer comparisons, 3 bounded policies, 2 recorded compiler rejections')
