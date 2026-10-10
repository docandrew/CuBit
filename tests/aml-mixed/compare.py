#!/usr/bin/env python3
"""Replay the frozen mixed-comparison ACPICA observations; no fresh oracle run."""
from pathlib import Path
import argparse, hashlib, json, re, resource, subprocess, tempfile, time
HERE = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['release', 'checked'], required=True)
args = parser.parse_args()
reference = HERE / 'reference'
manifest = json.loads((reference / 'manifest.json').read_text())
def verify():
    for name, expected in manifest.items():
        if Path(name).name != name or hashlib.sha256((reference / name).read_bytes()).hexdigest() != expected:
            raise RuntimeError('Reference integrity failure: ' + name)
verify()
cases = json.loads((reference / 'cases.json').read_text())
if len(cases) != 60 or len({c['case'] for c in cases}) != 60 or sum(c['expected_status'] == 'EMPTY_BUFFER' for c in cases) != 2:
    raise RuntimeError('Unexpected reference case inventory')
exe = HERE / ('build-' + args.mode) / 'mixed_oracle_runner'
exe_hash = hashlib.sha256(exe.read_bytes()).hexdigest()
base = HERE / 'results'
base.mkdir(exist_ok=True)
logs = Path(tempfile.mkdtemp(prefix=args.mode + '-', dir=base))
report = {'state': 'running', 'mode': args.mode, 'rows': [], 'executable_sha256': exe_hash,
          'manifest_sha256': hashlib.sha256((reference / 'manifest.json').read_bytes()).hexdigest(),
          'scope': '58 cached scalar differential outcomes plus 2 explicit empty-buffer errors; not full ASLTS'}
def save():
    (logs / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
def limits():
    resource.setrlimit(resource.RLIMIT_AS, (1024**3, 1024**3))
    resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024**2, 64 * 1024**2))
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
print('Comparison logs:', logs, flush=True)
save()
try:
    for c in cases:
        if c['aml'] not in manifest or Path(c['aml']).name != c['aml']:
            raise RuntimeError('Unmanifested AML input')
        row = dict(case=c['case'], classification=c['classification'], state='running',
                   expected_status=c['expected_status'], expected_integer=c['expected_integer'],
                   aml_sha256=manifest[c['aml']], timeout_seconds=30)
        report['rows'].append(row)
        save()
        started = time.monotonic()
        try:
            result = subprocess.run([str(exe), str(reference / c['aml']), 'TEST'],
                                    capture_output=True, timeout=30, preexec_fn=limits)
            (logs / (c['case'] + '.stdout')).write_bytes(result.stdout)
            (logs / (c['case'] + '.stderr')).write_bytes(result.stderr)
            text = result.stdout.decode('utf-8', errors='replace')
            statuses = re.findall(r'^STATUS ([A-Z_]+)$', text, re.M)
            numbers = re.findall(r'^INTEGER\s+(\d+)$', text, re.M)
            expected_numbers = [] if c['expected_integer'] is None else [str(c['expected_integer'])]
            match = result.returncode == 0 and statuses == [c['expected_status']] and numbers == expected_numbers
            row.update(exit_code=result.returncode, actual_status=statuses, actual_integer=numbers,
                       state='passed' if match else 'failed')
        except subprocess.TimeoutExpired as exc:
            (logs / (c['case'] + '.stdout')).write_bytes(exc.stdout or b'')
            (logs / (c['case'] + '.stderr')).write_bytes(exc.stderr or b'')
            row.update(state='timeout', exit_code=None)
        finally:
            row['elapsed_seconds'] = time.monotonic() - started
            save()
    verify()
    if hashlib.sha256(exe.read_bytes()).hexdigest() != exe_hash:
        raise RuntimeError('Runner changed during replay')
    if any(r['state'] != 'passed' for r in report['rows']):
        raise RuntimeError('One or more selected comparison cases failed')
    report['state'] = 'passed'
except BaseException as exc:
    report.update(state='failed', error=repr(exc))
    raise
finally:
    save()
print('Matched 60 cached classifications (58 values, 2 expected errors)', flush=True)
