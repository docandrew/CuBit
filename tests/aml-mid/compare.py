#!/usr/bin/env python3
"""Strict cached Mid result-tree and post-execution marker comparison."""
import argparse, hashlib, json, resource, subprocess
from pathlib import Path


def parse_value(lines, depth=0):
    if depth > 64 or not lines:
        raise ValueError('Missing/deep value')
    line = lines.pop(0)
    if line.startswith('INTEGER '):
        text = line[8:].strip()
        if not text.isascii() or not text.isdecimal():
            raise ValueError('Invalid integer')
        value = int(text)
        if value > 2**64-1:
            raise ValueError('Wide integer')
        return {'kind': 'integer', 'value': value}
    if line.startswith('STRING '):
        return {'kind': 'string', 'hex': line[7:].encode('ascii').hex()}
    if line.startswith('BUFFER'):
        fields = line.split()
        if fields[0] != 'BUFFER':
            raise ValueError('Invalid buffer prefix')
        return {'kind': 'buffer', 'hex': bytes(int(v) for v in fields[1:]).hex()}
    if line.startswith('PACKAGE '):
        text = line[8:].strip()
        if not text.isascii() or not text.isdecimal() or int(text) > 8192:
            raise ValueError('Invalid package count')
        return {'kind': 'package', 'items': [parse_value(lines, depth+1) for _ in range(int(text))]}
    raise ValueError('Unexpected value line')


def classify(expected, code, output):
    if code != 0:
        return False
    lines = output.splitlines()
    if not lines or lines.pop(0) != 'STATUS ' + expected['status']:
        return False
    try:
        if 'result' in expected and parse_value(lines) != expected['result']:
            return False
        return lines == ['SEEN ' + str(expected['marker'])]
    except (ValueError, UnicodeError, IndexError):
        return False


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def limits():
    resource.setrlimit(resource.RLIMIT_AS, (1024**3,)*2)
    resource.setrlimit(resource.RLIMIT_STACK, (64*1024*1024,)*2)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runner', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    ref = Path(__file__).resolve().parent / 'reference'
    manifest_file = ref / 'manifest.json'
    manifest_hash = digest(manifest_file)
    manifest = json.loads(manifest_file.read_text())
    def verify():
        if digest(manifest_file) != manifest_hash:
            raise ValueError('Manifest changed')
        for name, wanted in manifest.items():
            file = ref / name
            if not file.resolve().is_relative_to(ref.resolve()) or digest(file) != wanted:
                raise ValueError('Reference changed: ' + name)
    verify()
    cases = json.loads((ref / 'cases.json').read_text())
    if len(cases) != 62 or len({c['key'] for c in cases}) != 62:
        raise ValueError('Case inventory')
    runner = args.runner.resolve()
    before = digest(runner)
    args.output.mkdir(parents=True, exist_ok=False)
    rows = []
    for case in cases:
        row = {'key': case['key']}
        if case['classification'] == 'compiler_rejection':
            row['classification'] = 'compiler-rejected-not-run'
            rows.append(row)
            continue
        if case['classification'] not in ('returned', 'AE_AML_OPERAND_TYPE', 'AE_AML_BUFFER_LIMIT'):
            raise ValueError('Unexpected classification')
        aml = ref / case['aml']
        if case['aml'] not in manifest:
            raise ValueError('Unmanifested input')
        data = aml.read_bytes()
        if len(data) < 36 or data[:4] != b'DSDT' or data[8] != case['revision'] or int.from_bytes(data[4:8], 'little') != len(data) or sum(data) % 256:
            raise ValueError('Invalid AML table')
        try:
            process = subprocess.run([str(runner), str(aml), 'TEST'], capture_output=True,
                                     text=True, timeout=30, preexec_fn=limits)
            row.update(returncode=process.returncode, stdout=process.stdout, stderr=process.stderr,
                       passed=classify(case, process.returncode, process.stdout))
        except subprocess.TimeoutExpired as exc:
            row.update(passed=False, timeout=True, partial_stdout=repr(exc.stdout), partial_stderr=repr(exc.stderr))
        except OSError as exc:
            row.update(passed=False, launch_error=repr(exc))
        rows.append(row)
        (args.output / (case['key']+'.json')).write_text(json.dumps(row, indent=2)+'\n')
    verify()
    if digest(runner) != before:
        raise ValueError('Runner changed')
    executed = sum('passed' in r for r in rows)
    rejected = sum(r.get('classification') == 'compiler-rejected-not-run' for r in rows)
    failed = sum(r.get('passed') is False for r in rows)
    report = dict(runner=str(runner), runner_sha256=before, manifest_sha256=manifest_hash,
                  executed=executed, compiler_rejected=rejected, failed=failed, rows=rows)
    (args.output / 'report.json').write_text(json.dumps(report, indent=2)+'\n')
    print(f'Compared {executed}; compiler rejected {rejected}; failed {failed}')
    return 0 if executed == 56 and rejected == 6 and failed == 0 else 1


if __name__ == '__main__':
    raise SystemExit(main())
