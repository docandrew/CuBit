#!/usr/bin/env python3
"""Replay pinned ACPICA observations without private workspace dependencies."""
import argparse
import hashlib
import json
from pathlib import Path
import resource
import subprocess
import tempfile


def classify(expected, returncode, stdout):
    lines = stdout.splitlines()
    statuses = [x for x in lines if x.startswith('STATUS')]
    payloads = [x for x in lines if x.startswith(
        ('BUFFER', 'INTEGER', 'REFERENCE', 'PACKAGE', 'STRING'))]
    if returncode != 0:
        return False
    if expected['classification'] == 'runtime_error':
        return statuses == ['STATUS UNSUPPORTED_VALUE'] and not payloads
    if expected['kind'] == 'buffer':
        if statuses != ['STATUS OBJECT_RETURNED'] or len(payloads) != 1:
            return False
        fields = payloads[0].split()
        if not fields or fields[0] != 'BUFFER':
            return False
        try:
            data = bytes(int(x) for x in fields[1:])
        except ValueError:
            return False
        return len(data) == expected['length'] and data.hex() == expected['hex']
    return (statuses == ['STATUS RETURNED']
            and payloads == ['INTEGER ' + str(expected['integer'])])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mode', choices=['release', 'checked'], required=True)
    parser.add_argument('--runner', type=Path)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    directory = Path(__file__).resolve().parent
    reference = directory / 'reference'
    manifest_bytes = (reference / 'manifest.json').read_bytes()
    manifest_sha256 = hashlib.sha256(manifest_bytes).hexdigest()
    manifest = json.loads(manifest_bytes)
    for relative, expected in manifest['files'].items():
        path = reference / relative
        if not path.resolve().is_relative_to(reference.resolve()):
            raise ValueError('Reference path escapes bundle: ' + relative)
        if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise ValueError('Reference hash mismatch: ' + relative)
    runner = (args.runner or directory / ('build-' + args.mode) / 'to_buffer_runner').resolve()
    runner_sha256 = hashlib.sha256(runner.read_bytes()).hexdigest()
    if args.output:
        output = args.output.resolve()
        output.mkdir(parents=True, exist_ok=False)
    else:
        results = directory / 'results'
        results.mkdir(exist_ok=True)
        output = Path(tempfile.mkdtemp(prefix=args.mode + '-', dir=results))
    resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024, resource.getrlimit(resource.RLIMIT_STACK)[1]))
    resource.setrlimit(resource.RLIMIT_AS, (1024**3, resource.getrlimit(resource.RLIMIT_AS)[1]))
    rows = []
    for group in manifest['groups']:
        observations = json.loads((reference / group['observations']).read_text())
        for expected in observations:
            key = expected['key']
            row = {'group': group['directory'], 'key': key}
            if expected['compile_returncode'] != 0:
                row['classification'] = 'compiler-rejected-not-run'
                rows.append(row)
                continue
            case = reference / group['directory'] / 'cases' / (key + '.aml')
            data = case.read_bytes()
            if hashlib.sha256(data).hexdigest() != expected['aml_sha256']:
                raise ValueError('Observation AML hash mismatch: ' + str(case))
            if len(data) < 36 or int.from_bytes(data[4:8], 'little') != len(data) or sum(data) % 256:
                raise ValueError('Invalid ACPI table header/checksum: ' + str(case))
            if expected['classification'] == 'runtime_error':
                errors = json.loads((reference / group['directory'] / 'exact-errors.json').read_text())
                if errors.get(key) != 'AE_AML_OPERAND_TYPE':
                    raise ValueError('Unrecognized reference error: ' + key)
            elif expected['classification'] != 'returned' or expected['kind'] not in ('buffer', 'integer'):
                raise ValueError('Unrecognized observation: ' + key)
            try:
                process = subprocess.run([str(runner), str(case), 'TEST'], text=True,
                                         capture_output=True, timeout=30)
                row.update(exit_code=process.returncode, stdout=process.stdout, stderr=process.stderr,
                           passed=classify(expected, process.returncode, process.stdout))
            except subprocess.TimeoutExpired:
                row.update(passed=False, timeout=True)
            rows.append(row)
            (output / (group['directory'] + '-' + key + '.json')).write_text(json.dumps(row, indent=2) + '\n')
    executed = sum('passed' in row for row in rows)
    rejected = sum(row.get('classification') == 'compiler-rejected-not-run' for row in rows)
    failures = sum(row.get('passed') is False for row in rows)
    unchanged = (hashlib.sha256(runner.read_bytes()).hexdigest() == runner_sha256
                 and hashlib.sha256((reference / 'manifest.json').read_bytes()).hexdigest() == manifest_sha256
                 and all(hashlib.sha256((reference / path).read_bytes()).hexdigest() == expected
                         for path, expected in manifest['files'].items()))
    report = {'mode': args.mode, 'runner': str(runner), 'runner_sha256': runner_sha256,
              'reference_manifest_sha256': manifest_sha256, 'inputs_unchanged': unchanged, 'executed': executed,
              'compiler_rejected': rejected, 'failed': failures, 'rows': rows}
    (output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(f'Compared {executed}; compiler rejected {rejected}; failed {failures}; logs {output}', flush=True)
    return 0 if unchanged and executed == 88 and rejected == 2 and failures == 0 else 1


if __name__ == '__main__':
    raise SystemExit(main())
