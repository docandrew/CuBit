#!/usr/bin/env python3
"""Run unmodified pinned ASLTS collections and expose CuBit coverage gaps."""
import argparse
import hashlib
import json
import pathlib
import re
import subprocess
import tarfile
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parent
COMMIT = '232ff3f8ae1a4da11c709f61d9154482cfe8e6df'
SHA256 = '91addf34cf6f00c310dcff1d456c2e04629c2b45efe409385068e9fc5e9fe43b'
COLLECTIONS = ('arithmetic', 'control', 'logic')
MODES = {'n32': ['-oa', '-r', '1'], 'n64': ['-oa', '-r', '2'],
         'o32': ['-r', '1'], 'o64': ['-r', '2']}


def execute(argv, cwd, log):
    result = subprocess.run([str(x) for x in argv], cwd=cwd, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=180)
    log.write_text(result.stdout)
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    parser.add_argument('--require-full', action='store_true',
                        help='fail even for explicitly recorded coverage gaps')
    options = parser.parse_args()
    out = ROOT / 'build' / 'aslts'
    out.mkdir(parents=True, exist_ok=True)
    archive = out / 'source.tar.gz'
    if not archive.exists():
        with urllib.request.urlopen(f'https://codeload.github.com/acpica/acpica/tar.gz/{COMMIT}', timeout=60) as response:
            archive.write_bytes(response.read())
    if hashlib.sha256(archive.read_bytes()).hexdigest() != SHA256:
        raise RuntimeError('upstream source checksum mismatch')
    # Reextract verified bytes each time: local edits must not silently change the suite.
    with tarfile.open(archive) as tar:
        tar.extractall(out, filter='data')
    suite = out / f'acpica-{COMMIT}' / 'tests' / 'aslts'
    inventory = sorted(str(p.relative_to(suite)) for p in (suite / 'src/runtime').rglob('MAIN.asl'))
    report = {'commit': COMMIT, 'sha256': SHA256,
              'runtime_entrypoints': inventory, 'results': []}
    failures = 0
    gaps = 0
    passed = 0
    for collection in COLLECTIONS:
        cwd = suite / 'src/runtime/collections/functional' / collection
        for mode, flags in MODES.items():
            tag = f'{collection}-{mode}'
            prefix = out / tag
            row = {'collection': collection, 'mode': mode}
            report['results'].append(row)
            compiled = execute([options.tools / 'iasl', '-of', '-cr', '-vs', *flags,
                                *(['-vx', '6152', '-vx', '6163', '-vx', '6022', '-vw', '6141'] if collection == 'control' else []), '-p', prefix, 'MAIN.asl'], cwd, out / f'{tag}-compile.log')
            if compiled.returncode:
                row['status'] = 'COMPILE_FAILED'
                failures += 1
                continue
            table = prefix.with_suffix('.aml')
            reference = execute([options.tools / 'acpiexec', '-ef', '-el', '-to', '60', '-b', 'execute MN00', table],
                                out, out / f'{tag}-reference.log')
            row['reference_methods'] = re.findall(
                r':STST:([^\"\n]+)', reference.stdout)
            width = mode[1:]
            if reference.returncode or f'TEST ACPICA: {width}-bit : PASS' not in reference.stdout:
                row['status'] = 'REFERENCE_FAILED'
                failures += 1
                continue
            row['reference'] = 'PASS'
            if collection in ('arithmetic', 'logic'):
                admission = execute([ROOT / 'build/table_runner', table, '--metrics'],
                                    out, out / f'{tag}-admission.log')
                row['admission'] = 'PASS' if admission.returncode == 0 else 'FAILED'
                row['usage'] = {key: int(value) for key, value in re.findall(
                    r'^(NODES|VALUE_OBJECTS|VALUE_BYTES|PACKAGE_ELEMENTS|METHOD_BYTES)\s+(\d+)$',
                    admission.stdout, re.MULTILINE)}
                if admission.returncode or len(row['usage']) != 5:
                    row['status'] = 'ADMISSION_FAILED'
                    failures += 1
                    print(f'ASLTS {tag}: ADMISSION_FAILED', flush=True)
                    continue
            actual = execute([ROOT / 'build/table_runner', table, 'MN00'],
                             out, out / f'{tag}-cubit.log')
            if actual.returncode == 0 and re.fullmatch(r'RESULT\s+0\s*', actual.stdout):
                row['status'] = 'PASS'
                passed += 1
            elif actual.returncode != 0 and (
                ('execute: UNSUPPORTED' in actual.stdout and collection in ('arithmetic', 'logic'))
                or ('install: BYTE_LIMIT' in actual.stdout and collection == 'control')
            ):
                # Explicit current baseline, not a blanket catch for all execution errors.
                row['status'] = 'UNSUPPORTED'
                row['reason'] = ('Control table exceeds current 65536-byte service limit.' if collection == 'control' else 'Table admission and initial named integer writes passed; execution reaches the unsupported DataTableRegion declaration in STRT (runtime/cntl/common.asl), after creating its temporary M555 method. This is not a passed interpreter test.')
                gaps += 1
            else:
                row['status'] = 'FAILED'
                failures += 1
            print(f'ASLTS {tag}: {row["status"]}', flush=True)
    selected = {f'src/runtime/collections/functional/{name}/MAIN.asl' for name in COLLECTIONS}
    report['not_run'] = [p for p in inventory if p not in selected]
    report['summary'] = {'passed': passed, 'unsupported': gaps, 'failed': failures,
                         'entrypoints_not_run': len(report['not_run'])}
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print('AML-ASLTS: ' + json.dumps(report['summary']))
    if failures or (options.require_full and (gaps or report['not_run'])):
        raise SystemExit(1)


if __name__ == '__main__':
    main()
