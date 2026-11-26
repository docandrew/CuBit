#!/usr/bin/env python3
"""Check Unload operand effects, typed refusals and cleanup; no table teardown."""
from pathlib import Path
from datetime import datetime
import argparse
import json
import re
import subprocess
import tempfile

from build import HERE, build_profile, sha


def package(opcode, payload):
    for following in range(4):
        extent = len(payload)+following+1
        if extent < (64 if following == 0 else 1 << (4+8*following)):
            if following == 0:
                length = bytes([extent])
            else:
                length = bytes([(following << 6) | (extent & 15)]) + (extent >> 4).to_bytes(following, 'little')
            return bytes([opcode])+length+payload
    raise ValueError('Diagnostic package exceeds its encoding bound')


def table(spec, case):
    body = bytes.fromhex(spec['module_prefix_hex'])
    body += package(0x14, b'TEST'+bytes([0])+bytes.fromhex(case['TEST_body_hex']))
    header = bytearray.fromhex(spec['header_hex'])
    if len(header) != 36:
        raise ValueError('Expected one synthetic DSDT header')
    header[4:8] = (len(header)+len(body)).to_bytes(4, 'little')
    header[9] = 0
    data = header+body
    data[9] = (-sum(data)) & 255
    return bytes(data)


def run(root, output):
    if (output/'RESULT.json').exists():
        raise ValueError('Existing runtime receipt retained; choose another output directory')
    output.mkdir(parents=True, exist_ok=True)
    spec = json.loads((HERE/'cases.json').read_text())
    rows = []
    builds = []
    failure = None
    try:
        # All compilers finish before the first wall-time-limited case runs.
        builds = [build_profile(root, output/'build', mode)for mode in ['checked', 'release']]
        for build in builds:
            for case in spec['cases']:
                folder = output/build['profile']/case['name']
                folder.mkdir(parents=True)
                aml = folder/'test.aml'
                aml.write_bytes(table(spec, case))
                if sha(aml.read_bytes()) != case['AML_sha256']:
                    raise ValueError('Synthetic AML drift: '+case['name'])
                log = folder/'runtime.log'
                argv = ['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776', '--stack=134217728',
                        build['binary'], str(aml), 'TEST', 'AFTR', 'BEFO']
                with log.open('w') as stream:
                    process = subprocess.run(argv, stdout=stream, stderr=subprocess.STDOUT, timeout=16)
                text = log.read_text(errors='replace')
                calls = re.findall(r'^PROBE_CALL status=(\S+) charged=\s*(\d+) value=(\S+)$', text, re.M)
                observed = {name: int(value)for name, value in re.findall(
                    r'^PROBE_OBSERVATION name=(\S+) access=AVAILABLE status=RETURNED value=\s*(\d+)$', text, re.M)}
                cleanup = bool(re.search(r'^PROBE_CLEANUP roots=\s*0 tables=\s*0 bytes=\s*0 reads=\s*0 writes=\s*0$', text, re.M))
                passed = (process.returncode == 0 and len(calls) == 1 and calls[0][0] == case['expected_status']
                          and observed == {'AFTR': 0, 'BEFO': case['expected_BEFO']} and cleanup)
                row = dict(profile=build['profile'], case=case['name'], argv=argv, exit=process.returncode,
                           passed=passed, status=calls[0][0] if len(calls) == 1 else None, observations=observed,
                           cleanup_roots_and_IO_zero=cleanup, AML_sha256=sha(aml.read_bytes()),
                           binary_sha256=build['binary_sha256'], log=str(log), log_sha256=sha(log.read_bytes()))
                rows.append(row)
                print(build['profile'], case['name'], 'PASS' if passed else 'FAIL', flush=True)
                if not passed:
                    raise RuntimeError('Regression failed: '+build['profile']+'/'+case['name']+'; '+str(log))
    except Exception as error:
        failure = repr(error)
    result = dict(time=datetime.now().astimezone().isoformat(), terminal=True, failure=failure,
                  planned_cases=2*len(spec['cases']), passed_cases=sum(row['passed']for row in rows),
                  executed_cases=len(rows), builds=builds, rows=rows,
                  scope='Linux-hosted Unload operand consumption/refusal/effects/cleanup only',
                  original_ASLTS_PASS_credit=0, native_execution=False, proof_credit=0,
                  general_ERR_continuation=False, authenticated_table_teardown=False)
    (output/'RESULT.json').write_text(json.dumps(result, indent=2)+'\n')
    print('Receipt:', output/'RESULT.json', flush=True)
    if failure:
        raise RuntimeError(failure)
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--root', type=Path, default=HERE.parents[1])
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    output = args.output.resolve() if args.output else Path(tempfile.mkdtemp(prefix='acpi-unload-consumption-'))
    run(args.root.resolve(), output)
