#!/usr/bin/env python3
"""Run bounded hosted runtime _ERR regressions; no original ASLTS PASS credit."""
from pathlib import Path
import argparse
import hashlib
import json
import os
import re
import subprocess
import time
from build import HERE, build_profile


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def execute(binary, case, output, enabled=True, fuel=5_000_000, tag=1):
    output.mkdir(parents=True)
    table = output/'test.aml'
    table.write_bytes(bytes.fromhex(case['AML_hex']))
    assert digest(table) == case['AML_sha256']
    data = table.read_bytes()
    assert data[:4] == b'DSDT' and int.from_bytes(data[4:8], 'little') == len(data) and sum(data) & 255 == 0
    keys = ['AFTR', 'ECNT', 'STAT', 'ETYP', 'THRD']
    if case['family'] == 'robust':
        keys.append('SEEN')
    env = dict(os.environ, CUBIT_ERR_TEST_ENABLED='1' if enabled else '0',
               CUBIT_ERR_TEST_FUEL=str(fuel), CUBIT_ERR_TEST_TAG=str(tag))
    argv = ['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776', '--stack=134217728',
            binary, str(table), 'TEST', *keys]
    start = time.monotonic()
    log = output/'runtime.log'
    with log.open('w') as stream:
        p = subprocess.run(argv, stdout=stream, stderr=subprocess.STDOUT, timeout=16, env=env)
    text = log.read_text()
    assert p.returncode == 0, (p.returncode, str(log))
    calls = re.findall(r'^PROBE_CALL status=(\S+) charged=\s*(\d+) value=\s*(\S+)$', text, re.M)
    assert len(calls) == 1
    status, charged, value = calls[0]
    obs = {key: int(val) for key, val in re.findall(
        r'^PROBE_OBSERVATION name=(\S+) access=AVAILABLE status=RETURNED value=\s*(\d+)$', text, re.M)}
    assert set(obs) == set(keys)
    errors = [int(v) for v in re.findall(r'^PROBE_EXCEPTION code=\s*(\d+)$', text, re.M)]
    gc = re.findall(r'^PROBE_GC before=\s*(\d+) after=\s*(\d+) freed-before=\s*(\d+) freed-after=\s*(\d+)$', text, re.M)
    assert len(gc) == 1
    before, after, freed_before, freed_after = map(int, gc[0])
    assert int(charged) <= fuel
    assert re.search(r'^PROBE_CLEANUP roots=\s*0 tables=\s*0 bytes=\s*0 reads=\s*0 writes=\s*0$', text, re.M)
    if case['family'] == 'release':
        if fuel <= 8:
            assert status == 'BUDGET_EXCEEDED' and obs['AFTR'] == 0
        elif tag == 0:
            assert status == 'RUNTIME_EXCEPTION' and errors == [0x3016]
            assert obs['AFTR'] == obs['ECNT'] == 0
        elif not enabled:
            assert status == case['default_status'] and not errors
            assert obs == case['default_observations']
            assert value == ('42' if status == 'RETURNED' else 'unavailable')
        else:
            assert status == case['expected_status']
            assert errors == ([] if case['expected_code'] is None else [case['expected_code']])
            assert obs == case['expected_observations']
            assert value == ('42' if status == 'RETURNED' else 'unavailable')
    elif fuel <= 8:
        assert status == 'BUDGET_EXCEEDED' and obs['AFTR'] == obs['ECNT'] == 0
    elif tag == 0:
        assert status == 'RUNTIME_EXCEPTION' and errors == [0x3003]
        assert obs['AFTR'] == obs['ECNT'] == 0
    elif not enabled:
        assert status == 'UNSUPPORTED_VALUE' and not errors and all(v == 0 for v in obs.values())
    elif case['family'] == 'basic':
        continues = case['handler_return'] == 'Zero'
        assert status == ('RETURNED' if continues else 'RUNTIME_EXCEPTION')
        assert value == ('42' if continues else 'unavailable')
        assert errors == ([] if continues else [1 if case['handler_return'] == 'One' else 0x3003])
        for key in ['AFTR', 'ECNT', 'STAT', 'ETYP']:
            assert obs[key] == case['normal_observations'][key][0]
        assert obs['THRD'] == int(case['handler_present'])
    elif case['expected_status'] == 'RESOURCE':
        assert status in ['CALL_LIMIT', 'VALUE_LIMIT'] and not errors
        assert obs['AFTR'] == 0 and obs['ECNT'] > 1
    else:
        assert status == case['expected_status']
        assert errors == ([] if case['expected_code'] is None else [case['expected_code']])
        assert value == ('42' if status == 'RETURNED' else 'unavailable')
        assert obs['AFTR'] == int(status == 'RETURNED')
        assert obs['ECNT'] == 1 and obs['STAT'] == 0x3003 and obs['ETYP'] == 2 and obs['THRD'] == 1
        assert obs['SEEN'] == case['expected_SEEN']
    if case.get('require_collection_during_TEST') and enabled and fuel == 5_000_000 and tag != 0:
        assert after > before and freed_after > freed_before
    return dict(case=case['name'], enabled=enabled, fuel=fuel, tag=tag, status=status,
        charged=int(charged), value=value, observations=obs, exception_codes=errors,
        GC=dict(before=before, after=after, freed_before=freed_before, freed_after=freed_after),
        passed=True, argv=argv, exit=p.returncode, seconds=time.monotonic()-start,
        log=str(log), log_sha256=digest(log), environment_override={key: env[key] for key in
        ['CUBIT_ERR_TEST_ENABLED', 'CUBIT_ERR_TEST_FUEL', 'CUBIT_ERR_TEST_TAG']})


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--root', type=Path, default=HERE.parents[1])
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    output = args.output.resolve()
    if output.exists():
        raise ValueError('Use a fresh output directory: '+str(output))
    output.mkdir(parents=True)
    os.sched_setaffinity(0, os.sched_getaffinity(0)-{14, 15})
    cases = json.loads((HERE/'cases.json').read_text())['cases']
    builds = []
    rows = []
    failure = None
    try:
        # Complete both strict compiler profiles before timed case execution.
        for profile in ['checked', 'release']:
            builds.append(build_profile(args.root.resolve(), output/'build', profile))
        for build in builds:
            folder = output/build['profile']
            for case in cases:
                for enabled in ([False, True] if case['family'] in ['basic', 'release'] else [True]):
                    row = execute(build['binary'], case, folder/(case['name']+('-on' if enabled else '-off')), enabled)
                    rows.append(dict(profile=build['profile'], **row))
            control = next(case for case in cases if case['name'] == 'robust-high_bits_zero')
            for fuel in range(9):
                rows.append(dict(profile=build['profile'], **execute(build['binary'], control, folder/('fuel-'+str(fuel)), fuel=fuel)))
            rows.append(dict(profile=build['profile'], **execute(build['binary'], control, folder/'tag-zero', tag=0)))
            release_control = next(case for case in cases if case['name'] == 'release-unowned_zero')
            for fuel in range(9):
                rows.append(dict(profile=build['profile'], **execute(build['binary'], release_control,
                    folder/('release-fuel-'+str(fuel)), fuel=fuel)))
            rows.append(dict(profile=build['profile'], **execute(build['binary'], release_control, folder/'release-tag-zero', tag=0)))
        assert len(rows) == 120
    except Exception as error:
        failure = repr(error)
    result = dict(terminal=True, failure=failure, planned_cases=120, executed_cases=len(rows),
        passed_cases=len(rows), builds=builds, rows=rows, original_ASLTS_PASS_credit=0,
        original_corpus_or_expectation_edits=0, named_heap=dict(objects=64, bytes=1024, elements=64),
        normal_numeric_thread_ID_parity_claim=False, native=False, full_interpreter_proof_claim=False)
    (output/'RESULT.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(dict(failure=failure, passed_cases=len(rows), planned_cases=120)), flush=True)
    if failure:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
