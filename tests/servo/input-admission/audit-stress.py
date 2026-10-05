#!/usr/bin/env python3
"""Audit native Penny stress artifacts; distinguish survival from input reliability."""
import argparse
import json
from pathlib import Path
import re


def audit(serial, interaction, expected):
    submitted = re.findall(r'^CUBITSHELL-BROWSER: submitted (.*)$', serial, re.M)
    faults = [s for s in ('USER-MEMORY-FAULT', 'PENNY-ABORT:', 'CUBITSHELL: panic',
                          'CUBITSHELL: FAIL') if s in serial]
    resync = [int(n) for n in re.findall(r'\binput_resync=(\d+)', serial)]
    paints = [int(n) for n in re.findall(r'PENNY-FRAME: render=true paint_ms=(\d+)', serial)]
    recovery = interaction.get('recovery_attempts')
    survival = interaction.get('result') == 'PASS' and not faults
    addresses = submitted == expected
    complete = bool(expected) and isinstance(recovery, list) and bool(resync) and bool(paints)
    return {
        'survival_pass': survival,
        'exact_addresses_pass': addresses,
        'evidence_complete': complete,
        'input_reliability_pass': survival and addresses and complete and not recovery and max(resync) == 0,
        'recovery_attempts': recovery,
        'max_reported_input_resync': max(resync, default=None),
        'max_paint_guest_wall_ms': max(paints, default=None),
        'fault_markers': faults,
        'expected_addresses': expected,
        'submitted_addresses': submitted,
        'scope': 'Native VM functional evidence; guest wall times are not CPU measurements. No general crash-freedom or memory-leak claim.',
    }


def self_test():
    log = ('CUBITSHELL-BROWSER: submitted https://example.org/\n'
           'desktop: stats input_resync=0\nPENNY-FRAME: render=true paint_ms=1050\n')
    expected = ['https://example.org/']
    state = {'result': 'PASS', 'recovery_attempts': []}
    assert audit(log, state, expected)['input_reliability_pass']
    assert not audit(log.replace('example.org', 'exmple.org'), state, expected)['exact_addresses_pass']
    assert not audit(log + log, state, expected)['exact_addresses_pass']
    assert not audit(log, dict(state, recovery_attempts=[0]), expected)['input_reliability_pass']
    assert not audit(log.replace('input_resync=0', 'input_resync=1'), state, expected)['input_reliability_pass']
    assert not audit(log + 'PENNY-ABORT:', state, expected)['survival_pass']
    assert not audit(log.replace('input_resync=0', ''), state, expected)['evidence_complete']
    assert not audit(log, {'result': 'PASS'}, expected)['evidence_complete']
    assert not audit(log, state, [])['evidence_complete']
    print('PASS auditor rejects truncated/duplicate addresses, recoveries, resync, faults and incomplete evidence')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('run', type=Path, nargs='?')
    parser.add_argument('--expected', type=Path, help='JSON list of exact submitted URLs in order')
    parser.add_argument('--require-no-recovery', action='store_true')
    parser.add_argument('--self-test', action='store_true')
    args = parser.parse_args()
    if args.self_test:
        self_test()
    else:
        if args.run is None or args.expected is None:
            parser.error('run and --expected are required')
        expected = json.loads(args.expected.read_text())
        if not isinstance(expected, list) or not expected or not all(isinstance(x, str) for x in expected):
            parser.error('--expected must contain a nonempty JSON string list')
        result = audit((args.run / 'serial.log').read_text(errors='replace'),
                       json.loads((args.run / 'interaction.json').read_text()), expected)
        print(json.dumps(result, indent=2))
        passed = result['survival_pass'] and result['exact_addresses_pass'] and result['evidence_complete']
        if args.require_no_recovery:
            passed = passed and result['input_reliability_pass']
        raise SystemExit(0 if passed else 1)
