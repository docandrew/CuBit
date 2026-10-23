#!/usr/bin/env python3
"""CAP-003: fail on any refusal in a headless run's serial log.

The startup-health and logs tests check their required markers (every
service registered, records in logstore); this checks the other half: that
nothing was refused on the way. A refusal is a declaration that fell short -
a startup entry not granted, a Config key outside the caller's scopes, a
publication without its badge - which used to be silent (docs/
declaration-checking.md section 5). Prints each finding; exits 1 if any.
"""
import re
import sys

# Each pattern, and what it means.
REFUSALS = [
    (r': Warning: refused ', 'a refusal (CuBit.Refusal_Notes: config, logstore, procmgr)'),
    (r'CAP-003 startup check', "a program's generated startup check stopped it"),
    (r'a capability it requires was denied', 'procmgr could not grant a required startup entry'),
    (r'refused: startup table', 'procmgr refused a launch for its startup table'),
    (r'init spawn failed', 'a program in the startup profile did not start'),
    (r'boot image unavailable', 'a program devmgr starts is missing from the image'),
    (r'PUBLISH: denied', 'a registration notification published by the wrong process'),
    (r'config scopes not installed|refused the declared config scopes',
     "devmgr could not install a service's declared Config scopes"),
]


def main():
    if len(sys.argv) != 2:
        print('usage: check-refusals.py SERIAL_LOG', file=sys.stderr)
        return 2
    with open(sys.argv[1], 'rb') as log:
        lines = log.read().decode('utf-8', 'replace').splitlines()
    findings = 0
    for number, line in enumerate(lines, 1):
        for pattern, meaning in REFUSALS:
            if re.search(pattern, line):
                print(f'refusals: line {number}: {meaning}: {line.strip()}')
                findings += 1
    if findings:
        print(f'refusals: FAIL, {findings} refusal(s)')
        return 1
    print('refusals: PASS, no refusals')
    return 0


if __name__ == '__main__':
    sys.exit(main())
