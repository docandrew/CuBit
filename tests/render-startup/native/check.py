#!/usr/bin/env python3
"""Check production startup transitions and actual software-child identities."""
import re
import sys
from pathlib import Path

def check(text):
    def count(marker, expected):
        actual = text.count(marker)
        assert actual == expected, (marker, actual, expected)
    count('procmgr: render admission denied; child not resumed', 3)
    count('procmgr: render admission submitted; child suspended', 2)
    count('procmgr: failed launch child stop requested', 4)
    count('procmgr: render retry software with fresh child', 1)
    count('procmgr: render software-only admitted', 2)
    for name in ('render-denied', 'render-unavailable', 'render-occupied', 'render-invalid'):
        count('procmgr: init spawn failed: ' + name + '.app', 1)
    for name in ('render-software', 'render-fallback'):
        count('procmgr: init spawn failed: ' + name + '.app', 0)
    assert 'devices: native window ready' in text
    assert not re.search(r'TEST: FAIL|cleanup rejected|KILL: denied|render admission complete', text)
    attempts = re.findall(r'procmgr: render attempt incarnation=\s*(\d+) software=(TRUE|FALSE)', text)
    assert len(attempts) == 5, attempts
    hardware = {int(i) for i, software in attempts if software == 'FALSE'}
    software = [int(i) for i, sw in attempts if sw == 'TRUE']
    # Two software observations, both accepted. render-occupied asks for a
    # slot number (CAP-002: refused at load), so it makes no attempt at all.
    assert len(hardware) == 3 and len(software) == 2
    running = [int(i) for i in re.findall(r'RENDER-STARTUP: software child incarnation=\s*(\d+)', text)]
    assert len(running) == 2 and len(set(running)) == 2
    admitted = []
    last = None
    for line in text.splitlines():
        match = re.search(r'procmgr: render attempt incarnation=\s*(\d+) software=(TRUE|FALSE)', line)
        if match:
            last = (int(match[1]), match[2])
        if 'procmgr: render software-only admitted' in line:
            assert last is not None and last[1] == 'TRUE'
            admitted.append(last[0])
    assert len(admitted) == 2 and set(running) == set(admitted)
    assert not set(running) & hardware, 'failed GPU incarnation executed'
    assert len({int(i) for i, _ in attempts}) == 5, 'incarnation reused'
    assert all(i >> 32 and i & 0xffffffff for i in running)
    print('RENDER-STARTUP: PASS native optional startup; fresh software children, no render entry, bounded retry, slot requests refused and mandatory denial')

if __name__ == '__main__':
    check(Path(sys.argv[1]).read_text(errors='replace'))
