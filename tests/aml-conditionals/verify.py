#!/usr/bin/env python3
"""Verify cached normal ACPICA evidence and embedded synthetic test bytecode."""
from pathlib import Path
import hashlib, json, re
HERE = Path(__file__).resolve().parent
ref = HERE / 'reference'
manifest = json.loads((ref / 'manifest.json').read_text())
sha = lambda b: hashlib.sha256(b).hexdigest()
for name, expected in manifest.items():
    if Path(name).name != name or sha((ref / name).read_bytes()) != expected:
        raise RuntimeError('Reference integrity failure: ' + name)
for name, expected in json.loads((ref / 'fixture-sources.json').read_text()).items():
    if Path(name).name != name or sha((HERE / name).read_bytes()) != expected:
        raise RuntimeError('Fixture identity failure: ' + name)
fixtures = json.loads((ref / 'fixtures.json').read_text())
pattern = r'Test\("([^"]+)",\s*\[([^]]*)\],\s*(\d+),\s*(\w+),\s*(\d+),\s*(\d+),\s*Bits_(32|64)\);'
calls = re.findall(pattern, (HERE / 'tail_tests.adb').read_text())
if len(fixtures) != 40 or len(calls) != 40:
    raise RuntimeError('Unexpected fixture count')
outcomes = {r['name']: r for r in json.loads((ref / 'original-outcomes.json').read_text())}
normal = 0
for index, (call, fixture) in enumerate(zip(calls, fixtures)):
    name, raw, arg, status, value, budget, width = call
    literals = re.findall(r'16#([0-9A-Fa-f]{2})#', raw)
    if re.sub(r'16#[0-9A-Fa-f]{2}#|\s|,', '', raw):
        raise RuntimeError('Unexpected embedded byte expression')
    data = bytes(int(n, 16) for n in literals)
    actual = dict(name=name, sha256=sha(data), argument=int(arg), status=status,
                  expected=int(value), budget=int(budget), revision=1 if width == '32' else 2)
    if actual != fixture:
        raise RuntimeError('Embedded fixture metadata mismatch: ' + name)
    if index < 22:
        aml_name = name + '.aml'
        if aml_name not in manifest:
            raise RuntimeError('Missing normal AML input')
        table = (ref / aml_name).read_bytes()
        if len(table) <= 36 or table[:4] != b'DSDT' or int.from_bytes(table[4:8], 'little') != len(table) or sum(table) % 256 or table[8] != actual['revision']:
            raise RuntimeError('Invalid cached table framing: ' + name)
        if table[36:] != data:
            raise RuntimeError('Cached AML differs from invoked fixture: ' + name)
        for phase in ['compile', 'oracle']:
            if outcomes[name + '-' + phase]['returncode'] != 0:
                raise RuntimeError('Reference command did not succeed: ' + name)
        text = (ref / (name + '-oracle.out')).read_text() + (ref / (name + '-oracle.err')).read_text()
        values = re.findall(r'\[Integer\] = ([0-9A-Fa-f]+)', text)
        if len(values) != 1 or int(values[0], 16) != actual['expected'] or status != 'Returned':
            raise RuntimeError('Reference result differs: ' + name)
        normal += 1
if normal != 22:
    raise RuntimeError('Unexpected normal reference count')
print('VERIFIED 22 cached ACPICA correspondences + 18 synthetic fixture identities; boundary source hash exact')
