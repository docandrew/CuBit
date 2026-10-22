#!/usr/bin/env python3
"""Synthetic protocol/mismatch checks only; no AML or ACPICA execution."""
import hashlib
import json
from pathlib import Path
import byte_protocol as protocol
import compare

BASE = Path(__file__).resolve().parent
checks = 0

def check(condition):
    global checks
    checks += 1
    if not condition:
        raise AssertionError(f'Check {checks} failed')

def encode(expected):
    out = 'STATUS ' + expected['status'] + '\n'
    result = expected['result']
    if result:
        if result['kind'] == 'integer':
            out += 'INTEGER ' + str(result['value']) + '\n'
        else:
            out += 'BUFFER' + ''.join(' '+str(x) for x in bytes.fromhex(result['hex'])) + '\n'
    return (out + 'MARK ' + str(expected['marker']) + '\n').encode('ascii')

def invalid(data):
    try:
        protocol.parse(data)
    except (ValueError, UnicodeError):
        check(True)
    else:
        check(False)

cases = json.loads((BASE/'cases.json').read_text())
excluded = json.loads((BASE/'excluded.json').read_text())
check(len(cases) == 102)
check(len(excluded) == 6)
check(sum(r['classification']=='returned' for r in cases) == 74)
check(sum(r['classification']=='runtime_error' for r in cases) == 28)
check(sum(r['classification']=='inconclusive' for r in excluded) == 2)
check(sum(r['classification']=='normal_compiler_syntax_rejection' for r in excluded) == 4)
check(not any('HUGE' in r['key'] for r in cases))
check(len({r['key'] for r in cases+excluded}) == 108)
for row in cases:
    expected = compare.expected(row)
    data = encode(expected)
    check(protocol.parse(data) == expected)
    check(compare.classify(row,0,data))
    check(not compare.classify(row,1,data))
    check(not compare.classify(row,-9,data))
    check(not compare.classify(row,'timeout',data))
    wrong = dict(expected,marker=expected['marker']+1)
    check(not compare.classify(row,0,encode(wrong)))
    for bad in [b'',b'garbage\n'+data,data+b'garbage\n',data+b'\n',data+b'MARK 0\n',
                data.replace(b'\n',b'\r\n'),data.replace(b'MARK ',b'MARK -'),
                data.replace(b'MARK ',b'MARK +'),data.replace(b'MARK ',b'MARK \xff'),
                data.replace(b'MARK ',b'MARK \x00'),
                data.replace(b'MARK ',b'MARK 18446744073709551616'),
                data.replace(b'STATUS ',b'STATUS FAKE_'),data[:data.rfind(b'MARK ')],
                data.replace(b'STATUS ',b'STATUS \xe2\x80\xa8')]:
        check(not compare.classify(row,0,bad))
    if expected['result']:
        wrong = dict(expected)
        if expected['result']['kind']=='integer':
            wrong['result']={'kind':'integer','value':expected['result']['value'] ^ 1}
        else:
            raw=bytes.fromhex(expected['result']['hex'])
            wrong['result']={'kind':'buffer','hex':(bytes([raw[0]^1])+raw[1:]).hex() if raw else '00'}
        check(not compare.classify(row,0,encode(wrong)))
    else:
        check(not compare.classify(row,0,b'STATUS RETURNED\nINTEGER 0\nMARK '+str(expected['marker']).encode()+b'\n'))

for bad in [b'STATUS RETURNED\nBUFFER 1\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\nINTEGER 1\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\nBUFFER 256\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\nBUFFER -1\nMARK 0\n',
            b'STATUS RETURNED\nINTEGER 18446744073709551616\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\nPACKAGE 8192\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\n'+b'PACKAGE 1\n'*65+b'INTEGER 0\nMARK 0\n',
            b'STATUS OBJECT_RETURNED\nBUFFER'+b' 0'*65537+b'\nMARK 0\n',
            b'\n'*16385,b'x'*(protocol.MAX_OUTPUT_BYTES+1),
            'STATUS RETURNED\nINTEGER 1\nMARK 0\n']:
    invalid(bad)
for status in ('INVALID_FIELD_LENGTH','BUFFER_FIELD_LIMIT','DUPLICATE_NAME','EMPTY_BUFFER'):
    check(protocol.parse(('STATUS '+status+'\nMARK 123\n').encode()) == {'status':status,'result':None,'marker':123})
check(protocol.parse(b'STATUS OBJECT_RETURNED\nPACKAGE 2\nINTEGER 1\nBUFFER 0 255\nMARK 2\n') ==
      {'status':'OBJECT_RETURNED','result':{'kind':'package','items':[{'kind':'integer','value':1},{'kind':'buffer','hex':'00ff'}]},'marker':2})
check(len(protocol.parse(b'STATUS OBJECT_RETURNED\nBUFFER'+b' 255'*65536+b'\nMARK 0\n')['result']['hex'])==131072)
check(protocol.parse(b'STATUS RETURNED\nINTEGER 18446744073709551615\nMARK 18446744073709551615\n')['marker']==2**64-1)
manifest=json.loads((BASE/'reference-manifest.json').read_text())
for relative,digest in manifest.items():
    check(hashlib.sha256((BASE/relative).read_bytes()).hexdigest()==digest)
print(json.dumps({'checks':checks,'cases':102,'returned':74,'runtime_errors':28,
                  'excluded_compile_rejections':4,'excluded_inconclusive':2,
                  'reference_inputs_verified':len(manifest),
                  'scope':'synthetic protocol and cached evidence integrity only; no AML execution'}))
