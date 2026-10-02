#!/usr/bin/env python3
"""Compare field framing with iASL output; this does not execute CuBit fields."""
import argparse
import pathlib
from acpica_compare import ROOT, run

parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=pathlib.Path, required=True)
tools = parser.parse_args().tools
out = ROOT / 'build' / 'acpica-fields'
out.mkdir(parents=True, exist_ok=True)
checks = 0
for bits in (1, 8, 63, 64, 255, 4095, 4096, 65535):
    source = out / f'field-{bits}.asl'
    source.write_text('DefinitionBlock ("", "DSDT", 2, "CUBIT", "FIELDS", 1) {\n'
        'OperationRegion (RGN0, SystemMemory, 0, 0x10000)\n'
        'Field (RGN0, AnyAcc, NoLock, Preserve) {\n'
        f'FLD0, {bits}, , 8, AccessAs (ByteAcc), FLD1, 8\n'
        '} }\n')
    (out / f'{bits}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
    table = source.with_suffix('.aml').read_bytes()
    # Exactly one FieldOp in these compiler fixtures. Its package ends at EOF.
    if table.count(b'\x5b\x81') != 1:
        raise RuntimeError('ambiguous FieldOp')
    start = table.index(b'\x5b\x81') + 2
    first = table[start]
    following = first >> 6
    extent = first & (15 if following else 63)
    for i in range(following):
        extent += table[start + 1 + i] << (4 + i * 8)
    body = start + following + 1
    if start + extent != len(table) or table[body:body + 5] != b'RGN0\x00':
        raise RuntimeError('unexpected Field package framing')
    data = out / f'field-{bits}.bin'
    data.write_bytes(table[body + 5:start + extent])
    actual = run([ROOT / 'build' / 'field_runner', data]).splitlines()
    expected = [f'NAMED_FIELD FLD0 {bits} 0 0 0', 'RESERVED_FIELD ____ 8 0 0 0',
                'ACCESS_FIELD ____ 0 1 0 0', 'NAMED_FIELD FLD1 8 0 0 0']
    (out / f'{bits}-actual.log').write_text('\n'.join(actual) + '\n')
    if actual != expected:
        raise RuntimeError(f'{bits}: {actual!r} != {expected!r}')
    checks += len(expected)
print(f'AML-IASL-FIELD-FRAMING: PASS {checks}')
