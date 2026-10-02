#!/usr/bin/env python3
"""ACPICA field execution vs CuBit's namespace-bound immutable-table bit reader.

This does not claim CuBit DataTableRegion or Field opcode execution.
"""
import argparse
import itertools
import pathlib
import re
from acpica_compare import ROOT, run

parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=pathlib.Path, required=True)
tools = parser.parse_args().tools
out = ROOT / 'build' / 'acpica-field-data'
out.mkdir(parents=True, exist_ok=True)
cases = list(itertools.product((0, 1, 7, 8, 31, 63, 64, 79, 80, 95), (1, 7, 8, 9, 31, 32, 63, 64)))
source = out / 'fields.asl'
methods = []
for i, (offset, count) in enumerate(cases):
    reserved = f', {offset},' if offset else ''
    methods.append(f'Method (M{i:03}, 0, Serialized) {{ '
                   'DataTableRegion (RGN0, "DSDT", "", "") '
                   f'Field (RGN0, ByteAcc, NoLock, Preserve) {{ {reserved} VAL0, {count} }} '
                   'Return (VAL0) }')
source.write_text('DefinitionBlock ("", "DSDT", 2, "CUBIT", "BITREAD", 1) {\n'
                  + '\n'.join(methods) + '\n}\n')
(out / 'compile.log').write_text(run([tools / 'iasl', '-oa', source]))
table = source.with_suffix('.aml')
checks = 0
for start in range(0, len(cases), 8):
    batch = cases[start:start + 8]
    reference = run([tools / 'acpiexec', '-b', ';'.join(f'execute M{i:03}' for i in range(start, start + len(batch))), table])
    (out / f'{start}-reference.log').write_text(reference)
    values = re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', reference)
    if len(values) != len(batch):
        raise RuntimeError('missing/ambiguous reference integer')
    for (offset, count), expected in zip(batch, values):
        actual = run([ROOT / 'build' / 'field_data_runner', table, offset, count])
        (out / f'{offset}-{count}-actual.log').write_text(actual)
        if actual.strip() != f'VALUE {int(expected, 16)}':
            raise RuntimeError(f'{offset}/{count}: {actual!r} != {expected}')
        checks += 1
print(f'ACPI-ACPICA-TABLE-BITS: PASS {checks}')
