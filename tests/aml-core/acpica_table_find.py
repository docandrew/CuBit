#!/usr/bin/env python3
"""Compare retained-table selection with ACPICA's DataTableRegion lookup.

CuBit's candidate invokes Find_Table directly, not AML DataTableRegion/Field.
This is a focused lookup comparison, never claimed as opcode or ASLTS coverage.
"""
import argparse
import itertools
import pathlib
import re
from acpica_compare import ROOT, run

parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=pathlib.Path, required=True)
tools = parser.parse_args().tools
out = ROOT / 'build' / 'acpica-table-find'
out.mkdir(parents=True, exist_ok=True)
cases = list(itertools.product(('DSDT', 'NONE'), ('', 'ABCDEF', 'ZZZZZZ'), ('', 'TABLE001', 'OTHER001')))
checks = 0
for revision in (1, 2):
    source = out / f'find-{revision}.asl'
    methods = []
    for i, (signature, oem, table_id) in enumerate(cases):
        methods.append(f'Method (M{i:03}, 0, Serialized) {{ '
                       f'DataTableRegion (RGN0, "{signature}", "{oem}", "{table_id}") '
                       'Field (RGN0, ByteAcc, NoLock, Preserve) { Offset (8), REV0, 8 } '
                       'Return (REV0) }')
    source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "ABCDEF", "TABLE001", 1) {{\n'
                      + '\n'.join(methods) + '\n}\n')
    (out / f'{revision}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
    table = source.with_suffix('.aml')
    for i, selectors in enumerate(cases):
        reference = run([tools / 'acpiexec', '-b', f'execute M{i:03}', table])
        (out / f'{revision}-{i}-reference.log').write_text(reference)
        values = re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', reference)
        if values:
            if len(values) != 1 or int(values[0], 16) != revision or 'AE_NOT_FOUND' in reference:
                raise RuntimeError('ambiguous reference success')
            expected = 1
        else:
            if 'AE_NOT_FOUND' not in reference:
                raise RuntimeError('missing expected table lookup result')
            expected = 0
        actual = run([ROOT / 'build' / 'table_find_runner', table, *selectors])
        (out / f'{revision}-{i}-actual.log').write_text(actual)
        if actual.strip() != f'MATCH {expected}':
            raise RuntimeError(f'{revision}/{selectors}: {actual!r}, expected {expected}')
        checks += 1
print(f'ACPI-ACPICA-TABLE-FIND: PASS {checks}')
