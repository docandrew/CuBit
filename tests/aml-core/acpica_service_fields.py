#!/usr/bin/env python3
"""Compare service AML field evaluation; Field declarations execute AML; DataTableRegion still uses the binding API."""
import argparse
import itertools
import json
from pathlib import Path
from acpica_compare import ROOT, run
from acpica_typed import objects

parser = argparse.ArgumentParser()
parser.add_argument('--tools', required=True, type=Path)
tools = parser.parse_args().tools
out = ROOT / 'build/acpica-service-fields'
out.mkdir(parents=True, exist_ok=True)
cases = list(itertools.product((291, 295, 296), (1, 31, 32, 33, 63, 64, 65, 127, 8192)))
checks = 0
for revision in (1, 2):
    source = out / f'fields-{revision}.asl'
    methods = [f'Method (M{i:03}, 0, Serialized) {{ '
               'DataTableRegion (RGN0, "DSDT", "", "") '
               f'Field (RGN0, ByteAcc, NoLock, Preserve) {{ , {offset}, VAL0, {bits} }} '
               'Return (VAL0) }' for i, (offset, bits) in enumerate(cases)]
    source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "FIELDEVA", 1) {{\n'
                      'Name (PADD, Buffer (2048) {1,2,3,4,5,6,7,8})\n'
                      + '\n'.join(methods) + '\n}\n')
    (out / f'compile-{revision}.log').write_text(run([tools / 'iasl', '-oa', source]))
    table = source.with_suffix('.aml')
    for i, (offset, bits) in enumerate(cases):
        reference = run([tools / 'acpiexec', '-b', f'execute M{i:03}', table])
        (out / f'{revision}-{i}-reference.log').write_text(reference)
        expected = objects(reference)
        if len(expected) != 1:
            raise RuntimeError(f'Expected one result: {expected}')
        actual = run([ROOT / 'build/service_field_runner', table, revision, offset, bits]).strip()
        (out / f'{revision}-{i}-actual.log').write_text(actual + '\n')
        if actual != expected[0]:
            raise RuntimeError(f'{revision}/{offset}/{bits}: {actual!r} != {expected[0]!r}')
        checks += 1
(out / 'report.json').write_text(json.dumps({'checks': checks, 'scope': 'service evaluator and Field opcode; region via API'}, indent=2))
print(f'ACPI-ACPICA-SERVICE-FIELDS: PASS {checks}')
