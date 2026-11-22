#!/usr/bin/env python3
"""Compare synchronous serialized method ordering with pinned ACPICA."""
import argparse
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=Path, required=True)
tools = parser.parse_args().tools
out = ROOT / 'build' / 'acpica-serialized'
out.mkdir(exist_ok=True)

def run(argv, log):
    result = subprocess.run([str(x) for x in argv], text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=60)
    log.write_text(result.stdout)
    return result

cases = [(parent, child, bridge) for parent in range(16)
         for child in range(16) for bridge in (False, True)]
checks = 0
for revision in (1, 2):
    for base in range(0, len(cases), 24):
        declarations = []
        names = []
        expected = {}
        for index, (parent, child, bridge) in enumerate(cases[base:base + 24], base):
            leaf, middle, name = f'C{index:03}', f'B{index:03}', f'M{index:03}'
            names.append(name)
            expected[name] = parent > child
            declarations += [
                f'Method ({leaf}, 0, Serialized, {child}) {{ Return (One) }}',
                f'Method ({middle}, 0, NotSerialized, 15) {{ Return ({leaf} ()) }}',
                f'Method ({name}, 0, Serialized, {parent}) {{ Return ({middle if bridge else leaf} ()) }}',
            ]
        declarations += [
            'Method (HIGH, 0, Serialized, 7) { Return (One) }',
            'Method (LOW0, 0, Serialized, 2) { Return (One) }',
            'Method (REST, 0, Serialized, 1) { HIGH () Return (LOW0 ()) }',
            'Method (RECU, 1, Serialized, 5) { If (Arg0) { Return (RECU (Subtract (Arg0, One))) } Return (One) }',
            'Method (RTRY, 0) { Return (RECU (12)) }',
            'Method (DOWN, 0, Serialized, 1) { Return (UP00 ()) }',
            'Method (UP00, 0, Serialized, 2) { Return (DOWN ()) }',
        ]
        names += ['REST', 'RTRY', 'DOWN', 'LOW0']
        expected.update(REST=False, RTRY=False, DOWN=True, LOW0=False)
        source = out / f'{revision}-{base}.asl'
        source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "SERIAL", 1) {{\n'
                          + '\n'.join(declarations) + '\n}\n')
        compiled = run([tools / 'iasl', '-oa', source], source.with_suffix('.compile.log'))
        if compiled.returncode:
            raise RuntimeError(compiled.stdout)
        table = source.with_suffix('.aml')
        reference = run([tools / 'acpiexec', '-b', ';'.join(f'execute {name}' for name in names), table],
                        source.with_suffix('.reference.log'))
        if reference.returncode:
            raise RuntimeError(reference.stdout)
        parts = re.split(r'^Evaluating \\(M[0-9]{3}|REST|RTRY|DOWN|LOW0)\s*$', reference.stdout, flags=re.MULTILINE)
        if parts[1::2] != names:
            raise RuntimeError(('evaluation inventory', parts[1::2], names))
        for name, block in zip(parts[1::2], parts[2::2]):
            actual = run([ROOT / 'build' / 'table_runner', table, name], out / f'{revision}-{base}-{name}.actual.log')
            if expected[name]:
                if 'AE_AML_MUTEX_ORDER' not in block or actual.returncode == 0 or 'execute: MUTEX_ORDER' not in actual.stdout:
                    raise RuntimeError((revision, name, block, actual.stdout))
            else:
                values = re.findall(r'\[Integer\] = ([0-9A-Fa-f]+)', block)
                if len(values) != 1 or int(values[0], 16) != 1 or actual.returncode or actual.stdout.strip() != 'RESULT 1':
                    raise RuntimeError((revision, name, block, actual.stdout))
            checks += 1
print('AML-ACPICA-SERIALIZED: PASS', checks)
