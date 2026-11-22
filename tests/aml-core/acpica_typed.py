#!/usr/bin/env python3
"""Compare actual method-returned object trees and typed consumers with ACPICA."""
import argparse
import pathlib
import re
from acpica_compare import ROOT, run


def objects(output):
    result = []
    remaining = 0
    buffer = []
    for line in output.splitlines():
        if match := re.fullmatch(r'\s*\[Buffer\] Length ([0-9A-Fa-f]+) =(.*)', line):
            remaining = int(match[1], 16)
            buffer = []
            line = match[2]
            if remaining == 0:
                result.append('BUFFER')
                continue
            if not line.strip():
                continue
        if remaining:
            match = re.fullmatch(r'\s*[0-9A-Fa-f]+:\s*((?:[0-9A-Fa-f]{2}\s+)+)\s*//.*', line)
            if not match:
                raise RuntimeError(f'Unrecognized buffer bytes: {line}')
            values = [int(x, 16) for x in match[1].split()]
            if len(values) > remaining:
                raise RuntimeError('Excess buffer bytes')
            buffer.extend(values)
            remaining -= len(values)
            if not remaining:
                result.append('BUFFER' + ''.join(f' {v}' for v in buffer))
        elif match := re.fullmatch(r'\s*\[Package\] Contains (\d+) Elements:', line):
            result.append(f'PACKAGE {int(match[1])}')
        elif match := re.fullmatch(r'\s*\[Integer\] = ([0-9A-Fa-f]+)', line):
            result.append(f'INTEGER {int(match[1], 16)}')
        elif match := re.fullmatch(r'\s*\[String\] Length [0-9A-Fa-f]+ = "([^"]*)"', line):
            result.append('STRING ' + match[1])
        elif re.fullmatch(r'\s*\[Null Object\] \(Type=0\)', line):
            result.append('NULL')
        elif re.match(r'\s*\[(Package|Integer|String|Null Object|Buffer)\]', line):
            raise RuntimeError(f'Unrecognized object: {line}')
    if remaining or not result:
        raise RuntimeError('Missing or incomplete reference objects')
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', required=True, type=pathlib.Path)
    tools = parser.parse_args().tools
    out = ROOT / 'build' / 'acpica-typed'
    out.mkdir(parents=True, exist_ok=True)
    checks = 0
    initializers = ['"1F"', '""', '"123456789ABCDEF0"',
                    'Buffer (0) {}', 'Buffer (1) {255}',
                    'Buffer (8) {1,2,3,4,5,6,7,8}',
                    'Buffer (20) {0,255,1,128,32,64,7,6,5,4,3,2,1}',
                    'Package (4) {One, "A", Buffer (2) {255, 1}}',
                    'Package (2) {Package (2) {Ones, "NEST"}, Zero}']
    for revision in (1, 2):
        declarations = [
            'Method (IDEN, 1) { Return (Arg0) }',
            'Method (LCAL, 1) { Store (Arg0, Local0) Return (Local0) }',
            'Method (TYP0, 1) { Return (ObjectType (Arg0)) }',
            'Method (SIZ0, 1) { Return (SizeOf (Arg0)) }',
            'Method (ADD0, 1) { Return (Add (Arg0, One)) }']
        methods = {}
        for i, initializer in enumerate(initializers):
            name = f'V{i:03}'
            declarations.append(f'Name ({name}, {initializer})')
            bodies = [f'Return ({name})', f'Return (LCAL (IDEN ({name})))',
                      f'Return (TYP0 ({name}))', f'Return (SIZ0 ({name}))',
                      f'Store ({name}, Local7) Return (Local7)',
                      f'Store (One, Local3) Store ({name}, Local3) Return (Local3)',
                      f'Store ({name}, Local3) Store (One, Local3) Return (Local3)',
                      f'Store ({name}, Local0) Return (ObjectType (Local0))',
                      f'Store ({name}, Local0) Return (SizeOf (Local0))']
            if i < 7 and i != 3:
                bodies += [f'Return (ADD0 ({name}))',
                           f'Return (Add (IDEN ({name}), One))',
                           f'Store ({name}, Local0) Return (Add (Local0, One))']
            for body in bodies:
                method = f'M{len(methods):03}'
                methods[method] = body
                declarations.append(f'Method ({method}, 0) {{ {body} }}')
        for position in range(7):
            callee = f'A{position:03}'
            declarations.append(f'Method ({callee}, 7) {{ Return (Arg{position}) }}')
            method = f'X{position:03}'
            body = f'Return ({callee} (V000, V003, V005, V006, V007, V008, V002))'
            methods[method] = body
            declarations.append(f'Method ({method}, 0) {{ {body} }}')
        source = out / f'typed-{revision}.asl'
        source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "TYPED", 1) {{\n' + '\n'.join(declarations) + '\n}\n')
        (out / f'{revision}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
        table = source.with_suffix('.aml')
        names = list(methods)
        # Batch independent read-only evaluations to avoid one AcpiExec startup
        # per method. Require exactly one named result block for every request.
        for start in range(0, len(names), 8):
            batch = names[start:start + 8]
            reference = run([tools / 'acpiexec', '-b',
                             ';'.join(f'execute {name}' for name in batch), table])
            (out / f'{revision}-batch-{start}-reference.log').write_text(reference)
            parts = re.split(r'^Evaluating \\([MX][0-9]{3})\s*$', reference, flags=re.MULTILINE)
            if parts[1::2] != batch:
                raise RuntimeError(f'Missing or duplicate reference evaluations: {parts[1::2]}')
            for method, block in zip(parts[1::2], parts[2::2]):
                actual = run([ROOT / 'build/table_runner', table, method, '--result-object'])
                (out / f'{revision}-{method}-reference.log').write_text(block)
                (out / f'{revision}-{method}-actual.log').write_text(actual)
                expected = objects(block)
                if actual.splitlines() != expected:
                    raise RuntimeError(f'{revision}/{method}: {methods[method]}: ACPICA={expected!r}, CuBit={actual!r}')
                checks += 1
    print(f'AML-ACPICA-TYPED: PASS {checks}')


if __name__ == '__main__':
    main()
