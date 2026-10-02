#!/usr/bin/env python3
"""Same-table implicit conversion comparisons through the actual executor."""
import argparse
import pathlib
import re
import subprocess
from acpica_compare import ROOT, run


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    tools = parser.parse_args().tools
    out = ROOT / 'build' / 'acpica-coercions'
    out.mkdir(parents=True, exist_ok=True)
    strings = ['', ' ', '10', '0x10', '0X000fGH', '12 34', '-10', '+10',
               '0000000000000000000000001', '123456789ABCDEF01',
               'FFFFFFFFFFFFFFFFFFFFFFFF', '  0x12g', '0x', 'abcDEF']
    initializers = [f'"{s}"' for s in strings]
    initializers += [f'Buffer ({n}) {{' + ','.join(str(i) for i in range(1, n+1)) + '}'
                     for n in (1, 2, 3, 4, 5, 8, 9, 12)]
    checks = 0
    for revision in (1, 2):
        for start in range(0, len(initializers), 8):
            methods = {}
            declarations = ['Method (CID0, 1, NotSerialized) { Return (Arg0) }']
            for index, initializer in enumerate(initializers[start:start+8], start):
                name = f'V{index:03d}'
                declarations.append(f'Name ({name}, {initializer})')
                bodies = {
                    'A': f'Return (Add ({name}, Zero))',
                    'I': f'Return (Add ({initializer}, Zero))',
                    'P': f'If ({name}) {{ Return (One) }} Return (Zero)',
                    'L': f'Return (LNot ({name}))',
                    'E': f'Return (LEqual (Zero, {name}))',
                    'C': f'Return (CID0 (Add ({name}, Zero)))',
                }
                for prefix, body in bodies.items():
                    method = f'{prefix}{index:03d}'
                    methods[method] = body
                    declarations.append(f'Method ({method}, 0, NotSerialized) {{ {body} }}')
            tag = f'{revision}-{start}'
            source = out / f'{tag}.asl'
            source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "COERCION", 1) {{\n'
                              + '\n'.join(declarations) + '\n}')
            (out / f'{tag}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
            table = source.with_suffix('.aml')
            reference = run([tools / 'acpiexec', '-b', ';'.join(f'execute {name}' for name in methods), table])
            (out / f'{tag}-reference.log').write_text(reference)
            values = re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', reference)
            if len(values) != len(methods):
                raise RuntimeError(f'{tag}: missing or ambiguous reference results')
            for name, expected in zip(methods, values):
                actual = run([ROOT / 'build' / 'table_runner', table, name])
                (out / f'{tag}-{name}.log').write_text(actual)
                match = re.fullmatch(r'RESULT\s+(\d+)\s*', actual)
                if match is None or int(match[1]) != int(expected, 16):
                    raise RuntimeError(f'{tag}-{name}: ACPICA={expected}, CuBit={actual!r}')
                checks += 1
        source = out / f'{revision}-empty.asl'
        source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "EMPTYBUF", 1) {{ '
                          'Name (BUF0, Buffer (Zero) {}) '
                          'Method (NAM0, 0, NotSerialized) { Return (Add (BUF0, One)) } '
                          'Method (INL0, 0, NotSerialized) { Return (Add (Buffer (Zero) {}, One)) } }')
        (out / f'{revision}-empty-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
        for name in ('NAM0', 'INL0'):
            table = source.with_suffix('.aml')
            reference = run([tools / 'acpiexec', '-b', f'execute {name}', table])
            actual = subprocess.run([str(ROOT / 'build' / 'table_runner'), str(table), name],
                                    text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=30)
            (out / f'{revision}-{name}-empty-reference.log').write_text(reference)
            (out / f'{revision}-{name}-empty-actual.log').write_text(actual.stdout)
            if 'AE_AML_BUFFER_LIMIT' not in reference or 'returned object' in reference:
                raise RuntimeError('Missing reference empty-buffer failure')
            if actual.returncode == 0 or 'execute: EMPTY_BUFFER' not in actual.stdout:
                raise RuntimeError('Missing CuBit empty-buffer failure')
            checks += 1
    print(f'AML-ACPICA-COERCIONS: PASS {checks}')


if __name__ == '__main__':
    main()
