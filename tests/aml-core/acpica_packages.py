#!/usr/bin/env python3
"""Compare loaded package trees, not method execution, against AcpiExec."""
import argparse
import pathlib
import re
from acpica_compare import run, ROOT


def reference_objects(output):
    result = []
    for line in output.splitlines():
        if match := re.fullmatch(r'\s*\[Package\] Contains (\d+) Elements:', line):
            result.append(f'PACKAGE {int(match[1])}')
        elif match := re.fullmatch(r'\s*\[Integer\] = ([0-9A-Fa-f]+)', line):
            result.append(f'INTEGER {int(match[1], 16)}')
        elif match := re.fullmatch(r'\s*\[String\] Length [0-9A-Fa-f]+ = "([^"]*)"', line):
            result.append('STRING ' + match[1])
        elif re.fullmatch(r'\s*\[Null Object\] \(Type=0\)', line):
            result.append('NULL')
        elif re.match(r'\s*\[(Package|Integer|String|Null Object|Buffer)\]', line):
            raise RuntimeError(f'Unrecognized object output: {line}')
    if not result:
        raise RuntimeError('No reference objects')
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    options = parser.parse_args()
    out = ROOT / 'build' / 'acpica-packages'
    out.mkdir(parents=True, exist_ok=True)
    cases = [
        'Package (1) {}',
        'Package (4) {42, "A", Package (2) {One, Zero}}',
        'Package (3) {0xFFFFFFFFFFFFFFFF, 0x8000000000000000, ""}',
        'Package (255) {One, Package (1) {Ones}}',
        'Package (256) {One, "VAR", Package (2) {42}}',
        'Package (1) {' * 16 + 'One' + '}' * 16,
    ]
    checks = 0
    for revision in (1, 2):
        for number, initializer in enumerate(cases):
            tag = f'{revision}-{number}'
            source = out / f'{tag}.asl'
            source.write_text(
                f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "PACKAGES", 1) '
                f'{{ Name (PKG0, {initializer}) }}')
            (out / f'{tag}-compile.log').write_text(run([options.tools / 'iasl', '-oa', source]))
            table = source.with_suffix('.aml')
            reference = run([options.tools / 'acpiexec', '-b', 'execute PKG0', table])
            actual = run([ROOT / 'build' / 'table_runner', table, 'PKG0', '--object'])
            (out / f'{tag}-reference.log').write_text(reference)
            (out / f'{tag}-actual.log').write_text(actual)
            expected = reference_objects(reference)
            observed = actual.splitlines()
            if expected != observed:
                raise RuntimeError(f'{tag}: ACPICA={expected!r}, CuBit={observed!r}')
            checks += 1
    print(f'AML-ACPICA-PACKAGES: PASS {checks}')


if __name__ == '__main__':
    main()
