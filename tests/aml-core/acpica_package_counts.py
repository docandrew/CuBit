#!/usr/bin/env python3
"""Compare namespace-bound VarPackage counts and lexical lookup with ACPICA."""
import argparse
import pathlib
from acpica_compare import ROOT, run
from acpica_packages import reference_objects


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    tools = parser.parse_args().tools
    out = ROOT / 'build' / 'acpica-package-counts'
    out.mkdir(parents=True, exist_ok=True)
    cases = []
    for count in (0, 1, 2, 16, 255):
        for initialized in sorted({0, min(count, 1), min(count, 2)}):
            cases.append((f'Name (CNT0, {count}) Name (PKG0, Package (CNT0) {{' +
                          ','.join(['One'] * initialized) + '})', 'PKG0'))
    scoped = r'''
Name (CNT0, 4)
Device (DEV0) {
    Name (_ADR, Zero)
    Name (CNT0, 2)
    Name (PKG0, Package (CNT0) {One})
    Device (CHLD) {
        Name (_ADR, Zero)
        Name (PKG1, Package (^CNT0) {One})
        Name (PKG2, Package (\CNT0) {One})
        Name (PKG3, Package (CNT0) {One})
    }
}
Name (DEV0.PKG4, Package (CNT0) {One})
Name (PKG5, Package (CNT0) {Package (CNT0) {One}})
Name (PKG6, Package (DEV0.CNT0) {One})
'''
    for name in ('DEV0.PKG0', 'DEV0.CHLD.PKG1', 'DEV0.CHLD.PKG2',
                 'DEV0.CHLD.PKG3', 'DEV0.PKG4', 'PKG5', 'PKG6'):
        cases.append((scoped, name))
    checks = 0
    for revision in (1, 2):
        for index, (declarations, name) in enumerate(cases):
            tag = f'{revision}-{index}'
            source = out / f'{tag}.asl'
            source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "COUNTS", 1) {{\n'
                              + declarations + '\n}\n')
            (out / f'{tag}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
            table = source.with_suffix('.aml')
            reference = run([tools / 'acpiexec', '-b', f'evaluate {name}', table])
            (out / f'{tag}-reference.log').write_text(reference)
            actual = run([ROOT / 'build/table_runner', table, name, '--object'])
            (out / f'{tag}-actual.log').write_text(actual)
            expected = reference_objects(reference)
            if not expected or actual.splitlines() != expected:
                raise RuntimeError(f'Package count mismatch: {tag}/{name}')
            checks += 1
    print(f'AML-ACPICA-PACKAGE-COUNTS: PASS {checks}')


if __name__ == '__main__':
    main()
