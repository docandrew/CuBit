#!/usr/bin/env python3
"""Compare owned method bodies beyond the former buffer-literal size bound."""
import argparse
import pathlib
import re
from acpica_compare import ROOT, run


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    tools = parser.parse_args().tools
    out = ROOT / 'build' / 'acpica-methods'
    out.mkdir(parents=True, exist_ok=True)
    checks = 0
    for revision in (1, 2):
        for size in (1023, 1024, 1025, 1170, 4096, 16384, 65000):
            tag = f'{revision}-{size}'
            source = out / f'{tag}.asl'
            source.write_text(
                f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "METHODS", 1) {{\n'
                f'Method (LONG, 0) {{ {"Noop " * size} Return (Ones) }}\n'
                'Method (CALL, 0) { Return (LONG ()) }\n}\n')
            (out / f'{tag}-compile.log').write_text(run([tools / 'iasl', '-oa', source]))
            table = source.with_suffix('.aml')
            reference = run([tools / 'acpiexec', '-b', 'execute LONG;execute CALL', table])
            (out / f'{tag}-reference.log').write_text(reference)
            parts = re.split(r'^Evaluating \\(LONG|CALL)\s*$', reference, flags=re.MULTILINE)
            if parts[1::2] != ['LONG', 'CALL']:
                raise RuntimeError(f'Missing method result: {tag}')
            for name, block in zip(parts[1::2], parts[2::2]):
                expected = re.search(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', block)
                actual = run([ROOT / 'build/table_runner', table, name])
                (out / f'{tag}-{name}-actual.log').write_text(actual)
                if not expected or actual.strip() != f'RESULT {int(expected[1], 16)}':
                    raise RuntimeError(f'Method mismatch: {tag}/{name}: {actual}')
                checks += 1
    print(f'AML-ACPICA-METHODS: PASS {checks}')


if __name__ == '__main__':
    main()
