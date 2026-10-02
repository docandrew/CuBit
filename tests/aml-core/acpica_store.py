#!/usr/bin/env python3
"""Compare Store expression values, ordering and invocation-local writes."""
import argparse
import pathlib
import re
import subprocess
from acpica_compare import ROOT, run
from acpica_typed import objects


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    tools = parser.parse_args().tools
    out = ROOT / 'build' / 'acpica-store'
    out.mkdir(parents=True, exist_ok=True)
    cases = []
    targets = [f'Local{i}' for i in range(8)] + [f'Arg{i}' for i in range(7)]
    for target in targets:
        sink = "Local6" if target == "Local7" else "Local7"
        cases += [
            f'Return (Store (One, {target}))',
            f'Return (Store (STR0, {target}))',
            f'Store (BUF0, {target}) Return (SizeOf ({target}))',
            f'Store (PKG0, {target}) Return ({target})',
            f'Return (Add (Store (STR0, {target}), One))',
            f'Add (Store (STR0, {target}), One, {sink}) Return (ObjectType ({target}))',
            f'Add (One, One, {target}) Return ({target})',
            f'Return (Add (Store (One, {target}), {target}))',
            f'Store (9, {target}) Return (Add ({target}, Store (One, {target})))',
            f'Store (STR0, {target}) Return (Add ({target}, Store (One, {target})))',
            f'Store (One, {target}) Return (Add ({target}, Store (STR0, {target})))',
            f'Store (9, {target}) Return (SUM2 ({target}, Store (One, {target})))',
            f'Return (Store (Store (PKG0, {target}), Local0))',
        ]
    for remainder in range(7):
        for quotient in range(7):
            cases.append(f'Divide (17, 5, Arg{remainder}, Arg{quotient}) Return (Arg{remainder})')
    checks = 0
    for revision in (1, 2):
        for base in range(0, len(cases), 40):
            declarations = ['Name (STR0, "1F")',
                            'Name (BUF0, Buffer (8) {1,2,3,4,5,6,7,8})',
                            'Name (PKG0, Package (3) {One, "P"})',
                            'Method (SUM2, 2) { Return (Add (Arg0, Arg1)) }']
            names = []
            for index, body in enumerate(cases[base:base + 40], base):
                helper, name = f'H{index:03}', f'M{index:03}'
                names.append(name)
                declarations += [f'Method ({helper}, 7) {{ {body} }}',
                                 f'Method ({name}, 0) {{ Return ({helper} (5,6,7,8,9,10,11)) }}']
            # An argument write must not overwrite a caller's local or named object.
            declarations += ['Method (REPL, 1) { Store (One, Arg0) Return (Arg0) }',
                             'Method (KEEP, 0) { Store (PKG0, Local0) REPL (Local0) Return (Local0) }']
            names.append('KEEP')
            tag = f'{revision}-{base}'
            source = out / f'{tag}.asl'
            source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "STORE", 1) {{\n' + '\n'.join(declarations) + '\n}\n')
            compiled = subprocess.run([str(tools / 'iasl'), '-oa', str(source)],
                                      text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=30)
            (out / f'{tag}-compile.log').write_text(compiled.stdout)
            if compiled.returncode:
                raise RuntimeError(f'Compilation failed: {out / (tag + "-compile.log")}')
            table = source.with_suffix('.aml')
            for start in range(0, len(names), 8):
                batch = names[start:start + 8]
                reference = run([tools / 'acpiexec', '-b', ';'.join(f'execute {name}' for name in batch), table])
                (out / f'{tag}-batch-{start}.log').write_text(reference)
                parts = re.split(r'^Evaluating \\(M[0-9]{3}|KEEP)\s*$', reference, flags=re.MULTILINE)
                if parts[1::2] != batch:
                    raise RuntimeError(f'Missing/duplicate evaluations: {parts[1::2]}')
                for name, block in zip(parts[1::2], parts[2::2]):
                    actual = run([ROOT / 'build/table_runner', table, name, '--result-object'])
                    (out / f'{tag}-{name}-actual.log').write_text(actual)
                    expected = objects(block)
                    if actual.splitlines() != expected:
                        raise RuntimeError(f'{tag}/{name}: ACPICA={expected!r}, CuBit={actual!r}')
                    checks += 1
    # ASL syntax does not accept constant names as explicit target operands.
    # Feed the exact same checksum-valid AML encodings to both interpreters.
    for revision in (1, 2):
        payload = bytearray()
        names = []
        for target in (0, 1, 255):
            for code in (bytes([0xA4, 0x70, 0x0A, 23, target]),
                         bytes([0xA4, 0x72, 1, 1, target]),
                         bytes([0xA4, 0x78, 0x0A, 17, 0x0A, 5, target, target])):
                name = f'M{900 + len(names)}'
                names.append(name)
                payload += bytes([0x14, 6 + len(code)]) + name.encode() + bytes([0]) + code
        header = bytearray(36)
        header[:4] = b'DSDT'
        header[4:8] = (36 + len(payload)).to_bytes(4, 'little')
        header[8] = revision
        header[10:16] = b'CUBIT '
        header[16:24] = b'STORECON'
        header[9] = (-sum(header + payload)) & 255
        table = out / f'constants-{revision}.aml'
        table.write_bytes(header + payload)
        reference = run([tools / 'acpiexec', '-b', ';'.join(f'execute {name}' for name in names), table])
        (out / f'constants-{revision}-reference.log').write_text(reference)
        parts = re.split(r'^Evaluating \\(M[0-9]{3})\s*$', reference, flags=re.MULTILINE)
        if parts[1::2] != names:
            raise RuntimeError('Missing constant-target evaluations')
        for name, block in zip(parts[1::2], parts[2::2]):
            actual = run([ROOT / 'build/table_runner', table, name, '--result-object'])
            (out / f'constants-{revision}-{name}-actual.log').write_text(actual)
            if actual.splitlines() != objects(block):
                raise RuntimeError(f'Constant target mismatch: {revision}/{name}')
            checks += 1
    print(f'AML-ACPICA-STORE: PASS {checks}')


if __name__ == '__main__':
    main()
