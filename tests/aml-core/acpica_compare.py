#!/usr/bin/env python3
"""Hosted, generated ASL comparisons; this is not the upstream ASL test suite."""
import argparse
import pathlib
import re
import subprocess

ROOT = pathlib.Path(__file__).resolve().parent


def run(argv):
    result = subprocess.run([str(x) for x in argv], text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=30)
    if result.returncode:
        raise RuntimeError(f'{argv}:\n{result.stdout}')
    return result.stdout


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--tools', type=pathlib.Path, required=True)
    options = parser.parse_args()
    out = ROOT / 'build' / 'acpica'
    out.mkdir(parents=True, exist_ok=True)
    methods = {
        'FSL0': 'Return (FindSetLeftBit (Arg0))',
        'FSR0': 'Return (FindSetRightBit (Arg0))',
        'FSLT': 'FindSetLeftBit (Arg0, Local0) Return (Local0)',
        'FSRT': 'FindSetRightBit (Arg0, Local0) Return (Local0)',
        'DIV0': 'Return (Divide (Arg0, Arg1))',
        'REM0': 'Divide (Arg0, Arg1, Local0, Local1) Return (Local0)',
        'QUO0': 'Divide (Arg0, Arg1, Local0, Local1) Return (Local1)',
        'ALI0': 'Divide (Arg0, Arg1, Local0, Local0) Return (Local0)',
        'MOD0': 'Return (Mod (Arg0, Arg1))',
        'DNST': 'Return (Add (Divide (Arg0, Arg1, Local0), Local0))',
        'SHL0': 'Return (ShiftLeft (Arg0, Arg1))',
        'SHR0': 'Return (ShiftRight (Arg0, Arg1))',
        'NAN0': 'Return (NAnd (Arg0, Arg1))',
        'NOR0': 'Return (NOr (Arg0, Arg1))',
        'NOT0': 'Return (Not (Arg0))',
        'NTAR': 'Not (Arg0, Local0) Return (Local0)',
        'SNST': 'Return (NAnd (ShiftLeft (Arg0, One), Not (Arg1)))',
        'SINT': 'Return (SizeOf (GLBL))',
        'SSTR': 'Return (SizeOf (STR0))',
        'SBUF': 'Return (SizeOf (BUF0))',
        'SPKG': 'Return (SizeOf (PKG0))',
        'SARG': 'Return (SizeOf (Arg0))',
        'SLOC': 'Store (Arg1, Local0) Return (SizeOf (Local0))',
        'TINT': 'Return (ObjectType (GLBL))',
        'TSTR': 'Return (ObjectType (STR0))',
        'TBUF': 'Return (ObjectType (BUF0))',
        'TPKG': 'Return (ObjectType (PKG0))',
        'TDEV': 'Return (ObjectType (DEV0))',
        'TMTH': 'Return (ObjectType (CREC))',
        'TARG': 'Return (ObjectType (Arg1))',
        'TLOC': 'Store (Arg1, Local0) Return (ObjectType (Local0))',
        'TNON': 'Return (ObjectType (Local7))',
        'TDBG': 'Return (ObjectType (Debug))',
        'TSUM': 'Return (Add (SizeOf (STR0), ObjectType (PKG0)))',
        'CALL': 'Return (Add (CID0 (Arg0), CID0 (Arg1)))',
        'CLOC': 'Store (Arg0, Local0) CZER () Return (Local0)',
        'CSEQ': 'Return (Add (CID0 (Add (Arg0, One, Local0)), CID0 (Local0)))',
        'CSTM': 'CVOI () Return (Arg1)',
        'CRUN': 'Return (CREC (Arg0))',
        'CSCP': r'Return (\DEV0.GETV ())',
        'NREF': 'Return (GLBL)',
        'NADD': 'Return (Add (GLBL, Arg0))',
        'NABS': r'Return (\GLBL)',
        'NDUA': r'Return (\DEV0.VAL0)',
        'LAN0': 'Return (LAnd (Arg0, Arg1))',
        'LOR0': 'Return (LOr (Arg0, Arg1))',
        'LNO0': 'Return (LNot (Arg0))',
        'LEQU': 'Return (LEqual (Arg0, Arg1))',
        'LGRT': 'Return (LGreater (Arg0, Arg1))',
        'LLES': 'Return (LLess (Arg0, Arg1))',
        'LNST': 'Return (LNot (LOr (LEqual (Arg0, Arg1), LGreater (Arg0, Arg1))))',
        'EVAL': 'Store (Zero, Local0) Store (LAnd (Zero, Add (Arg1, One, Local0)), Local1) Return (Local0)',
        'ADD0': 'Return (Add (Arg0, Arg1))',
        'SUB0': 'Return (Subtract (Arg0, Arg1))',
        'MUL0': 'Return (Multiply (Arg0, Arg1))',
        'AND0': 'Return (And (Arg0, Arg1))',
        'OR00': 'Return (Or (Arg0, Arg1))',
        'XOR0': 'Return (Xor (Arg0, Arg1))',
        'NEST': 'Return (Xor (Add (Arg0, Arg1), Multiply (Arg0, Arg1)))',
        'IF00': 'If (Arg0) { Return (Arg1) } Else { Return (Ones) }',
        'LOOP': 'Store (Arg0, Local0) While (Local0) { Subtract (Local0, One, Local0) } Return (Local0)',
        'BRK0': 'While (One) { If (Arg0) { Break } Return (Arg1) } Return (Ones)',
        'CONT': 'Store (Arg0, Local0) Store (Zero, Local1) While (Local0) { Subtract (Local0, One, Local0) If (Local0) { Continue } Store (Arg1, Local1) } Return (Local1)',
    }
    checks = 0
    failures = []
    for revision in (1, 2):
        source = out / f'rev{revision}.asl'
        source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "COMPARE", 1) {{\n' +
                          'Name (GLBL, 0x12345678)\n' +
                          'Name (STR0, \"AB\") Name (BUF0, Buffer (9) {42}) Name (PKG0, Package (5) {})\n' +
                          'Method (CID0, 1, NotSerialized) { Return (Arg0) }\n'
                          'Method (CZER, 0, NotSerialized) { Store (9, Local0) Return (One) }\n'
                          'Method (CVOI, 0, NotSerialized) { Noop }\n'
                          'Method (CREC, 1, NotSerialized) { If (Arg0) { Return (CREC (Subtract (Arg0, One))) } Return (One) }\n' +
                          'Device (DEV0) { Name (_HID, "CUB0001") Name (VAL0, 0x55) '
                          'Method (GETV, 0, NotSerialized) { Return (VAL0) } '
                          'Method (NLOC, 2, NotSerialized) { Return (VAL0) } '
                          'Method (NPAR, 2, NotSerialized) { Return (^VAL0) } '
                          'Method (NUP2, 2, NotSerialized) { Return (^^GLBL) } }\n' +
                          '\n'.join(f'Method ({name}, 2, NotSerialized) {{ {body} }}' for name, body in methods.items()) + '\n}\n')
        (out / f'compile{revision}.log').write_text(run([options.tools / 'iasl', '-oa', source]))
        table = source.with_suffix('.aml')
        for name in [*methods, 'DEV0.NLOC', 'DEV0.NPAR', 'DEV0.NUP2']:
            pairs = [(0, 0), (1, 7), (10, 0xFFFFFFFF), (31, 0xFFFFFFFFFFFFFFFF)]
            if name not in ('LOOP', 'CONT', 'CRUN'):
                pairs += [(0xFFFFFFFF, 1), (0xFFFFFFFFFFFFFFFF, 2), (0x8000000000000000, 3)]
            if name in ('FSL0', 'FSR0', 'FSLT', 'FSRT'):
                pairs += [(1 << bit, 0) for bit in range(64)]
            if name in ('SHL0', 'SHR0'):
                pairs += [(a, b) for a in (1, 0xFFFFFFFFFFFFFFFF, 0x100000000)
                          for b in (0, 31, 32, 33, 63, 64, 65, 0x100000000)]
            if name in ('DIV0', 'REM0', 'QUO0', 'ALI0', 'MOD0', 'DNST'):
                pairs = [(a, b) for a, b in pairs if b != 0]
                pairs += [(0x1FFFFFFFF, 0x200000000),
                          (0xFFFFFFFFFFFFFFFF, 0x8000000000000000)]
            batches = ([pairs[i:i + 16] for i in range(0, len(pairs), 16)]
                       if name in ('FSL0', 'FSR0', 'FSLT', 'FSRT') else [pairs])
            reference = '\n'.join(run([options.tools / 'acpiexec', '-b',
                             ';'.join(f'execute {name} 0x{left:X} 0x{right:X}'
                                      for left, right in batch), table]) for batch in batches)
            (out / f'{revision}-{name}-reference.log').write_text(reference)
            values = re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', reference)
            if len(values) != len(pairs):
                raise RuntimeError(f'{revision}-{name}: missing or ambiguous reference results')
            for (left, right), expected in zip(pairs, values):
                actual = run([ROOT / 'build' / 'table_runner', table, name, left, right])
                tag = f'{revision}-{name}-{left}-{right}'
                (out / f'{tag}.log').write_text(actual)
                ours = re.fullmatch(r'RESULT\s+(\d+)\s*', actual)
                if ours is None:
                    raise RuntimeError(f'{tag}: missing or ambiguous result')
                if int(expected, 16) != int(ours[1]):
                    failures.append(f'{tag}: ACPICA={expected}, CuBit={ours[1]}')
                checks += 1
        for name in ('DIV0', 'MOD0'):
            reference = run([options.tools / 'acpiexec', '-b', f'execute {name} 0x1 0x0', table])
            actual = subprocess.run([str(ROOT / 'build' / 'table_runner'), str(table), name, '1', '0'],
                                    text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=30)
            (out / f'{revision}-{name}-zero-reference.log').write_text(reference)
            (out / f'{revision}-{name}-zero-actual.log').write_text(actual.stdout)
            if 'AE_AML_DIVIDE_BY_ZERO' not in reference or 'returned object' in reference:
                raise RuntimeError(f'{revision}-{name}: missing reference divide-by-zero failure')
            if actual.returncode == 0 or 'execute: DIVISION_BY_ZERO' not in actual.stdout:
                raise RuntimeError(f'{revision}-{name}: missing CuBit divide-by-zero failure')
            checks += 1
    if failures:
        raise RuntimeError('ACPICA mismatches:\n' + '\n'.join(failures))
    print(f'AML-ACPICA-COMPARE: PASS {checks}')


if __name__ == '__main__':
    main()
