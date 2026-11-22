#!/usr/bin/env python3
"""Compare mutable integer namespace writes, capture order and nested calls."""
from pathlib import Path
import argparse
import subprocess
import re
root=Path(__file__).resolve().parent
out=root/'build'/'acpica-named-store';out.mkdir(exist_ok=True)
parser = argparse.ArgumentParser()
parser.add_argument('--tools', type=Path, required=True)
tools = parser.parse_args().tools
cases=[
    'Store (7, NUM0) Return (NUM0)',
    'Return (Store (7, NUM0))',
    'Return (Add (NUM0, Store (7, NUM0)))',
    'Return (Add (Store (7, NUM0), NUM0))',
    'Return (Add (NUM0, WRIT (7)))',
    'Return (SUM2 (NUM0, WRIT (7)))',
    'Return (SUM2 (WRIT (7), NUM0))',
    'Add (One, One, NUM0) Return (NUM0)',
    'Divide (17, 5, NUM0, NUM0) Return (NUM0)',
    'WRIT (7) Return (NUM0)',
    'Return (Add (NUM0, Add (One, One, NUM0)))',
    'Return (Add (NUM0, Store (Store (7, NUM0), Local0)))',
    'Return (LAnd (NUM0, Store (Zero, NUM0)))',
    'Return (LEqual (NUM0, Store (7, NUM0)))',
    'Store (NUM0, Local0) WRIT (7) Return (Local0)',
    'Return (IDEN (NUM0))',
    'Return (REPL (NUM0))',
    'REPL (NUM0) Return (NUM0)',
    'Store (Ones, NUM0) Return (NUM0)',
    'Store (Zero, NUM0) Return (NUM0)',
    r'Store (7, \NUM0) Return (\NUM0)',
    'Store (7, ^NUM0) Return (^NUM0)',
    r'Store (7, \DEV0.NUM0) Return (\DEV0.NUM0)',
    r'Return (\DEV0.WDEV ())',
]
checks=0
for rev in (1,2):
    declarations=['Name (NUM0, 9)', 'Device (DEV0) { Name (_ADR, Zero) Name (NUM0, 3) Method (WDEV, 0) { Store (5, NUM0) Return (NUM0) } }', 'Method (WRIT, 1) { Store (Arg0, NUM0) Return (NUM0) }',
    'Method (SUM2, 2) { Return (Add (Arg0, Arg1)) }',
    'Method (IDEN, 1) { Return (Arg0) }',
    'Method (REPL, 1) { Store (One, Arg0) Return (Arg0) }']
    declarations += [f'Method (M{i:03}, 0) {{ Store (9, NUM0) {body} }}' for i,body in enumerate(cases)]
    source=out/f'{rev}.asl';source.write_text(f'DefinitionBlock ("", "DSDT", {rev}, "CUBIT", "NSTORE", 1) {{\n'+'\n'.join(declarations)+'\n}')
    r=subprocess.run([str(tools/'iasl'),'-oa',str(source)],text=True,capture_output=True,timeout=60)
    (out/f'{rev}-compile.log').write_text(r.stdout+r.stderr)
    if r.returncode: raise RuntimeError(r.stdout+r.stderr)
    for i,body in enumerate(cases):
        name=f'M{i:03}';table=source.with_suffix('.aml')
        r=subprocess.run([str(tools/'acpiexec'),'-b',f'execute {name}',str(table)],text=True,capture_output=True,timeout=60)
        ref=r.stdout+r.stderr;(out/f'{rev}-{name}-reference.log').write_text(ref)
        expected=re.findall(r'\[Integer\] = ([0-9A-Fa-f]+)',ref)
        a=subprocess.run([str(root/'build'/'table_runner'),str(table),name],text=True,capture_output=True,timeout=60)
        actual=a.stdout+a.stderr;(out/f'{rev}-{name}-actual.log').write_text(actual)
        if r.returncode or a.returncode or len(expected)!=1 or actual.strip()!=f'RESULT {int(expected[0],16)}':
            raise RuntimeError((rev,body,expected,actual))
        checks+=1
print('AML-ACPICA-NAMED-STORE: PASS',checks)
