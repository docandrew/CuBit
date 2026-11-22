#!/usr/bin/env python3
"""Compare actual AML literal returns and typed consumers with ACPICA."""
import argparse,json,re
from pathlib import Path
from acpica_compare import ROOT,run
from acpica_typed import objects
parser=argparse.ArgumentParser();parser.add_argument('--tools',required=True,type=Path)
tools=parser.parse_args().tools
out=ROOT/'build/acpica-literals';out.mkdir(parents=True,exist_ok=True)
values=['"DSDT"','""','"123456789ABCDEF0"','Buffer (0) {}','Buffer (1) {255}',
        'Buffer (8) {1,2,3,4,5,6,7,8}','Buffer (20) {0,255,1,128,32,64,7}']
checks=0
for revision in (1,2):
 declarations=['Method (IDEN, 1) { Return (Arg0) }',
               'Method (TYP0, 1) { Return (ObjectType (Arg0)) }',
               'Method (SIZ0, 1) { Return (SizeOf (Arg0)) }']
 methods={}
 for value in values:
  for body in (f'Return ({value})',f'Return (IDEN ({value}))',
               f'Store ({value}, Local0) Return (Local0)',
               f'Return (TYP0 ({value}))',f'Return (SIZ0 ({value}))'):
   name=f'M{len(methods):03}';methods[name]=body;declarations.append(f'Method ({name}, 0) {{ {body} }}')
 source=out/f'literals-{revision}.asl';source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "LITERAL", 1) {{\n'+'\n'.join(declarations)+'\n}\n')
 (out/f'{revision}-compile.log').write_text(run([tools/'iasl','-oa',source]));table=source.with_suffix('.aml');names=list(methods)
 for start in range(0,len(names),8):
  batch=names[start:start+8];reference=run([tools/'acpiexec','-b',';'.join(f'execute {name}' for name in batch),table])
  (out/f'{revision}-{start}-reference.log').write_text(reference)
  parts=re.split(r'^Evaluating \\([M][0-9]{3})\s*$',reference,flags=re.MULTILINE)
  if parts[1::2]!=batch:raise RuntimeError('Missing or duplicate evaluations')
  for name,block in zip(parts[1::2],parts[2::2]):
   expected=objects(block);actual=run([ROOT/'build/table_runner',table,name,'--result-object'])
   (out/f'{revision}-{name}-actual.log').write_text(actual)
   if actual.splitlines()!=expected:raise RuntimeError(f'{revision}/{name}: {methods[name]}: {actual!r} != {expected!r}')
   checks+=1
(out/'report.json').write_text(json.dumps({'checks':checks,'scope':'AML literal strings/buffers and typed consumers'},indent=2)+'\n')
print(f'AML-ACPICA-LITERALS: PASS {checks}')
