from pathlib import Path
import subprocess,re,json,argparse
root=Path(__file__).resolve().parent
out=root/'build/acpica-string-conversions';out.mkdir(parents=True,exist_ok=True)
parser=argparse.ArgumentParser();parser.add_argument('--tools',required=True,type=Path)
parser.add_argument('--runner',type=Path,default=root/'build/conversion_runner')
options=parser.parse_args();tools=options.tools
def run(args):
 r=subprocess.run(list(map(str,args)),text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=30)
 if r.returncode:raise RuntimeError(r.stdout)
 return r.stdout
integers=[0,1,15,16,255,0x12345678,0xFFFFFFFF,0xFEDCBA9876543210]
buffers=[[],*[ [n] for n in (0,1,9,10,15,16,31,32,127,128,254,255)], [0,255],[255,0],[0,1,128,255],[68,83,68,84]]
# Preserve and classify the exact allocation-tracker diagnostic reproduced by
# a control method with no conversions. Never suppress execution errors.
control_source=out/'control.asl'
control_source.write_text('DefinitionBlock ("", "DSDT", 2, "CUBIT", "CONTROL", 1) { Method (TEST, 0) { Return (One) } }\n')
(out/'control-compile.log').write_text(run([tools/'iasl','-oa',control_source]))
control=run([tools/'acpiexec','-b','execute TEST',control_source.with_suffix('.aml')])
(out/'control.log').write_text(control)
known='ACPI Error: 4 (0x4) Outstanding cache allocations (20260408/uttrack-902)'
pattern=r'^.*(?:ACPI (?:Error|Exception):|AE_[A-Z_]+|Failed at AML Offset).*$'
control_errors=re.findall(pattern,control,re.M)
if control_errors not in ([],[known]) or '[Integer] = 0000000000000001' not in control:
 raise RuntimeError('Control diagnostic changed: '+control)
checks=0
shutdown_diagnostics=0
for revision,width in ((1,32),(2,64)):
 cases=[(str(n & ((1<<width)-1)),['integer',str(width),str(n)]) for n in integers]
 cases += [('Buffer (%d) {%s}'%(len(b),','.join(map(str,b))),['buffer',*map(str,b)]) for b in buffers]
 declarations=[]
 for i,(value,_) in enumerate(cases):declarations.append(f'Method (M{i:03},0) {{ Store ({value}, Local0) Return (Concatenate ("", Local0)) }}')
 source=out/f'convert-{revision}.asl';source.write_text(f'DefinitionBlock ("", "DSDT", {revision}, "CUBIT", "STRCONV", 1) {{\n'+'\n'.join(declarations)+'\n}\n')
 (out/f'compile-{revision}.log').write_text(run([tools/'iasl','-oa',source]))
 for start in range(0,len(cases),8):
  names=[f'M{i:03}' for i in range(start,min(start+8,len(cases)))]
  output=run([tools/'acpiexec','-b',';'.join('execute '+n for n in names),source.with_suffix('.aml')])
  (out/f'{revision}-{start}.log').write_text(output)
  errors=re.findall(pattern,output,re.M)
  if errors:
   if errors != control_errors or errors != [known] or output.index(known) < output.rfind('[String]'):
    raise RuntimeError(output)
   shutdown_diagnostics+=1
  parts=re.split(r'^Evaluating \\([M][0-9]{3})\s*$',output,flags=re.M)
  if parts[1::2]!=names:raise RuntimeError('missing/duplicate evaluations')
  for name,block in zip(parts[1::2],parts[2::2]):
   matches=re.findall(r'^\s*\[String\] Length ([0-9A-Fa-f]+) = "([^"\n]*)"\s*$',block,re.M)
   if len(matches)!=1:raise RuntimeError(block)
   size,expected=matches[0]
   if int(size,16)!=len(expected):raise RuntimeError('truncated reference output')
   actual=run([options.runner,*cases[int(name[1:])][1]]).removesuffix('\n')
   if actual!=expected:raise RuntimeError((revision,name,actual,expected))
   checks+=1
(out/'report.json').write_text(json.dumps({'checks':checks,'baseline_shutdown_diagnostics':control_errors,'batches_with_baseline_diagnostic':shutdown_diagnostics,'scope':'pure implicit string conversions compared with ACPICA Concatenate string operand conversion'},indent=2)+'\n')
print('ACPICA conversion values MATCH',checks,'; baseline shutdown diagnostics in',shutdown_diagnostics,'batches'+(' (not an error-free ACPICA run)' if shutdown_diagnostics else ''))
