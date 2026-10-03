from pathlib import Path
import subprocess,re,json
p=Path(__file__).resolve().parent
out=p/'reference-results';out.mkdir(exist_ok=True)
tools=Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin')
def run(args):
 r=subprocess.run(list(map(str,args)),text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=30)
 if r.returncode: raise RuntimeError(r.stdout)
 return r.stdout
known='ACPI Error: 4 (0x4) Outstanding cache allocations (20260408/uttrack-902)'
pattern=r'^.*(?:ACPI (?:Error|Exception):|AE_[A-Z_]+|Failed at AML Offset).*$'
control=out/'control.asl';control.write_text('DefinitionBlock ("", "DSDT", 2, "CUBIT", "CONTROL", 1) { Method (TEST, 0) { Return (One) } }')
(out/'control-compile.log').write_text(run([tools/'iasl','-oa',control]))
control_log=run([tools/'acpiexec','-b','execute TEST',control.with_suffix('.aml')]);(out/'control.log').write_text(control_log)
control_errors=re.findall(pattern,control_log,re.M)
assert control_errors in ([],[known]) and '[Integer] = 0000000000000001' in control_log
checks=0;warnings=0
for rev in (1,2):
 source=out/f'regions-{rev}.asl'
 source.write_text('DefinitionBlock ("", "DSDT", '+str(rev)+', "CUBIT", "REGIONS", 1) {\n'+'''
 Method (MSEL, 0) { Return ("DSDT") }
 Method (GOOD, 0, Serialized) { DataTableRegion (REG0, "DSDT", "", "") Return (ObjectType (REG0)) }
 Method (READ, 0, Serialized) { DataTableRegion (REG0, "DSDT", "", "") Field (REG0, AnyAcc, NoLock, Preserve) { FLD0, 8 } Return (FLD0) }
 Method (DYNM, 0, Serialized) { DataTableRegion (REG0, MSEL (), "", "") Return (ObjectType (REG0)) }
 Method (BUFF, 0, Serialized) { DataTableRegion (REG0, "DSDT", Buffer (0) {}, "") Return (ObjectType (REG0)) }
}
''')
 (out/f'compile-{rev}.log').write_text(run([tools/'iasl','-oa',source]))
 names=['GOOD','READ','DYNM','BUFF']
 ref=run([tools/'acpiexec','-b',';'.join('execute '+n for n in names),source.with_suffix('.aml')]);(out/f'reference-{rev}.log').write_text(ref)
 errors=re.findall(pattern,ref,re.M)
 if errors:
  assert errors==control_errors==[known] and ref.index(known)>ref.rfind('[Integer]'),ref
  warnings+=1
 parts=re.split(r'^Evaluating \\([A-Z]{4})\s*$',ref,flags=re.M);assert parts[1::2]==names,ref
 for name,block in zip(parts[1::2],parts[2::2]):
  values=re.findall(r'^\s*\[Integer\] = ([0-9A-Fa-f]+)\s*$',block,re.M);assert len(values)==1,block
  actual=run([p/'reference-build/table_runner',source.with_suffix('.aml'),name,'--result-object'])
  (out/f'actual-{rev}-{name}.log').write_text(actual)
  assert actual.strip()=='INTEGER '+str(int(values[0],16)),(name,actual,values)
  checks+=1
report={'checks':checks,'baseline_diagnostics':control_errors,'batches_with_diagnostics':warnings,'scope':'actual AML DataTableRegion + Field execution through CuBit service and ACPICA'}
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n');print(report)
