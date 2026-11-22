from pathlib import Path
import subprocess,re,json,argparse,hashlib
p=Path(__file__).resolve().parent
parser=argparse.ArgumentParser()
parser.add_argument('--tools',required=True,type=Path)
parser.add_argument('--runner',type=Path,default=p/'build/table_runner')
parser.add_argument('--output',type=Path,default=p/'build/acpica-datatable-regions')
options=parser.parse_args();tools=options.tools;out=options.output
out.mkdir(parents=True,exist_ok=True)
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
  actual=run([options.runner,source.with_suffix('.aml'),name,'--result-object'])
  (out/f'actual-{rev}-{name}.log').write_text(actual)
  assert actual.strip()=='INTEGER '+str(int(values[0],16)),(name,actual,values)
  checks+=1

# Compare failures by the interpreter's documented coarse status mapping.
cases={'EMPT':('""','""','""'),'SHRT':('"DSD"','""','""'),
       'LONG':('"DSDTextra"','""','""'),'OEML':('"DSDT"','"1234567"','""'),
       'TIDL':('"DSDT"','""','"123456789"'),'BOTH':('"bad!"','"1234567"','""'),
       'MISS':('"NONE"','""','""'),'ZSIG':('Zero','""','""'),'PART':('"DSDT"','"CUB"','""')}
source=out/'selectors.asl'
source.write_text('DefinitionBlock ("", "DSDT", 2, "CUBIT", "SELECTOR", 1) {\n'+
 '\n'.join(f'Method ({name},0,Serialized) {{ DataTableRegion(REG0,{a},{b},{c}) Return (ObjectType(REG0)) }}' for name,(a,b,c) in cases.items())+'\n}\n')
(out/'selectors-compile.log').write_text(run([tools/'iasl','-oa',source]))
status_mapping={'AE_BAD_SIGNATURE':'BAD_NAME','AE_AML_STRING_LIMIT':'UNSUPPORTED_VALUE','AE_NOT_FOUND':'UNKNOWN_NAME'}
selector_results={}
for name in cases:
 ref=run([tools/'acpiexec','-b','execute '+name,source.with_suffix('.aml')])
 (out/f'{name}-reference.log').write_text(ref)
 values=re.findall(r'^\s*\[Integer\] = ([0-9A-Fa-f]+)\s*$',ref,re.M)
 failures=re.findall(r'Evaluation of \\'+name+r' failed with status (AE_[A-Z_]+)',ref)
 assert (len(values)==1 and not failures) or (len(failures)==1 and not values),ref
 expected_code=failures[0] if failures else None
 errors=re.findall(pattern,ref,re.M)
 for error in errors:
  if error==known:
   assert control_errors==[known],ref
  elif expected_code=='AE_NOT_FOUND' and re.fullmatch(r'ACPI Error: ACPI Table \[[A-Z0-9_!]{4}\] OEM:\([^)]*\) not found in RSDT/XSDT \([0-9]+/dsopcode-[0-9]+\)',error):
   pass
  elif expected_code and (expected_code in error or error.startswith('Failed at AML Offset')):
   pass
  else:
   raise RuntimeError('Unexpected reference diagnostic: '+error)
 if known in errors:warnings+=1
 result=subprocess.run([str(options.runner),str(source.with_suffix('.aml')),name,'--result-object'],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=30)
 (out/f'{name}-actual.log').write_text(result.stdout)
 if values:
  expected='INTEGER '+str(int(values[0],16));actual=result.stdout.strip()
  assert result.returncode==0 and actual==expected,(name,result.stdout,expected)
 else:
  expected=status_mapping[expected_code]
  statuses=re.findall(r'execute: ([A-Z_]+)',result.stdout)
  assert result.returncode!=0 and statuses==[expected],(name,result.stdout,expected)
  actual=statuses[0]
 selector_results[name]={'reference':expected_code or expected,'actual':actual}
 checks+=1
report={'checks':checks,'baseline_diagnostics':control_errors,'batches_with_diagnostics':warnings,
        'scope':'actual AML DataTableRegion/Field execution and selector error mapping',
        'status_mapping':status_mapping,'selectors':selector_results,
        'runner_sha256':hashlib.sha256(options.runner.read_bytes()).hexdigest()}
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
print('DataTableRegion reference comparisons:',checks,'; baseline shutdown diagnostics:',warnings)
