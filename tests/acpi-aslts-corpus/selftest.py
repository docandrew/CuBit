#!/usr/bin/env python3
"""Control result typing; these synthetic cases never enter corpus percentages."""
from pathlib import Path
import argparse,json,subprocess,sys
sys.path.insert(0,str(Path(__file__).parent));from run import command,save,runtime_argv
p=argparse.ArgumentParser();p.add_argument('--output',type=Path,required=True);p.add_argument('--runner',required=True);p.add_argument('--iasl',required=True);a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
cases={
 'pass':('PASS','Name(ERRS,Zero) Name(RMRC,One) Name(RP0P,Package(){":STST:synthetic:control:PASS:"}) Method(MN00,0){Return(Zero)}'),
 'assertions':('FAIL','Name(ERRS,3) Method(MN00,0){Return(One)}'),
 'skip':('BLOCKED','Name(ERRS,Zero) Name(RMRC,One) Name(RP0P,Package(){":STST:synthetic:control:SKIPPED:"}) Method(MN00,0){Return(Zero)}'),
 'no_report':('BLOCKED','Method(MN00,0){Return(Zero)}'),
 'fatal':('UNSUPPORTED','Method(MN00,0){Fatal(0,0,0) Return(Zero)}'),
 'loop':('TIMEOUT','Method(MN00,0){While(One){Noop}}')}
rows=[]
for name,(expected,body) in cases.items():
 d=a.output/name;d.mkdir(exist_ok=True);asl=d/'MAIN.asl';asl.write_text('DefinitionBlock("","DSDT",2,"CUBIT","CONTROL",1){'+body+'}\n');c=command([a.iasl,'-p',str(d/'MAIN'),str(asl)],d,d/'compile.log',20)
 assert c['returncode']==0,(name,c)
 r=command(runtime_argv(a.runner,d/'MAIN.aml'),d,d/'runtime.log',3)
 if r['timeout']:actual='TIMEOUT';detail={}
 else:
  text=(d/'runtime.log').read_text();detail=json.loads(next(x[len('ASLTS_RESULT '):] for x in text.splitlines() if x.startswith('ASLTS_RESULT ')));actual=detail['status']
 assert actual==expected,(name,expected,actual,detail)
 if name=='assertions':assert detail['assertions_failed']==3
 rows.append(dict(name=name,expected=expected,actual=actual,compile=c,runtime=r,result=detail))
# The input is an upstream-style concatenation, not an edited AML wrapper.
# These synthetic controls stay outside the corpus denominator.
bundle_dir=a.output/'bundle-inputs';bundle_dir.mkdir(exist_ok=True)
bundle_sources={'first': 'DefinitionBlock ("", "DSDT", 1, "CUBIT", "BNDLDSDT", 1) {\n External (LATE, IntObj)\n Name (ERRS, Zero) Name (RMRC, One)\n Name (RP0P, Package () {":STST:synthetic:bundle:PASS:"})\n Name (WIDE, Ones)\n Method (MN00, 0) { If (LATE != One) { ERRS++ } If (WIDE != 0xFFFFFFFF) { ERRS++ } Return (ERRS) }\n}\n', 'second': 'DefinitionBlock ("", "SSDT", 1, "CUBIT", "BNDLSSDT", 1) { Name (LATE, One) }\n', 'only': 'DefinitionBlock ("", "SSDT", 1, "CUBIT", "BNDLONLY", 1) {\n Name (ERRS, Zero) Name (RMRC, One)\n Name (RP0P, Package () {":STST:synthetic:bundle:PASS:"})\n Name (WIDE, Ones)\n Method (MN00, 0) { If ((WIDE >> 32) == Zero) { ERRS++ } Return (ERRS) }\n}\n', 'empty': 'DefinitionBlock ("", "SSDT", 2, "CUBIT", "EMPTY", 1) {}\n'}
bundle_compiles=[]
for name,text in bundle_sources.items():
 asl=bundle_dir/(name+'.asl');asl.write_text(text)
 c=command([a.iasl,'-p',str(bundle_dir/name),str(asl)],bundle_dir,bundle_dir/(name+'-compile.log'),20)
 assert c['returncode']==0,(name,c)
 bundle_compiles.append(c)
first=(bundle_dir/'first.aml').read_bytes();second=(bundle_dir/'second.aml').read_bytes()
only=(bundle_dir/'only.aml').read_bytes();empty=(bundle_dir/'empty.aml').read_bytes()
def changed(data,offset,value):
 result=bytearray(data);result[offset]=value;return bytes(result)
def extent(data,value):
 result=bytearray(data);result[4:8]=value.to_bytes(4,'little');return bytes(result)
def signature(data,value):
 result=bytearray(data);result[:4]=value;result[9]=0;result[9]=(-sum(result))%256;return bytes(result)
bundle_cases=[
 ('dsdt32_then_ssdt',first+second,'PASS'),
 ('ssdt1_only_default64',only,'PASS'),
 ('dsdt32_catalog32',first+second+empty*30,'PASS'),
 ('ssdt_catalog32',only+empty*30,'PASS'),
 ('dsdt_catalog_overflow',first+second+empty*31,'UNSUPPORTED'),
 ('ssdt_catalog_overflow',only+empty*31,'UNSUPPORTED'),
 ('late_dsdt',second+first,'UNSUPPORTED'),
 ('duplicate_dsdt',first+first,'UNSUPPORTED'),
 ('truncated_header',first+second[:20],'UNSUPPORTED'),
 ('truncated_extent',first+second[:-1],'UNSUPPORTED'),
 ('bad_second_checksum',first+changed(second,9,(second[9]+1)%256),'UNSUPPORTED'),
 ('non_aml_signature',signature(first,b'FACP'),'UNSUPPORTED'),
 ('zero_extent',extent(first,0),'UNSUPPORTED'),
 ('trailing_byte',first+second+b'\x00','UNSUPPORTED'),
 ('overflow_extent',first+extent(second,0xffffffff),'UNSUPPORTED'),
 ('bad_first_checksum',changed(first,9,(first[9]+1)%256),'UNSUPPORTED')]
for name,data,expected in bundle_cases:
 d=a.output/name;d.mkdir(exist_ok=True);aml=d/'MAIN.aml';aml.write_bytes(data)
 r=command(runtime_argv(a.runner,aml),d,d/'runtime.log',3)
 assert not r['timeout'],(name,r)
 detail=json.loads(next(x[len('ASLTS_RESULT '):] for x in (d/'runtime.log').read_text().splitlines() if x.startswith('ASLTS_RESULT ')))
 assert r['returncode']==0 and detail['status']==expected,(name,expected,detail,r)
 if expected=='UNSUPPORTED':
  assert detail['reason']=='stage invalid AML table bundle' and detail['value_objects']==0,(name,detail)
 rows.append(dict(name=name,expected=expected,actual=detail['status'],runtime=r,result=detail,synthetic=True))
save(a.output/'RESULT.json',dict(passed=len(rows),total=len(rows),percent=100,synthetic=True,cases=rows,bundle_compiles=bundle_compiles))
print('CLASSIFICATION_CONTROLS '+str(len(rows))+'/'+str(len(rows))+' 100%')
