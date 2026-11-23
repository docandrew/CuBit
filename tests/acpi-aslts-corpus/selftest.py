#!/usr/bin/env python3
"""Control result typing; these synthetic cases never enter corpus percentages."""
from pathlib import Path
import argparse,json,subprocess,sys
sys.path.insert(0,str(Path(__file__).parent));from run import command,save
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
 r=command([a.runner,str(d/'MAIN.aml')],d,d/'runtime.log',3)
 if r['timeout']:actual='TIMEOUT';detail={}
 else:
  text=(d/'runtime.log').read_text();detail=json.loads(next(x[len('ASLTS_RESULT '):] for x in text.splitlines() if x.startswith('ASLTS_RESULT ')));actual=detail['status']
 assert actual==expected,(name,expected,actual,detail)
 if name=='assertions':assert detail['assertions_failed']==3
 rows.append(dict(name=name,expected=expected,actual=actual,compile=c,runtime=r,result=detail))
save(a.output/'RESULT.json',dict(passed=len(rows),total=len(rows),percent=100,synthetic=True,cases=rows));print('CLASSIFICATION_CONTROLS '+str(len(rows))+'/'+str(len(rows))+' 100%')
