#!/usr/bin/env python3
"""Bounded hosted live-field Load checks; run inside the pinned Nix shell."""
import argparse, hashlib, json, os, re, subprocess, time
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--profile',choices=['checked','production','all'],default='all');p.add_argument('--output',type=Path,required=True);p.add_argument('--plan-only',action='store_true');a=p.parse_args()
w=Path(__file__).resolve().parent;repo=w.parent.parent;plan=json.loads((w/'cases.json').read_text());profiles=['checked','production']if a.profile=='all'else[a.profile];selected=[{'profile':profile,**case}for profile in profiles for case in plan['cases']if profile not in case.get('skip_profiles',{})]
if a.plan_only:
 print(json.dumps({'planned':len(selected),'cases':selected,'skips':[{'profile':profile,'case':case['name'],'reason':case['skip_profiles'][profile]}for profile in profiles for case in plan['cases']if profile in case.get('skip_profiles',{})]},indent=2));raise SystemExit(0)
sha=lambda path:hashlib.sha256(Path(path).read_bytes()).hexdigest()
pins=json.loads((w/'sources.json').read_text())
def verify():
 for x in pins['inputs']:assert sha(repo/x['path'])==x['sha256'],x['path']
 for x in pins['tools']:assert sha(x['path'])==x['sha256'],x['path']
verify();out=a.output.resolve();out.mkdir(parents=True,exist_ok=False);commands=[];results=[];compiled={};failure=None

def run(label,argv,timeout,env=None):
 t=time.monotonic();r=subprocess.run(argv,cwd=w,capture_output=True,text=True,timeout=timeout,env=env);log=out/(label+'.log');log.write_text(r.stdout+r.stderr);commands.append({'label':label,'argv':argv,'exit':r.returncode,'seconds':time.monotonic()-t,'log':str(log),'log_sha256':sha(log)});return r,r.stdout+r.stderr
try:
 iasl=next(x['path']for x in pins['tools']if Path(x['path']).name=='iasl')
 for fixture in sorted({x['fixture']for x in selected}):
  stem=Path(fixture).stem;dest=out/stem;r,text=run('compile-'+stem,['timeout','--kill-after=2s','30s',iasl,'-p',str(dest),str(w/fixture)],35);assert r.returncode==0,'compile '+fixture;compiled[fixture]=dest.with_suffix('.aml')
 for profile in profiles:
  build=out/('build-'+profile);env=dict(os.environ,ACPI_FIELD_BUILD=str(build),ACPI_PROFILE_ALLOCATION='off',ACPI_BENCH_PROFILE='off');r,text=run(profile+'-build',['timeout','--kill-after=2s','240s','gprbuild','-q','-p','-j2','-P',str(w/(profile+'.gpr'))],245,env);assert r.returncode==0,profile+' build'
  for case in [x for x in selected if x['profile']==profile]:
   env['ACPI_ASLTS_COPIED_TABLES']=case['copied_tables'];r,text=run(profile+'-'+case['name'],['timeout','--kill-after=2s','12s','prlimit','--as=25769803776','--stack=134217728',str(build/'bin/aslts_corpus_runner'),str(compiled[case['fixture']])],17,env)
   values=[json.loads(x[len('ASLTS_RESULT '):])for x in text.splitlines()if x.startswith('ASLTS_RESULT ')];value=values[0]if len(values)==1 else None;m=re.search(r'SIMULATED_BYTE_BUS reads=\s*(\d+) writes=\s*(\d+)',text);io={'reads':int(m[1]),'writes':int(m[2])}if m else None;ok=r.returncode==0 and value is not None and value['status']==case['status']and value['execution']==case['execution']and(case['io']is None or io==case['io']);delta=None
   if case['gc']:
    stats=[json.loads(x[len('FIELD_LOAD_GC '):])for x in text.splitlines()if x.startswith('FIELD_LOAD_GC ')]
    if len(stats)!=1:ok=False
    else:
     g=stats[0];delta={k:g['after_'+k]-g['before_'+k]for k in ['completed','freed_objects','freed_bytes','rejected']};ok=ok and delta['rejected']==0
     if case['gc']=='none':ok=ok and all(delta[k]==0 for k in ['completed','freed_objects','freed_bytes'])
     else:ok=ok and delta['completed']>=1 and delta['freed_objects']>=1 and delta['freed_bytes']>=4096
   results.append({'profile':profile,'case':case['name'],'matched':ok,'expected_status':case['status'],'result':value,'io':io,'gc_delta':delta});print(profile,case['name'],'MATCH'if ok else'MISMATCH',flush=True);assert ok,profile+' '+case['name']
  verify()
except Exception as e:failure=repr(e)
verify();(out/'RESULT.json').write_text(json.dumps({'terminal':True,'failure':failure,'planned':len(selected),'executed':len(results),'matched':sum(x['matched']for x in results),'cases':results,'commands':commands,'qualification':'Expected refusals remain refusals; not full ACPICA suite or native/proof acceptance.'},indent=2)+'\n')
if failure:raise SystemExit(failure)
