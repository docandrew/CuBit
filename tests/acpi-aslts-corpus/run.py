#!/usr/bin/env python3
"""Pinned ASLTS runtime census. Original sources are copied, never edited."""
import argparse, collections, concurrent.futures, hashlib, json, os, pathlib, re, resource, shlex, shutil, subprocess, time
P=pathlib.Path
HERE=P(__file__).resolve().parent
STATUSES=('PASS','FAIL','UNSUPPORTED','BLOCKED','TIMEOUT','NOT_RUN')
def sha(p): return hashlib.sha256(P(p).read_bytes()).hexdigest()
def save(p,v):
 p=P(p);p.parent.mkdir(parents=True,exist_ok=True);t=p.with_suffix(p.suffix+'.tmp');t.write_text(json.dumps(v,indent=2)+'\n');t.replace(p)
def command(argv,cwd,log,seconds):
 started=time.monotonic()
 try:
  with P(log).open('w') as stream:
   r=subprocess.run(argv,cwd=cwd,stdout=stream,stderr=subprocess.STDOUT,timeout=seconds)
  return dict(argv=list(map(str,argv)),returncode=r.returncode,elapsed=time.monotonic()-started,log=str(log),timeout=False)
 except subprocess.TimeoutExpired:
  return dict(argv=list(map(str,argv)),returncode=None,elapsed=time.monotonic()-started,log=str(log),timeout=True)
def runtime_argv(runner,aml):
 # Match the explicitly bounded hosted census; never inherit a small shell stack.
 return ['prlimit','--as=25769803776','--stack=134217728',str(runner),str(aml)]
def summary(rows):
 c=collections.Counter(r['status'] for r in rows);done=len(rows)-c['NOT_RUN'];return dict(counts={s:c[s] for s in STATUSES},total=len(rows),completed=done,pass_percent_of_completed=round(100*c['PASS']/done,3) if done else 0,completion_percent=round(100*done/len(rows),3),pass_percent_of_full_grid=round(100*c['PASS']/len(rows),3))
def line(s): return 'ASLTS '+' '.join(k+'='+str(v) for k,v in s['counts'].items())+' TOTAL='+str(s['total'])+' PASS_RATE='+str(s['pass_percent_of_completed'])+'% COMPLETION='+str(s['completion_percent'])+'%'
def main():
 ap=argparse.ArgumentParser();ap.add_argument('--upstream',type=P,required=True);ap.add_argument('--output',type=P,required=True);ap.add_argument('--runner',type=P,required=True);ap.add_argument('--iasl',type=P,required=True);ap.add_argument('--jobs',type=int,choices=(1,2),default=2);ap.add_argument('--case-timeout',type=float,default=12);ap.add_argument('--wall-seconds',type=float,default=1200);ap.add_argument('--limit',type=int);ap.add_argument('--select',help='optional source regex for harness admission only');ap.add_argument('--rerun',action='store_true');ap.add_argument('--compat-6141',action='store_true');ap.add_argument('--source-cache',type=P);ap.add_argument('--reuse-runtime',type=P,action='append',default=[]);a=ap.parse_args()
 a.upstream=a.upstream.resolve();a.output=a.output.resolve();a.runner=a.runner.resolve();a.iasl=a.iasl.resolve();a.output.mkdir(parents=True,exist_ok=True)
 catalog=json.loads((HERE/'sources.json').read_text());assert sha(a.iasl)==catalog['tools']['iasl']['sha256'],'pinned iasl drift'
 aggregate_data=json.loads((HERE/'aggregate-flags.json').read_text());aggregate_map={r['source']:r for r in aggregate_data['rows']}
 for entry in aggregate_data['aggregate_evidence']:
  if sha(a.upstream/entry['makefile'])!=entry['makefile_sha256']:raise SystemExit('aggregate Makefile drift')
 inventory=json.loads((HERE/'upstream.json').read_text());tools=dict(runner=dict(path=str(a.runner),sha256=sha(a.runner)),iasl=dict(path=str(a.iasl),sha256=sha(a.iasl)));key=hashlib.sha256(json.dumps(dict(inventory=sha(HERE/'upstream.json'),harness=sha(__file__),aggregate_map=sha(HERE/'aggregate-flags.json'),tools=tools,case_timeout=a.case_timeout,common_flags=['-of','-cr','-vs'],compat_6141=a.compat_6141,source_cache=str(a.source_cache.resolve()) if a.source_cache else None),sort_keys=True).encode()).hexdigest()
 sources=inventory['source_files'];sources={r['path']:r['sha256'] for r in sources} if isinstance(sources,list) else sources
 for p,h in sources.items():
  if sha(a.upstream/p)!=h: raise SystemExit('upstream source drift: '+p)
 save(a.output/'INPUTS.json',dict(cache_key=key,upstream_commit=inventory['commit'],source_files=len(sources),source_manifest_sha256=sha(HERE/'upstream.json'),tools=tools,case_timeout=a.case_timeout,common_flags=['-of','-cr','-vs'],compat_6141=a.compat_6141,profile='release -O1 -gnatp; hosted only',proofs='unchanged packages reused; no proof run',runtime_limits=dict(address_space_bytes=25769803776,stack_bytes=134217728)))
 runtime_candidates={}
 for aggregate in a.reuse_runtime:
  prior=json.loads(aggregate.read_text());inputs=json.loads((aggregate.parent/'INPUTS.json').read_text())
  if inputs['tools']['runner']['sha256']!=tools['runner']['sha256'] or inputs['case_timeout']!=a.case_timeout:raise SystemExit('runtime reuse input mismatch: '+str(aggregate))
  for candidate in prior['cases']:
   if candidate.get('runtime') and candidate.get('aml_sha256') and candidate['status'] in STATUSES[:-1]:
    runtime_candidates.setdefault(candidate['id'],[]).append(candidate)
 # Every mode has an independent tree, so compiler outputs cannot collide.
 for mode in inventory['modes']:
  root=(a.source_cache.resolve() if a.source_cache else a.output/'source')/mode
  if a.source_cache and not (root/'.complete').is_file():raise SystemExit('incomplete source cache: '+str(root))
  if not (root/'.complete').exists():
   for p in sources:
    q=root/p;q.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(a.upstream/p,q)
   (root/'.complete').write_text(key+'\n')
  for p,h in sources.items():
   if sha(root/p)!=h:raise SystemExit('copied upstream source drift: '+p)
 query=a.output/'query.mk';query.write_text('_cubit_aslts_vars:\n\t@printf "FLAGS=%s\\nEXTRA=%s\\nMODULES=%s\\n" \'$(ASLTS_ASLFLAGS)\' \'$(ADD_ASLFLAGS)\' \'$(ASLMOD) $(ASLMODADD)\'\n')
 rows=[]
 for entry in inventory['entrypoints']:
  for mode,flags in inventory['modes'].items():
   ident=hashlib.sha256((entry['path']+'@'+mode).encode()).hexdigest()[:16];folder=a.output/'cases'/ident;folder.mkdir(parents=True,exist_ok=True);receipt=folder/'RESULT.json';row=dict(id=ident,source=entry['path'],source_sha256=entry['sha256'],mode=mode,status='NOT_RUN',cache_key=key,receipt=str(receipt))
   if receipt.exists() and not a.rerun:
    old=json.loads(receipt.read_text())
    if old.get('cache_key')==key:row=old
   rows.append(row)
 def run_case(row):
  row=dict(row);folder=P(row['receipt']).parent;root=(a.source_cache.resolve() if a.source_cache else a.output/'source')/row['mode'];source=root/row['source'];cwd=source.parent;makefile=cwd/'Makefile'
  def finish(status,reason,**kw):
   row.update(status=status,reason=reason,**kw);save(row['receipt'],row);return row
  if makefile.exists():
   q=command(['make','-s','--no-print-directory','-f',str(makefile),'-f',str(query),'_cubit_aslts_vars','ASLTS_MODE='+row['mode'],'ASLTS_VER=cubit-corpus','ASL='+str(a.iasl)],cwd,folder/'make-vars.log',15)
   if q['timeout'] or q['returncode']!=0:return finish('BLOCKED','upstream Makefile variable query failed',make_query=q)
   vals=dict(re.findall(r'^(FLAGS|EXTRA|MODULES)=(.*)$',P(q['log']).read_text(),re.M));flags=shlex.split(vals.get('FLAGS','').replace('"',''));extra=shlex.split(vals.get('EXTRA',''));modules=shlex.split(vals.get('MODULES',''))
   if flags!=inventory['modes'][row['mode']] or not modules or modules[0]!='MAIN':return finish('BLOCKED','unrecognized upstream compiler contract',variables=vals,make_query=q)
   row.update(compiler_contract='upstream install mode/common flags plus per-collection Makefile variables',compiler_flags=flags,upstream_add_aslflags=extra,upstream_modules=modules,make_query=q,makefile_sha256=sha(makefile))
  else:
   flags=inventory['modes'][row['mode']];extra=[];modules=['MAIN']
   row.update(compiler_contract='global Makefile.def mode flags; no local Makefile/overrides in pinned source',compiler_flags=flags,upstream_add_aslflags=extra,upstream_modules=modules,makefile_sha256=None)
  flags=flags+['-of','-cr','-vs'];row['compiler_flags']=flags;row['compile_lane']='6141_compatibility' if a.compat_6141 else 'strict_upstream_install'
  builds=[]
  for module in modules:
   path=cwd/(module+'.asl')
   if not path.is_file():return finish('BLOCKED','upstream auxiliary ASL source missing: '+module,compiles=builds)
   applied_extra=list(extra)
   prefix=folder/module;r=command([str(a.iasl),*flags,*extra,'-p',str(prefix),str(path)],cwd,folder/(module+'-compile.log'),30);builds.append(r)
   if r['timeout']:return finish('TIMEOUT','iasl compiler wall limit',compiles=builds)
   if r['returncode']!=0 and not makefile.exists() and row['source'] in aggregate_map:
    mapped=aggregate_map[row['source']]
    if mapped['source_sha256']!=row['source_sha256']:raise RuntimeError('aggregate source pin drift')
    strict_codes=set(re.findall(r'Error\s+(\d+)',P(r['log']).read_text(errors='replace')))
    expected=sorted(strict_codes & set(mapped['expected_diagnostic_codes']))
    if expected:
     row.setdefault('aggregate_expected_diagnostics',[]).append(dict(codes=expected,strict_preflight=r,preflight_log_sha256=sha(r['log']),upstream_evidence=mapped['aggregates'],scope='configured aggregate includes this leaf DECL and RUN; only emitted upstream expected diagnostics are selected'))
     applied_extra+=sum((['-vx',code]for code in expected),[])
     r=command([str(a.iasl),*flags,*applied_extra,'-p',str(prefix),str(path)],cwd,folder/(module+'-aggregate-compile.log'),30);builds.append(r)
     if r['timeout']:return finish('TIMEOUT','iasl aggregate compiler wall limit',compiles=builds)
   if a.compat_6141 and r['returncode']!=0:
    strict_text=P(r['log']).read_text(errors='replace');error_lines=[line for line in strict_text.splitlines() if re.match(r'^Error\s+\d+',line)]
    exact_lint=bool(error_lines) and set(re.findall(r'Error\s+(\d+)',strict_text))=={'6141'} and all(re.match(r'^Error\s+6141\s*-',line) and 'Device object requires a _HID or _ADR' in line for line in error_lines)
    if exact_lint:
     row.setdefault('expected_diagnostics',[]).append(dict(code=6141,meaning='Device object requires a _HID or _ADR',strict_preflight=r,preflight_log_sha256=sha(r['log']),scope='authorized103000expected-diagnostic lane; every other diagnostic remains effective'))
     r=command([str(a.iasl),*flags,*applied_extra,'-vx','6141','-p',str(prefix),str(path)],cwd,folder/(module+'-compat-compile.log'),30);builds.append(r)
     if r['timeout']:return finish('TIMEOUT','iasl compatibility compiler wall limit',compiles=builds)
   if r['returncode']!=0 or not prefix.with_suffix('.aml').is_file():return finish('BLOCKED','iasl compile failed; original flags retained',diagnostic_codes=sorted(set(re.findall(r'(?:Error|Warning)\s+(\d+)',P(r['log']).read_text(errors='replace')))),compiles=builds)
  aml=folder/'MAIN.aml';row.update(aml_sha256=sha(aml),compiles=builds)
  row['aml_revision']=aml.read_bytes()[8]
  if row['aml_revision']!=(1 if row['mode'].endswith('32')else 2):return finish('BLOCKED','compiler AML revision does not match mode')
  for prior in runtime_candidates.get(row['id'],[]):
   if prior['source_sha256']==row['source_sha256'] and prior['aml_sha256']==row['aml_sha256']:
    receipt=P(prior['receipt']);log=P(prior['runtime']['log'])
    if not receipt.is_file() or not log.is_file():raise RuntimeError('missing runtime reuse evidence')
    if json.loads(receipt.read_text())!=prior:raise RuntimeError('runtime receipt drift')
    row['runtime_reuse']=dict(receipt=str(receipt),receipt_sha256=sha(receipt),log_sha256=sha(log),reason='same source/AML/runner hashes and runtime timeout; explicitly reused, not freshly executed')
    extra_result=dict(runtime=prior['runtime'])
    if 'result' in prior:extra_result['result']=prior['result']
    return finish(prior['status'],prior['reason'],**extra_result)
  r=command(runtime_argv(a.runner,aml),folder,folder/'runtime.log',a.case_timeout)
  if r['timeout']:return finish('TIMEOUT','hosted runner wall limit',runtime=r)
  records=[t[len('ASLTS_RESULT '):] for t in P(r['log']).read_text(errors='replace').splitlines() if t.startswith('ASLTS_RESULT ')]
  if r['returncode']!=0 or len(records)!=1:return finish('BLOCKED','hosted runner crash or missing/duplicate result record',runtime=r)
  try:result=json.loads(records[0])
  except ValueError:return finish('BLOCKED','malformed runner result',runtime=r)
  if result.get('status') not in STATUSES[:-1]:return finish('BLOCKED','unknown runner result',runtime=r)
  return finish(result['status'],result['reason'],runtime=r,result=result)
 todo=[i for i,r in enumerate(rows) if r['status']=='NOT_RUN' and (not a.select or re.search(a.select,r['source']))];todo=todo[:a.limit] if a.limit else todo;started=time.monotonic();last=started
 def checkpoint():
  s=summary(rows);save(a.output/'RESULT.json',dict(summary=s,cache_key=key,cases=rows));print(line(s),flush=True)
 checkpoint()
 with concurrent.futures.ThreadPoolExecutor(max_workers=a.jobs) as pool:
  pending={};it=iter(todo)
  while True:
   while len(pending)<a.jobs and time.monotonic()-started<a.wall_seconds:
    try:i=next(it)
    except StopIteration:break
    pending[pool.submit(run_case,rows[i])]=i
   if not pending:break
   ready,_=concurrent.futures.wait(pending,timeout=1,return_when=concurrent.futures.FIRST_COMPLETED)
   for fut in ready:
    i=pending.pop(fut)
    try:rows[i]=fut.result()
    except Exception as e:rows[i].update(status='BLOCKED',reason='harness orchestrator exception: '+repr(e));save(rows[i]['receipt'],rows[i])
   if time.monotonic()-last>=20:checkpoint();last=time.monotonic()
 checkpoint()
if __name__=='__main__':
 # Large generic interpreter frames use hosted stack space; never alter shared limits.
 soft,hard=resource.getrlimit(resource.RLIMIT_STACK);target=128*1024*1024
 resource.setrlimit(resource.RLIMIT_STACK,(min(target,hard) if hard!=-1 else target,hard))
 main()
