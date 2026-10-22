"""Private bounded hosted gate. Explicit selection; no oracle regeneration."""
from pathlib import Path
import argparse,hashlib,json,os,re,shlex,shutil,signal,sys,time
if sys.flags.optimize:
 raise RuntimeError("optimized Python is forbidden for this guarded runner")
from hosted_capture import capture,CaptureInterrupted
HERE=Path(__file__).resolve().parent; ROOT=HERE.parents[1]
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def interrupted(signum,frame):raise CaptureInterrupted(str(signum))
signal.signal(signal.SIGTERM,interrupted);signal.signal(signal.SIGINT,interrupted)
ap=argparse.ArgumentParser();ap.add_argument('--profile',choices=['release'],required=True);ap.add_argument('--selection',choices=['local-n64'],default='local-n64');args=ap.parse_args()
manifest_path=HERE/'source-inputs.json';manifest_bytes=manifest_path.read_bytes();manifest=json.loads(manifest_bytes)
inventory=json.loads((HERE/'source-inventory.json').read_text())
corpus=json.loads((HERE/'corpus.json').read_text())
OUT=HERE/('results-'+args.profile+'-'+args.selection)
if OUT.exists():raise RuntimeError('preserve existing evidence; choose fresh runner successor')
OUT.mkdir();BUILD=OUT/'build';OBJ=BUILD/'obj';BIN=BUILD/'bin'
OBJ.mkdir(parents=True);BIN.mkdir()
os.environ['ASLTS_BUILD']=str(BUILD)
if os.environ.get('TMPDIR')!='/home/doc/cubit-build-tmp':raise RuntimeError('required TMPDIR not selected before Nix')

def verify():
 if manifest_path.read_bytes()!=manifest_bytes:raise RuntimeError('manifest changed')
 for rel,h in manifest.items():
  f=ROOT/rel
  if not f.is_file() or digest(f)!=h:raise RuntimeError('source changed '+rel)
 for rel,names in inventory.items():
  actual=sorted(x.name for x in (ROOT/rel).iterdir() if x.is_file() and x.suffix in ('.ads','.adb'))
  if actual!=names:raise RuntimeError('source directory inventory changed '+rel)
verify()
local={}
for directory,names in inventory.items():
 for name in names:
  if name in local:raise RuntimeError('duplicate Ada basename '+name)
  local[name]=(ROOT/directory/name).resolve()
python=Path(sys.executable).resolve();gpr=Path(shutil.which('gprbuild') or '').resolve();gcc=(Path(shutil.which('gnatbind') or '').parent/'gcc').resolve()
if not gpr.is_file() or not gcc.is_file():raise RuntimeError('missing pinned Nix tools')
tools={str(x):digest(x) for x in (python,gpr,gcc)}
rows=[]
def stage(label,command,seconds):
 verify()
 if any(digest(Path(x))!=h for x,h in tools.items()):raise RuntimeError('tool changed')
 log=OUT/(label+'.log');started_ns=time.monotonic_ns()
 status=capture([str(python),str(HERE/'bounded_exec.py'),*map(str,command)],ROOT,log,seconds)
 elapsed_ns=time.monotonic_ns()-started_ns
 row={'label':label,'command':list(map(str,command)),'status':status,'elapsed_monotonic_ns':elapsed_ns,'timeout_seconds':seconds,'log_sha256':digest(log)};rows.append(row)
 (OUT/'report.json').write_text(json.dumps({'stages':rows,'full_suite_complete':False},indent=2)+'\n')
 verify();return status,log
status,log=stage('runtime-directory',[gcc,'-print-file-name=adainclude'],5)
if status!=0:raise SystemExit(1)
runtime=Path(log.read_text().strip()).resolve()
if not runtime.is_dir() or not str(runtime).startswith('/nix/store/'):raise RuntimeError('unrecognized Ada runtime')
status,log=stage('build',[gpr,'-p','-j1','-v','-P',HERE/(args.profile+'.gpr'),'aslts_ordered_runner.adb'],180)
if status!=0:raise SystemExit(1)
binder_source = 'b__aslts_ordered_runner.adb'
binder_dependencies = {binder_source, 'b__aslts_ordered_runner.ads'}
compiled=[];compiler_tools={}
for line in log.read_text().splitlines():
 try:words=shlex.split(line)
 except ValueError:continue
 if '-c' not in words:continue
 sources=[x for x in words if x.lower().endswith(('.adb','.ads'))]
 if not sources:continue
 if len(sources)!=1:raise RuntimeError('ambiguous compiler source '+line)
 f=Path(sources[0])
 if not f.is_absolute() and f.parent==Path('.') and f.name == binder_source:
  f=(OBJ/f).resolve()
 else:f=f.resolve() if f.is_absolute() else (ROOT/f).resolve()
 generated=f.parent==OBJ.resolve() and f.name == binder_source
 if not generated and (f.name not in local or f!=local[f.name]):raise RuntimeError('unselected compiler source '+str(f))
 tool=Path(words[0]);tool=tool.resolve() if tool.is_absolute() else Path(shutil.which(words[0]) or '').resolve()
 if not tool.is_file():raise RuntimeError('missing actual compiler')
 if tool != gcc:raise RuntimeError('actual compiler differs from pinned gcc '+str(tool))
 compiler_tools[str(tool)]=digest(tool);compiled.append({'path':str(f),'sha256':digest(f),'generated':generated})
if not any(Path(x['path']).name=='aslts_ordered_runner.adb' for x in compiled):raise RuntimeError('main compilation absent')
deps={}
for ali in OBJ.glob('*.ali'):
 for line in ali.read_text(errors='replace').splitlines():
  if not line.startswith('D '):continue
  name=shlex.split(line)[1];base=Path(name).name
  if base in local:f=local[base]
  elif (runtime/base).is_file():f=runtime/base
  elif (OBJ/base).is_file() and base in binder_dependencies:f=OBJ/base
  else:raise RuntimeError('unresolved ALI dependency '+name)
  if Path(name).is_absolute() and Path(name).resolve()!=f.resolve():raise RuntimeError('ALI path mismatch '+name)
  deps[str(f.resolve())]=digest(f)
for required in ('aml_execute.adb','aml_namespace.adb','acpi_service_core.adb'):
 if str(local[required]) not in deps:raise RuntimeError('required Core dependency absent '+required)
(OUT/'source-selection.json').write_text(json.dumps({'compiled':compiled,'dependencies':deps,'compiler_tools':compiler_tools},indent=2)+'\n')
exe=BIN/'aslts_ordered_runner';binary=digest(exe)
cases=corpus['cases']
if [c['case'] for c in cases]!=['local-n64']:raise RuntimeError('selection must be exact original local n64')
for c in cases:
 if digest(exe)!=binary:raise RuntimeError('binary changed')
 status,log=stage(c['case'],[exe,ROOT/c['aml']],64)
 good=status==0 and len(re.findall(r'^RESULT\s+0\s*$',log.read_text(),re.M))==2 and len(re.findall(r'^ERRS_RESULT\s+0\s*$',log.read_text(),re.M))==2 and log.read_text().count('MN00_STATUS RETURNED')==2 and log.read_text().count('ERRS_STATUS RETURNED')==2 and re.findall(r'^COMPLETION RMRC\s+(\d+) ALL_RECORDS_CMO0_PASS RETAINED_RESTORED$',log.read_text(),re.M)==['1','2']
 rows[-1]['selected_case_pass']=good
 if not good:
  (OUT/'report.json').write_text(json.dumps({'stages':rows,'full_suite_complete':False},indent=2)+'\n');raise SystemExit(1)
 if digest(exe)!=binary:raise RuntimeError('binary changed after run')
verify()
(OUT/'audit.json').write_text(json.dumps({'source_manifest_sha256':hashlib.sha256(manifest_bytes).hexdigest(),'inputs':manifest,'tools':tools|compiler_tools,'binary':{'path':str(exe),'sha256':binary},'artifacts':{str(f.relative_to(OUT)):digest(f) for f in sorted(OUT.rglob('*')) if f.is_file()},'stages':rows,'selection':args.selection,'full_suite_complete':False,'unselected_entrypoints':corpus['not_run']},indent=2)+'\n')
print(json.dumps({'selected_passed':len(cases),'full_suite_complete':False,'audit':str(OUT/'audit.json')}))
