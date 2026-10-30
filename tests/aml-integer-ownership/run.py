from pathlib import Path
import argparse, tempfile, os,sys,json,hashlib,shutil,signal,resource,shlex
from hosted_capture import capture,CaptureInterrupted
if not __debug__:raise SystemExit("Python optimization disables required guards; use python3 without -O")
HERE=Path(__file__).resolve().parent
S=HERE.parents[1]
p=argparse.ArgumentParser()
p.add_argument('--mode',choices=['release','checked'],default='release')
p.add_argument('--selection',choices=['wide','conversion','completion'],default='conversion')
args=p.parse_args()
assert os.environ['TMPDIR']=='/home/doc/cubit-build-tmp'
if os.getpriority(os.PRIO_PROCESS,0)<19:os.nice(19-os.getpriority(os.PRIO_PROCESS,0))
resource.setrlimit(resource.RLIMIT_STACK,(67108864,)*2)
def interrupted(signum,frame):raise CaptureInterrupted('integer ownership validation interrupted')
signal.signal(signal.SIGTERM,interrupted);signal.signal(signal.SIGINT,interrupted)
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
manifest=HERE/'source-manifest.json';manifest_hash=sha(manifest)
sourcepins=json.loads(manifest.read_text())
for rel,digest in sourcepins.items():
 q=S/rel
 assert not Path(rel).is_absolute() and '..' not in Path(rel).parts
 assert q==q.resolve(strict=True) and q.resolve().is_relative_to(S) and sha(q)==digest,rel
out=HERE/'results';assert not out.is_symlink();out.mkdir(exist_ok=True)
D=Path(tempfile.mkdtemp(prefix=args.selection+'-'+args.mode+'-',dir=out))
print(D,flush=True)
jobs=[]
for item in json.loads((HERE/'selections.json').read_text())[args.selection]:
 rel=item['main'];label=args.mode+'-'+Path(rel).parent.name+'-'+Path(rel).stem
 P=D/'projects'/label;P.mkdir(parents=True);obj=D/'build'/label/'obj';binary=D/'build'/label/'bin'
 dirs=[S/Path(rel).parent]+[S/x for x in item['extra_sources']]+[S/'userspace/services/acpi/lib',S/'userspace/services/acpi',S/'shared/firmware'];sources={}
 for folder in dirs:
  for q in folder.glob('*.ad?'):
   assert q.name not in sources or sources[q.name]==str(q),q.name
   assert str(q.relative_to(S)) in sourcepins
   sources[q.name]=str(q)
 (P/'sources.json').write_text(json.dumps(sources,indent=2))
 flags=['-O1','-gnat2022','-gnatwa','-gnatwe']+(['-gnatp'] if args.mode=='release' else ['-gnata','-gnato'])+(['-gnatc'] if item['kind']=='semantic' else [])
 (P/'check.gpr').write_text('project Check is\n for Source_Dirs use ('+', '.join(json.dumps(str(x)) for x in dirs)+');\n for Object_Dir use '+json.dumps(str(obj))+';\n for Exec_Dir use '+json.dumps(str(binary))+';\n for Main use ('+json.dumps(Path(rel).name)+');\n package Compiler is\n for Default_Switches ("Ada") use ('+', '.join(json.dumps(x) for x in flags)+');\n end Compiler;\nend Check;\n')
 jobs.append({'label':label,'main':rel,'kind':item['kind'],'object_dir':str(obj),'binary_dir':str(binary)})
(D/'jobs.json').write_text(json.dumps(jobs,indent=2))
tools={n:Path(shutil.which(n)).resolve(strict=True) for n in ['gprbuild','gnatls','gnatbind']};tools['python']=Path(sys.executable).resolve(strict=True)
compiler=Path(shutil.which('gnatbind')).parent/'gcc';assert compiler.is_file();tools['gcc']=compiler
toolpins={str(p):sha(p) for p in tools.values()}
def verify():
 assert sha(manifest)==manifest_hash
 for n,h in sourcepins.items():assert sha(S/n)==h,n
 for n,h in toolpins.items():assert sha(Path(n))==h,n
verify();R=D/'results';R.mkdir();(R/'tool-pins.json').write_text(json.dumps(toolpins,indent=2)+'\n');rows=[];audits=[]
def stage(label,command,timeout):
 verify();rc=capture(command,D,R/(label+'.log'),timeout)
 rows.append({'stage':label,'command':command,'timeout':timeout,'returncode':rc});(R/'outcomes.json').write_text(json.dumps(rows,indent=2)+'\n');print(label,rc,flush=True);verify()
 if rc!=0:raise SystemExit(1)
stage('gprbuild-version',[str(tools['gprbuild']),'--version'],30)
stage('gnat-version',[str(tools['gnatls']),'--version'],30)
for job in json.loads((D/'jobs.json').read_text()):
 label=job['label'];P=D/'projects'/label;sources=json.loads((P/'sources.json').read_text())
 stage(label+'-build',[str(tools['gprbuild']),'-v','-j1','-p']+(['-c'] if job['kind']=='semantic' else [])+['-P',str(P/'check.gpr')],180)
 log=(R/(label+'-build.log')).read_text();assert str(S/job['main']) in log
 compiled=[];generated=[]
 for line in log.splitlines():
  if ' -c -x ada ' not in line:continue
  words=shlex.split(line);assert words[0]==str(compiler),line
  if '-o' in words:
   assert job['kind']=='runtime',line
   oi=words.index('-o');src=Path(words[oi-1]);expected='b__'+Path(job['main']).stem+'.adb'
   assert src==Path(expected) and words[oi+1]==expected.removesuffix('.adb')+'.o',line
   actual=(Path(job['object_dir'])/src).resolve(strict=True);assert actual.parent==(Path(job['object_dir'])).resolve() and actual.is_file()
   generated.append({'path':str(actual),'sha256':sha(actual)});continue
  src=Path(words[-1]);assert src.is_absolute() and str(src) in sources.values(),line
  assert src.is_relative_to(S);compiled.append(str(src.relative_to(S)))
 assert job['main'] in compiled
 alis=list((Path(job['object_dir'])).glob('*.ali'));assert alis,'no ALI artifacts'
 dependencies=set()
 for ali in alis:
  for line in ali.read_text().splitlines():
   if not line.startswith('D '):continue
   name=shlex.split(line)[1]
   if name in sources:
    path=Path(sources[name]);assert sha(path)==sourcepins[str(path.relative_to(S))];dependencies.add(str(path.relative_to(S)))
 assert job['main'] in dependencies
 audits.append({'job':label,'compiled':compiled,'generated_binders':generated,'local_ALI_dependencies':sorted(dependencies),'ALI_count':len(alis)})
 (R/'source-selection.json').write_text(json.dumps(audits,indent=2)+'\n')
 if job['kind']=='runtime':
  binary=Path(job['binary_dir'])/Path(job['main']).stem;toolpins[str(binary)]=sha(binary)
  stage(label+'-run',[str(binary)],30)
verify();art={str(p.relative_to(D)):sha(p) for p in D.rglob('*') if p.is_file()}
(R/'audit.json').write_text(json.dumps({'source_manifest':manifest_hash,'sourcepins':sourcepins,'runner_integrity':'runner, capture and selections included in sourcepins','tools_and_binaries':toolpins,'artifacts_count':len(art),'artifacts':art,'source_selection':audits},indent=2)+'\n')
