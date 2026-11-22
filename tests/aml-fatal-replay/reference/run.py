from pathlib import Path
import sys
if not __debug__:raise SystemExit('Python optimization forbidden')
import hashlib,json,signal
from hosted_capture import capture,CaptureInterrupted
R=Path(__file__).resolve().parent
signal.signal(signal.SIGTERM,lambda *a: (_ for _ in ()).throw(CaptureInterrupted('TERM')))
signal.signal(signal.SIGINT,lambda *a: (_ for _ in ()).throw(CaptureInterrupted('INT')))
H=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
pins=json.loads((R/'inputs.json').read_text());tools=json.loads((R/'tools.json').read_text())
tools['python']={'path':str(Path(sys.executable).resolve()),'sha256':H(Path(sys.executable).resolve())}
def verify():
 for p,h in pins.items():assert H(p)==h,p
 for x in tools.values():assert H(x['path'])==x['sha256']
verify();O=R/'results';O.mkdir();rows=[]
try:
 for c in json.loads((R/'cases.json').read_text()):
  pref=O/c['label'];log=O/(c['label']+'-iasl.log')
  cmd=[sys.executable,str(R/'bounded_tool.py'),tools['iasl']['path'],'-p',str(pref),str(R/c['source'])]
  rc=capture(cmd,cwd=O,log=log,timeout=5,limit=1048576);verify()
  item={**c,'compile_status':rc,'compile_log':str(log),'runtime_status':None};rows.append(item)
  if rc==0 and pref.with_suffix('.aml').is_file():
   dlog=O/(c['label']+'-disassemble.log')
   item['disassembly_status']=capture([sys.executable,str(R/'bounded_tool.py'),tools['iasl']['path'],'-d',str(pref)+'.aml'],cwd=O,log=dlog,timeout=5,limit=1048576);verify()
   dsl=pref.with_suffix('.dsl')
   item['fatal_retained']=item['disassembly_status']==0 and dsl.is_file() and 'Fatal (' in dsl.read_text()
   if not item['fatal_retained']:
    item['classification']='disassembly missing Fatal; runtime not executed'
    (O/'report.json').write_text(json.dumps(rows,indent=2)+'\n')
    raise SystemExit(1)
   log=O/(c['label']+'-acpiexec.log');item['runtime_status']=capture([sys.executable,str(R/'bounded_tool.py'),tools['acpiexec']['path'],'-b',c['batch'],str(pref)+'.aml'],cwd=O,log=log,timeout=5,limit=1048576);item['runtime_log']=str(log);verify()
  else:item['classification']='compiler_process_failure; inspect diagnostic before syntactic inference'
  (O/'report.json').write_text(json.dumps(rows,indent=2,default=str)+'\n')
  print(c['label'],rc,item['runtime_status'],flush=True)
finally:
 verify();(O/'audit.json').write_text(json.dumps({'inputs':pins,'tools':tools,'artifacts':{str(p.relative_to(O)):H(p) for p in O.rglob('*') if p.is_file() and p.name!='audit.json'},'scope':'normal Fatal interpreter observations only; Unix host signal no-op and CONTINUE_ON_FATAL policy must be distinguished; no OS delivery claim; process0 is not semantic success'},indent=2)+'\n')
