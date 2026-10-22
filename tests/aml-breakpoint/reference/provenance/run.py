from pathlib import Path
import sys
if not __debug__:raise SystemExit('Python optimization forbidden')
import hashlib,json,signal,re
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
   aml=pref.with_suffix('.aml');item['aml_sha256']=H(aml)
   dlog=O/(c['label']+'-disassembly.log')
   item['disassembly_status']=capture([sys.executable,str(R/'bounded_tool.py'),tools['iasl']['path'],'-d',str(aml)],cwd=O,log=dlog,timeout=5,limit=1048576)
   item['disassembly_log']=str(dlog);verify()
   assert H(aml)==item['aml_sha256'],'disassembly changed emitted AML'
   (O/'report.json').write_text(json.dumps(rows,indent=2,default=str)+'\n')
   log=O/(c['label']+'-acpiexec.log');item['runtime_status']=capture([sys.executable,str(R/'bounded_tool.py'),tools['acpiexec']['path'],'-b',c['batch'],str(pref)+'.aml'],cwd=O,log=log,timeout=5,limit=1048576);item['runtime_log']=str(log);verify()
   assert H(aml)==item['aml_sha256'],'execution changed emitted AML'
   dsl=pref.with_suffix('.dsl')
   if item['disassembly_status']==0 and dsl.is_file():
    item['disassembly_dsl']=str(dsl);item['disassembly_sha256']=H(dsl)
    # Lexical disassembly evidence only, not a reachability or equivalence claim.
    text=re.sub(r'/\*.*?\*/|//[^\n]*','',dsl.read_text(errors='replace'),flags=re.S)
    item['breakpoint_mentions']=len(re.findall(r'\bBreakPoint\b',text,re.I))
    item['opcode_classification']=('breakpoint_in_disassembly' if item['breakpoint_mentions'] else 'no_breakpoint_in_successful_disassembly')
   else:item['opcode_classification']='disassembly_unavailable; no opcode-presence inference'
  else:item['classification']='compiler_process_failure; inspect diagnostic before syntactic inference'
  (O/'report.json').write_text(json.dumps(rows,indent=2,default=str)+'\n')
  print(c['label'],rc,item['runtime_status'],flush=True)
finally:
 verify();(O/'audit.json').write_text(json.dumps({'inputs':pins,'tools':tools,'artifacts':{str(p.relative_to(O)):H(p) for p in O.rglob('*') if p.is_file() and p.name!='audit.json'},'scope':'normal BreakPoint oracle observations only; host debugger/OS policy may differ from CuBit non-debug Noop; process0 is not semantic equivalence'},indent=2)+'\n')
