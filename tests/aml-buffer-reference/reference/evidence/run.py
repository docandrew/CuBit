from pathlib import Path
import subprocess,resource,json,re,hashlib,os,signal
D=Path(__file__).resolve().parent
O=D/'results';O.mkdir()
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
inputs=json.loads((D/'run-inputs.json').read_text())
def verify():
 for f,h in inputs.items():assert digest(D/f)==h,f
verify()
pins=json.loads((D/'tool-pins.json').read_text())
for p in pins.values():assert digest(Path(p['path']))==p['sha256']
def cap():
 resource.setrlimit(resource.RLIMIT_AS,(128*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_STACK,(8*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_FSIZE,(1024*1024,)*2)
def run(cmd,key):
 log=O/(key+'.log')
 with log.open('wb') as f:
  p=subprocess.Popen(cmd,cwd=D,stdout=f,stderr=subprocess.STDOUT,start_new_session=True,preexec_fn=cap)
  try:rc=p.wait(timeout=5)
  except subprocess.TimeoutExpired:
   os.killpg(p.pid,signal.SIGKILL);p.wait();rc='timeout'
  except BaseException:
   try:os.killpg(p.pid,signal.SIGKILL)
   except ProcessLookupError:pass
   p.wait();raise
 raw=log.read_bytes();assert len(raw)<=1024*1024
 return rc,raw.decode(errors='replace')
tools={k:v['path'] for k,v in pins.items()}
for name,path in tools.items():run([path,'-v'],name+'-version')
def framed(text,names):
 starts=[]
 for n in names:
  hits=list(re.finditer('^'+re.escape('Evaluating '+chr(92)+n)+r'\s*$',text,re.M))
  if len(hits)!=1:return None
  starts.append(hits[0].start())
 if starts!=sorted(starts):return None
 return {n:text[a:(starts[i+1] if i+1<len(starts) else len(text))] for i,(n,a) in enumerate(zip(names,starts))}
def observe(segment):
 return {'values':re.findall(r'^\s*\[Integer\] = ([0-9A-F]+)\s*$',segment,re.M),'ae_statuses':sorted(set(re.findall(r'\bAE_[A-Z_]+\b',segment))),'raw_reference_lines':[l for l in segment.splitlines() if 'Reference' in l]}
rows=[]
for rev in (1,2):
 for c in json.loads((D/'cases.json').read_text()):
  verify();key=f'{rev}-{c["name"]}';src=D/'cases'/(key+'.asl')
  row={'key':key,'revision':rev,'question':c['question']};rows.append(row)
  rc,out=run([tools['iasl'],'-oa',str(src)],key+'-compile');row['compile_rc']=rc
  if rc!=0:row['classification']='compiler_rejected' if isinstance(rc,int) and rc>0 else 'compile_process_failure'
  else:
   aml=src.with_suffix('.aml');raw=aml.read_bytes();row['aml_sha256']=digest(aml);row['actual_revision']=raw[8];row['table_length']=int.from_bytes(raw[4:8],'little');row['checksum_valid']=sum(raw)%256==0;assert raw[8]==rev and row['table_length']==len(raw) and row['checksum_valid']
   rc,out=run([tools['iasl'],'-d',str(aml)],key+'-disassemble');row['disassemble_rc']=rc
   dsl=aml.with_suffix('.dsl');row['disassembly_sha256']=digest(dsl) if dsl.exists() else None
   row['disassembled_operations']=sorted(set(re.findall(r'\b(Buffer|Name|Store|Return|Package|Index|SizeOf|ToInteger|Divide|Increment)\s*\(',dsl.read_text()))) if dsl.exists() else []
   row['disassembly_valid']=row['disassemble_rc']==0 and dsl.exists()
   row['disassembled_buffer_counts']=re.findall(r'Buffer\s*\(([^)]*)\)',dsl.read_text()) if dsl.exists() else []
   rc,ctrl=run([tools['acpiexec'],'-b','execute CTRL;execute WIDT',str(aml)],key+'-control');row['control_rc']=rc
   frames=framed(ctrl,['CTRL','WIDT']);row['controls']={n:observe(v) for n,v in frames.items()} if frames else None
   row['load_errors']=sorted(set(re.findall(r'\bAE_[A-Z_]+\b',ctrl.split('Evaluating \\CTRL')[0])))
   valid=rc==0 and frames is not None and not row['load_errors'] and row['controls']['CTRL']['values']==['0000000000000001'] and row['controls']['WIDT']['values']==['0000000000000000' if rev==1 else '00000000FFFFFFFF']
   if not valid:row['classification']='control_or_load_failure'
   else:
    command='execute TEST;execute MARK;execute POST';row['execution_commands']=command
    rc,out=run([tools['acpiexec'],'-b',command,str(aml)],key+'execute');row['execute_rc']=rc
    frames=framed(out,['TEST','MARK','POST']);row['frames_complete']=frames is not None
    row['observations']={n:observe(v) for n,v in frames.items()} if frames else None
    row['all_ae_statuses']=sorted(set(re.findall(r'\bAE_[A-Z_]+\b',out)))
    row['classification']='process_failure' if rc!=0 else 'incomplete_framing' if frames is None else 'runtime_error' if row['observations']['TEST']['ae_statuses'] else 'returned' if len(row['observations']['TEST']['values'])==1 else 'missing_result'
  (O/'observations.json').write_text(json.dumps(rows,indent=2)+'\n');print(key,row['classification'],flush=True)
verify()
summary={'attempts':len(rows),'classes':{k:sum(r['classification']==k for r in rows) for k in sorted(set(r['classification'] for r in rows))},'scope':'large Buffer normal ASL: recorded scalar observables and count side effects, no arbitrary-size allocation claim'}
assert len(rows)==18
(O/'summary.json').write_text(json.dumps(summary,indent=2)+'\n');print(json.dumps(summary),flush=True)
