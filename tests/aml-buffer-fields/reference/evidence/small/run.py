import pathlib,subprocess,resource,json,re,hashlib,shutil
D=pathlib.Path(__file__).resolve().parent
O=D/'results';O.mkdir()
def cap():
 resource.setrlimit(resource.RLIMIT_AS,(128*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_FSIZE,(1024*1024,)*2)
def run(cmd,key):
 log=O/(key+'.log')
 with log.open('wb') as f:
  try:
   proc=subprocess.run(cmd,stdout=f,stderr=subprocess.STDOUT,timeout=5,preexec_fn=cap);rc=proc.returncode
  except subprocess.TimeoutExpired:rc='timeout'
 raw=log.read_bytes();assert len(raw)<=1024*1024
 return rc,raw.decode('utf-8',errors='replace')
tools={x:str(pathlib.Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin')/x) for x in ('iasl','acpiexec')}
assert all(tools.values()),tools
(O/'tools.json').write_text(json.dumps({x:{'path':v,'sha256':hashlib.sha256(pathlib.Path(v).read_bytes()).hexdigest(),'version':run([v,'-v'],x+'-version')[1]} for x,v in tools.items()},indent=2))
rows=[]
import atexit
atexit.register(lambda: (O/'observations.json').write_text(json.dumps(rows,indent=2)+'\n'))
for rev in (1,2):
 for c in json.loads((D/'cases.json').read_text()):
  key=str(rev)+'-'+c['name'];src=D/'cases'/(key+'.asl')
  row={'key':key,'expectation':None};rows.append(row)
  rc,out=run([tools['iasl'],'-oa',str(src)],key+'-compile');row['compile_returncode']=rc
  if rc!=0:row['classification']='compile_nonzero_pending_review' if isinstance(rc,int) else 'compile_timeout';continue
  aml=src.with_suffix('.aml'); assert aml.read_bytes()[:4]==b'DSDT' and aml.read_bytes()[8]==rev; row['signature']='DSDT';row['revision']=rev;row['aml_sha256']=hashlib.sha256(aml.read_bytes()).hexdigest()
  crc,ctrl=run([tools['acpiexec'],'-b','execute CTRL',str(aml)],key+'-control')
  assert crc==0 and re.findall(r'\[Integer\] = ([0-9A-F]+)',ctrl)==['0000000000000001'],key
  wrc,wout=run([tools['acpiexec'],'-b','execute WIDT',str(aml)],key+'-width');row['width_returncode']=wrc;row['width_witness']=re.findall(r'\[Integer\] = ([0-9A-F]+)',wout);assert wrc==0 and row['width_witness']==[('0000000000000000' if rev==1 else '00000000FFFFFFFF')],(key,row['width_witness']);row['verified_bits']=32 if rev==1 else 64
  drc,dout=run([tools['iasl'],'-d',str(aml)],key+'-disassemble');row['disassembly_returncode']=drc
  dsl=aml.with_suffix('.dsl').read_text()
  match=re.search(r'Method \(TEST\b',dsl)
  assert drc==0 and match,key
  opening=dsl.index('{',match.end());depth=1;end=opening+1
  while depth:
   depth += (dsl[end]=='{')-(dsl[end]=='}');end+=1
  method=dsl[opening:end]
  row['method_opcode_verified']=bool(re.search(r'\b'+c['method_opcode']+r'\s*\(',method))
  assert row['method_opcode_verified'],(key,method)
  (O/(key+'-TEST.dsl')).write_text(method)
  if c['name']=='ARG':
   hm=re.search(r'Method \(HELP\b',dsl);assert hm,key
   hs=dsl.index('{',hm.end());he=hs+1;hd=1
   while hd:hd += (dsl[he]=='{')-(dsl[he]=='}');he+=1
   helper=dsl[hs:he];assert 'CreateByteField' in helper,key
   (O/(key+'-HELP.dsl')).write_text(helper)
   row['helper_opcode_verified']=True

  rc,out=run([tools['acpiexec'],'-b','execute TEST; execute MARK',str(aml)],key+'-execute');row['returncode']=rc
  full_out=out;parts=out.split('Evaluating \\MARK');row['marker_values']=re.findall(r'\[Integer\] = ([0-9A-F]+)',parts[1]) if len(parts)>1 else [];out=parts[0]
  errors=re.findall(r'^.*(?:ACPI (?:Error|Exception):|AE_[A-Z_]+|Failed at AML Offset).*$',full_out,re.M)
  baseline=re.findall(r'^.*(?:ACPI (?:Error|Exception):|AE_[A-Z_]+|Failed at AML Offset).*$',ctrl,re.M)
  row['diagnostics']=errors;row['control_diagnostics']=baseline
  vals=re.findall(r'^\s*\[Integer\] = ([0-9A-F]+)\s*$',out,re.M)
  m=re.search(r'\[Buffer\] Length ([0-9A-F]+) =',out)
  if rc!=0:row['classification']='process_failure';continue
  if m:
   length=int(m[1],16); tail=out[m.end():]; data=[]
   for line in tail.splitlines():
    z=re.match(r'\s*[0-9A-F]{4,}: ((?:[0-9A-F]{2}(?: +|$))+)',line)
    if z:data.extend(int(v,16) for v in z[1].split())
    elif data:break
   assert len(data)==length,(key,length,len(data))
   row.update(kind='buffer',length=length,hex=bytes(data).hex())
  elif vals:
   assert len(vals)==1,key
   row.update(kind='integer',integer=int(vals[0],16))
  else:
   row['ae_statuses']=sorted(set(re.findall(r'AE_[A-Z_]+',out)));row['classification']='runtime_error' if row['ae_statuses'] else 'missing_result';continue
  row['cache_diagnostics']=[e for e in errors if re.fullmatch(r'ACPI Error: [0-9]+ \(0x[0-9A-Fa-f]+\) Outstanding cache allocations \(20260408/uttrack-902\)',e) is not None]
  assert not [e for e in errors if re.fullmatch(r'ACPI Error: [0-9]+ \(0x[0-9A-Fa-f]+\) Outstanding cache allocations \(20260408/uttrack-902\)',e) is None],(key,errors)
  row['classification']='returned'
  exp=None
  if exp is not None:
   row['source_expectation_matches']=(row.get('integer')==exp['integer']) if 'integer' in exp else row.get('hex')==exp[str(32 if rev==1 else 64)]
 (O/'observations.json').write_text(json.dumps(rows,indent=2)+'\n')
(O/'observations.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps({'attempts':len(rows),'classes':{k:sum(r.get('classification')==k for r in rows) for k in set(r.get('classification') for r in rows)}},indent=2))
