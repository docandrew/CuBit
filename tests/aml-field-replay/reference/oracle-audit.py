from pathlib import Path
import hashlib,json,re
R=Path('/home/doc/git/.acpi-proof-work/field-core-normal-oracle-proposal-mhs6804e')
O=R/'results'; D=Path(__file__).resolve().parent
H=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
a=json.loads((O/'audit.json').read_text()); refs=json.loads((R/'reference-pins.json').read_text())
for p,h in a['inputs'].items(): assert H(p)==h,p
for p,h in refs.items(): assert H(p)==h,p
for x in a['tools'].values(): assert H(x['path'])==x['sha256'],x
for p,h in a['artifacts'].items(): assert H(O/p)==h,p
rows=json.loads((O/'report.json').read_text()); pred=json.loads((R/'predictions-not-goldens.json').read_text())
assert len(rows)==6
out=[]; scalar=errors=0
for row,prediction in zip(rows,pred):
 assert row['label']==prediction['label']
 assert (row['compile_status'],row['disassembly_status'],row['runtime_status'])==(0,0,0)
 label=row['label']; bits=int(re.search(r'whole(\d+)',label).group(1))
 aml=(O/(label+'.aml')).read_bytes(); assert aml[:4]==b'DSDT'
 assert len(aml)==int.from_bytes(aml[4:8],'little') and sum(aml)%256==0 and aml[8]==row['revision']
 dsl=(O/(label+'.dsl')).read_text(); clean=re.sub(r'/\*.*?\*/|//[^\n]*','',dsl,flags=re.S)
 access={8:'ByteAcc',16:'WordAcc',32:'DWordAcc'}[bits]
 assert re.search(r'Field\s*\(RGN0,\s*'+access+r',\s*NoLock,\s*Preserve\)\s*\{\s*FLD0,\s*'+str(bits)+r'\s*\}',clean)
 assert 'FLD0 = SRC0 ()' in clean and 'FLD0 = BAD0 ()' in clean and 'Local0 = FLD0' in clean
 raw=(O/(label+'-acpiexec.log')).read_text(); blocks=re.split(r'^Evaluating ',raw,flags=re.M)[1:]
 # Split delimiter consumes the initial backslash only; each block starts with method.
 assert len(blocks)==10,(label,len(blocks))
 obs=[]
 for block,expected in zip(blocks,prediction['predictions_not_goldens']):
  name=block.splitlines()[0].strip().lstrip(chr(92)); assert name==expected['method'],(name,expected)
  failed=re.findall(r'Evaluation of \\'+name+r' failed with status ([A-Z_]+)',block)
  numbers=re.findall(r'^  \[Integer\] = ([0-9A-F]+)$',block,re.M)
  if 'error' in expected:
   assert failed==[expected['error']] and not numbers,(label,block)
   actual={'method':name,'error':failed[0]};errors+=1
  else:
   assert not failed and len(numbers)==1 and int(numbers[0],16)==expected['integer'],(label,block)
   actual={'method':name,'integer':int(numbers[0],16),'hex':numbers[0]};scalar+=1
  actual['raw_block']=block;obs.append(actual)
 out.append({'label':label,'aml_sha256':H(O/(label+'.aml')),'length':len(aml),'revision':aml[8],'checksum_mod256':sum(aml)%256,'retained_width':bits,'access':access,'ordered':obs,'compiler_log':str(O/(label+'-iasl.log')),'literal_note':'revision1 compiler truncates high literal to0x9ABCDEF0; revision2 retains0x123456789ABCDEF0'})
assert scalar==48 and errors==12
(D/'observations.json').write_text(json.dumps(out,indent=2)+'\n')
receipt={'session':67684,'terminal_exit':0,'terminal_chunk':'1f061d','sandbox_prelaunch_denial':'92fbcc; Nix cache lock only, runner not started','counts':{'inputs':len(a['inputs']),'reference_pins':len(refs),'tools':len(a['tools']),'artifacts':len(a['artifacts']),'tables':6,'zero_stages':18,'scalar_observations':scalar,'expected_error_observations':errors},'raw_audit':str(O/'audit.json'),'raw_audit_sha256':H(O/'audit.json'),'observations_sha256':H(D/'observations.json'),'scope':'normal simulated-memory AML value/effect evidence; no CuBit/native/bus-count conformance; registered deriver provenance gap retained; cache diagnostics preserved','tool_pins':a['tools']}
(D/'review.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt['counts']))
