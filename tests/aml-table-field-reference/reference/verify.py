"""Reconstruct expectations from pinned normal ACPICA logs and AML headers."""
from pathlib import Path
import hashlib,json,re
D=Path(__file__).resolve().parent
SHAPES={'aligned-1025':(512,8200),'unaligned-1025':(515,8200),'masked-tail-1025':(515,8193),'aligned-8192':(512,65536),'unaligned-8192':(515,65536),'threshold-33':(515,33)}
KEYS={str(rev)+'-'+name for rev in (1,2) for name in SHAPES}
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
def safe(root,name):
 rel=Path(name);p=root/rel
 if rel.is_absolute() or '..' in rel.parts or any(q.is_symlink() for q in [p,*p.parents]):raise ValueError('unsafe evidence path')
 if not p.resolve(strict=True).is_relative_to(root.resolve()):raise ValueError('escape')
 return p

def frames(text,names):
 hits=[list(re.finditer(r'^Evaluating '+re.escape('\\'+n)+r'\s*$',text,re.M)) for n in names]
 if any(len(h)!=1 for h in hits):raise ValueError('raw phase count')
 starts=[h[0].start() for h in hits]
 if starts!=sorted(starts):raise ValueError('raw phase order')
 return {n:text[a:starts[i+1] if i+1<len(starts) else len(text)] for i,(n,a) in enumerate(zip(names,starts))}
def observed(s):
 return re.findall(r'^\s*\[Integer\] = ([0-9A-F]+)\s*$',s,re.M),sorted(set(re.findall(r'\bAE_[A-Z_]+\b',s)))
def verify(root=D):
 data=json.loads((root/'reference.json').read_bytes());rows=data['rows']
 if len(rows)!=12 or {r['key'] for r in rows}!=KEYS:raise ValueError('corpus identity')
 for name,h in data['inputs'].items():
  if sha(safe(root,name))!=h:raise ValueError('evidence drift')
 outcomes=json.loads(safe(root,'evidence/results/observations.json').read_bytes())
 if len(outcomes)!=12 or len({r['key'] for r in outcomes})!=12 or {r['key'] for r in outcomes}!={r['key'] for r in rows}:raise ValueError('outcome scope')
 bykey={r['key']:r for r in outcomes};returned=0
 for row in rows:
  key=row['key'];o=bykey[key]
  if row['revision']!=int(key[0]):raise ValueError('key revision')
  if row['comparison_scope']!='table_field_type_length_endpoints':raise ValueError('scope')
  if row['selection']!='compare' or row['revision']!=o['revision']:raise ValueError('row type')
  if any(o[k]!=0 for k in ['compile_rc','disassemble_rc','control_rc','execute_rc']):raise ValueError('failed normal tool stage')
  if row['aml']!='evidence/cases/'+key+'.aml':raise ValueError('AML correspondence')
  aml=safe(root,row['aml']).read_bytes()
  if len(aml)<36 or aml[8]!=row['revision'] or int.from_bytes(aml[4:8],'little')!=len(aml) or sum(aml)%256:raise ValueError('table header')
  if sha(safe(root,row['aml']))!=o['aml_sha256'] or sha(safe(root,'evidence/cases/'+key+'.dsl'))!=o['disassembly_sha256']:raise ValueError('actual tool artifacts')
  control=safe(root,'evidence/results/'+key+'-control.log').read_text();c=frames(control,['CTRL','WIDT'])
  if re.search(r'\bAE_[A-Z_]+\b',control[:control.index('Evaluating \\CTRL')]):raise ValueError('load error')
  if observed(c['CTRL'])!=(['0000000000000001'],[]) or observed(c['WIDT'])!=([ '0000000000000000' if row['revision']==1 else '00000000FFFFFFFF'],[]):raise ValueError('width witness')
  raw=safe(root,'evidence/results/'+key+'execute.log').read_text();f=frames(raw,['TEST','MARK','POST'])
  if re.search(r'\bAE_[A-Z_]+\b',raw[:raw.index('Evaluating \\TEST')]):raise ValueError('execute load error')
  for phase in ['TEST','MARK','POST']:
   values,statuses=observed(f[phase])
   if statuses or len(values)!=1:raise ValueError('nonpositive raw outcome')
   want={'status':'RETURNED','value':int(values[0],16)}
   if phase=='TEST':returned+=1
   if row['phases'][phase]!=want:raise ValueError('expected value not raw-derived')
 case_rows=json.loads(safe(root,'evidence/cases.json').read_bytes())
 cases={c['name']:c for c in case_rows}
 if len(case_rows)!=6 or set(cases)!=set(SHAPES):raise ValueError('shape inventory')
 if any((c['bit_offset'],c['bit_count'])!=SHAPES[n] for n,c in cases.items()):raise ValueError('shape bit metadata')
 for row in rows:
  c=cases[row['key'][2:]];raw=safe(root,row['aml']).read_bytes();bits=c['bit_count'];offset=c['bit_offset']
  dsl=safe(root,'evidence/cases/'+row['key']+'.dsl').read_text()
  field=dsl[dsl.index('    DataTableRegion'):dsl.index('    Method (CTRL')]
  if 'DataTableRegion (REG0, "DSDT", "CUBIT", "TBLFIELD")' not in field or 'Field (REG0, ByteAcc, NoLock, Preserve)' not in field or 'Offset (0x40)' not in field:raise ValueError('field DSL identity')
  if not re.search(r'FLD0,\s*'+str(bits)+r'\b',field) or bool(re.search(r',\s*3,',field))!=(offset%8==3):raise ValueError('field DSL bits')
  if offset+bits>len(raw)*8:raise ValueError('field outside table')
  value=(int.from_bytes(raw,'little')>>offset)&((1<<bits)-1)
  if bits<=(32 if row['revision']==1 else 64):expect={'TEST':value,'MARK':1,'POST':value}
  else:
   count=(bits+7)//8;v=value.to_bytes(count,'little');expect={'TEST':count,'MARK':3,'POST':v[-1]*256+v[0]}
  if any(row['phases'][n]!={'status':'RETURNED','value':v} for n,v in expect.items()):raise ValueError('independent table bits mismatch')
 if returned!=12:raise ValueError('outcome counts')
 return {'attempts':12,'inputs':len(data['inputs']),'integer_flows':12,'explicit_errors':0}
if __name__=='__main__':print(json.dumps(verify()))
