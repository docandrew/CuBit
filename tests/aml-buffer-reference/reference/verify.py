"""Reconstruct expectations from pinned normal ACPICA logs and AML headers."""
from pathlib import Path
import hashlib,json,re
D=Path(__file__).resolve().parent
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
 if len(rows)!=18 or len({r['key'] for r in rows})!=18:raise ValueError('corpus identity')
 for name,h in data['inputs'].items():
  if sha(safe(root,name))!=h:raise ValueError('evidence drift')
 outcomes=json.loads(safe(root,'evidence/results/observations.json').read_bytes())
 if len(outcomes)!=18 or len({r['key'] for r in outcomes})!=18 or {r['key'] for r in outcomes}!={r['key'] for r in rows}:raise ValueError('outcome scope')
 bykey={r['key']:r for r in outcomes};expected_errors={'AE_AML_DIVIDE_BY_ZERO':'DIVISION_BY_ZERO','AE_AML_BUFFER_LIMIT':'EMPTY_BUFFER'};returned=0;errors=0
 for row in rows:
  key=row['key'];o=bykey[key]
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
   if statuses:
    if phase!='TEST' or len(statuses)!=1 or statuses[0] not in expected_errors or values:raise ValueError('unexpected raw error')
    want={'status':expected_errors[statuses[0]],'value':None,'acpica_error':statuses[0]};errors+=1
   else:
    if len(values)!=1:raise ValueError('missing integer')
    want={'status':'RETURNED','value':int(values[0],16)}
    if phase=='TEST':returned+=1
   if row['phases'][phase]!=want:raise ValueError('expected value not raw-derived')
 if (returned,errors)!=(14,4):raise ValueError('outcome counts')
 return {'attempts':18,'inputs':len(data['inputs']),'integer_flows':14,'explicit_errors':4}
if __name__=='__main__':print(json.dumps(verify()))
