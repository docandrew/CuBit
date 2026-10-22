from pathlib import Path
import tempfile,shutil,json,hashlib
from verify import verify,D
checks=0
assert verify()['attempts']==12;checks+=1
with tempfile.TemporaryDirectory(prefix='table-field-guards-') as folder:
 root=Path(folder)/'reference';shutil.copytree(D,root,ignore=shutil.ignore_patterns('__pycache__'))
 ref=root/'reference.json';original=ref.read_bytes()
 for mutation in ['duplicate_row','missing_row','unknown_key','revision','value','scope','duplicate_outcome','duplicate_outcome_fixed','execute_preamble','changed_bits','duplicate_shape','dsl_bits']:
  data=json.loads(original);target=None;before=None
  if mutation=='duplicate_row':data['rows'][1]=data['rows'][0]
  elif mutation=='missing_row':data['rows'].pop()
  elif mutation=='unknown_key':data['rows'][0]['key']='1-unknown'
  elif mutation=='revision':data['rows'][0]['revision']=2
  elif mutation=='value':data['rows'][0]['phases']['POST']['value']+=1
  elif mutation=='scope':data['rows'][0]['comparison_scope']='full_buffer_equality'
  else:
   name=('evidence/results/observations.json' if mutation.startswith('duplicate_outcome') else 'evidence/cases.json' if mutation in ['changed_bits','duplicate_shape'] else 'evidence/cases/'+data['rows'][0]['key']+'.dsl' if mutation=='dsl_bits' else 'evidence/results/'+data['rows'][0]['key']+'execute.log')
   target=root/name;before=target.read_bytes()
   if mutation=='execute_preamble':target.write_bytes(b'load failure AE_AML_OPERAND_TYPE\n'+before)
   elif mutation=='dsl_bits':
    import re
    target.write_text(re.sub(r'(FLD0,\s*)8200',r'\g<1>8201',before.decode()))
   else:
    obj=json.loads(before)
    if mutation=='duplicate_outcome':obj.append(obj[0])
    elif mutation=='duplicate_outcome_fixed':obj[1]=obj[0]
    elif mutation=='changed_bits':obj[0]['bit_offset']+=1
    else:obj[1]=obj[0]
    target.write_text(json.dumps(obj))
   data['inputs'][name]=hashlib.sha256(target.read_bytes()).hexdigest()
   if mutation=='dsl_bits':
    # Repin the tool observation too so the DSL structural check is exercised.
    obs=root/'evidence/results/observations.json';obs_before=obs.read_bytes();o=json.loads(obs_before);o[0]['disassembly_sha256']=data['inputs'][name];obs.write_text(json.dumps(o));data['inputs']['evidence/results/observations.json']=hashlib.sha256(obs.read_bytes()).hexdigest()
  ref.write_text(json.dumps(data))
  try:verify(root)
  except ValueError:checks+=1
  else:raise AssertionError('accepted mutation '+mutation)
  if target:target.write_bytes(before)
  if mutation=='dsl_bits':obs.write_bytes(obs_before)
  ref.write_bytes(original)
print(json.dumps({'checks':checks,'status':'PASS','scope':'raw evidence and fixed field-shape guards'}))
