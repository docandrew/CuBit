from pathlib import Path
import json,shutil,tempfile,hashlib
from verify import verify,safe,D
checks=0
expected={'attempts':18,'inputs':136,'integer_flows':14,'explicit_errors':4}
assert verify()==expected;checks+=1
with tempfile.TemporaryDirectory(prefix='large-buffer-guards-') as folder:
 root=Path(folder)/'reference';shutil.copytree(D,root,ignore=shutil.ignore_patterns('__pycache__'))
 assert verify(root)==expected;checks+=1
 ref=root/'reference.json';original=ref.read_bytes();data=json.loads(original);name=next(iter(data['inputs']));target=root/name;raw=target.read_bytes()
 for op in ['modify','remove']:
  if op=='modify':target.write_bytes(raw+b'x')
  else:target.unlink()
  try:verify(root)
  except (ValueError,FileNotFoundError):checks+=1
  else:raise AssertionError('changed evidence accepted')
  target.write_bytes(raw)
 for name in ['../outside','/tmp/outside']:
  try:safe(root,name)
  except ValueError:checks+=1
  else:raise AssertionError('unsafe path accepted')
 for mutation in ['value','error','revision','duplicate','missing','aml']:
  data=json.loads(original)
  if mutation=='value':data['rows'][0]['phases']['TEST']['value']+=1
  elif mutation=='error':next(r for r in data['rows'] if r['phases']['TEST']['value'] is None)['phases']['TEST']['status']='UNSUPPORTED_VALUE'
  elif mutation=='revision':data['rows'][0]['revision']=2
  elif mutation=='duplicate':data['rows'][1]=data['rows'][0]
  elif mutation=='missing':data['rows'].pop()
  else:data['rows'][0]['aml']=data['rows'][1]['aml']
  ref.write_text(json.dumps(data))
  try:verify(root)
  except ValueError:checks+=1
  else:raise AssertionError('bad reference accepted '+mutation)
  ref.write_bytes(original)
 # Coherently repin altered raw evidence so semantic checks must reject it.
 for mutation in ['execute_load_error','duplicate_outcome','duplicate_key_at_fixed_count']:
  data=json.loads(original)
  name=('evidence/results/'+data['rows'][0]['key']+'execute.log'
        if mutation=='execute_load_error' else 'evidence/results/observations.json')
  target=root/name;before=target.read_bytes()
  if mutation=='execute_load_error':target.write_bytes(b'Load failure AE_AML_OPERAND_TYPE\n'+before)
  else:
   outcomes=json.loads(before)
   if mutation=='duplicate_outcome':outcomes.append(outcomes[0])
   else:outcomes[1]=outcomes[0]
   target.write_text(json.dumps(outcomes))
  data['inputs'][name]=hashlib.sha256(target.read_bytes()).hexdigest()
  ref.write_text(json.dumps(data))
  try:verify(root)
  except ValueError:checks+=1
  else:raise AssertionError('coherently repinned bad evidence accepted '+mutation)
  target.write_bytes(before);ref.write_bytes(original)
print(json.dumps({'checks':checks,'status':'PASS','scope':'synthetic evidence guards'}))
