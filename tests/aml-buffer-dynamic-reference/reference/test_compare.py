import json
from pathlib import Path
from compare import parse,compare,encode,PHASES
D=Path(__file__).resolve().parent
all_rows=json.loads((D/'reference.json').read_bytes())['rows'];rows=[r for r in all_rows if r['selection']=='compare'];checks=0
assert len(all_rows)==20 and len(rows)==20 and len({r['key'] for r in rows})==20
assert sum(r['phases']['TEST']['status']=='RETURNED' for r in rows)==16
assert {r['phases']['TEST'].get('acpica_error') for r in rows}=={None,'AE_AML_DIVIDE_BY_ZERO','AE_AML_BUFFER_LIMIT'}
def reject(raw,row):
    global checks
    checks+=1
    try:compare(parse(raw),row)
    except ValueError:return
    raise AssertionError('accepted mutation: '+repr(raw))
for row in rows:
    good=encode(row);assert compare(parse(good),row);checks+=1
    for cut in range(len(good)):reject(good[:cut],row)
    lines=good.splitlines(keepends=True)
    for i in range(len(lines)):
        reject(b''.join(lines[:i]+lines[i+1:]),row)
        reject(b''.join(lines[:i]+[lines[i]]+lines[i:]),row)
    for bad in [good+b'\n',good+b'junk',b'junk\n'+good,good.replace(b'\n',b'\r\n'),good+b'\0',good+b'\xff',good.replace(b'REPLAY 1',b'REPLAY 2'),good.replace(b'RETURNED',b'UNKNOWN'),b''.join([lines[0],lines[2],lines[1],*lines[3:]])]:reject(bad,row)
    for phase in PHASES:
        line=lines[PHASES.index(phase)+1]
        for value in [b'NONE',b'-1',b'+1',b'00',b'01',b'18446744073709551616']:
            reject(good.replace(line,phase.encode()+b' RETURNED '+value+b'\n'),row)
        reject(good.replace(line,phase.encode()+b' UNSUPPORTED_VALUE 1\n'),row)
        value=row['phases'][phase]['value']
        reject(good.replace(line,phase.encode()+b' RETURNED '+str(0 if value is None else value+1).encode()+b'\n'),row)
    reject(good.replace(lines[1],b'TEST UNSUPPORTED NONE\n'),row)
    if row['phases']['TEST']['status']!='RETURNED':
        wrong=b'UNSUPPORTED_VALUE' if row['phases']['TEST']['status']=='DIVISION_BY_ZERO' else b'DIVISION_BY_ZERO'
        reject(good.replace(lines[1],b'TEST '+wrong+b' NONE\n'),row)
    for phase in PHASES:
        line=lines[PHASES.index(phase)+1]
        for status in ['OBJECT_RETURNED','REFERENCE_RETURNED','METADATA_RETURNED']:
            reject(good.replace(line,(phase+' '+status+' NONE\n').encode()),row)
    if row['phases']['TEST']['status']=='RETURNED':reject(good.replace(lines[1],b'TEST UNSUPPORTED_VALUE NONE\n'),row)
for row in all_rows:
    if row['selection']=='compiler_rejected':
        checks+=1
        try:compare({},row)
        except ValueError:pass
        else:raise AssertionError('executed compiler rejection')
print(json.dumps({'checks':checks,'compiled_rows':20,'preserved_compiler_rejections':0,'integer_flows':16,'explicit_error_mappings':4,'status':'PASS','scope':'synthetic protocol only'}))

# An expected row cannot relabel unrelated errors as the target-only projection.
import copy
for original in rows:
    for mutation in ('scope','error','key'):
        bad=copy.deepcopy(original)
        if mutation=='scope':bad['comparison_scope']='coarse_target_failure_status_projection' if original['comparison_scope']!='coarse_target_failure_status_projection' else 'exact_integer_flow'
        elif mutation=='error':bad['phases']['TEST']={'status':'UNSUPPORTED_VALUE','value':None,'acpica_error':'AE_AML_BUFFER_LIMIT'};bad['comparison_scope']='coarse_target_failure_status_projection';bad['key']='1-count-failure'
        else:bad['key']='1-unrelated';bad['phases']['TEST']={'status':'UNSUPPORTED_VALUE','value':None,'acpica_error':'AE_AML_BUFFER_LIMIT'};bad['comparison_scope']='coarse_target_failure_status_projection'
        reject(encode(bad),bad)
print(json.dumps({'checks_including_scope':checks,'status':'PASS'}))
