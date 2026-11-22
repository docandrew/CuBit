import json,copy
from pathlib import Path
from compare import parse,compare,encode,PHASES
rows=json.loads((Path(__file__).resolve().parent/'reference.json').read_text())['rows'];checks=0
def reject(raw,row):
 global checks
 checks+=1
 try:compare(parse(raw),row)
 except ValueError:return
 raise AssertionError('accepted mutation')
for row in rows:
 good=encode(row);assert compare(parse(good),row);checks+=1
 for cut in range(len(good)):reject(good[:cut],row)
 lines=good.splitlines(keepends=True)
 for i in range(5):
  reject(b''.join(lines[:i]+lines[i+1:]),row)
  reject(b''.join(lines[:i]+[lines[i]]+lines[i:]),row)
 for raw in [good+b'\n',good+b'junk',good.replace(b'\n',b'\r\n'),good+b'\xff',good.replace(b'REPLAY 1',b'REPLAY 2'),b''.join([lines[0],lines[2],lines[1],*lines[3:]])]:reject(raw,row)
 for phase in PHASES:
  line=lines[PHASES.index(phase)+1]
  for value in ['NONE','-1','+1','00','01','18446744073709551616',str(row['phases'][phase]['value']+1)]:reject(good.replace(line,(phase+' RETURNED '+value+'\n').encode()),row)
  for status in ['UNSUPPORTED_VALUE','EMPTY_BUFFER','DIVISION_BY_ZERO','OBJECT_RETURNED','REFERENCE_RETURNED','METADATA_RETURNED']:
   for value in ['NONE','0']:reject(good.replace(line,(phase+' '+status+' '+value+'\n').encode()),row)
 bad=copy.deepcopy(row);bad['comparison_scope']='full_buffer_equality';reject(good,bad)
print(json.dumps({'checks':checks,'status':'PASS','scope':'positive table-field protocol only'}))
