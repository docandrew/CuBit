"""Independent reference bytes: actual checksummed immutable AML table, little-endian bit extraction."""
from pathlib import Path
import json,hashlib
D=Path(__file__).resolve().parent
rows=[]
for c in json.loads((D/'cases.json').read_text()):
 for rev in (1,2):
  key=str(rev)+'-'+c['name'];p=D/'cases'/(key+'.aml')
  if not p.exists():rows.append({'key':key,'classification':'no_compiled_table'});continue
  raw=p.read_bytes();bits=c['bit_count'];offset=c['bit_offset'];assert offset+bits<=len(raw)*8
  value=(int.from_bytes(raw,'little')>>offset)&((1<<bits)-1)
  width=32 if rev==1 else 64
  if bits<=width:phases={'TEST':value,'MARK':1,'POST':value}
  else:
   count=(bits+7)//8;data=value.to_bytes(count,'little')
   phases={'TEST':count,'MARK':3,'POST':data[-1]*256+data[0]}
  rows.append({'key':key,'aml_sha256':hashlib.sha256(raw).hexdigest(),'phases':phases,'bit_count':bits,'bit_offset':offset})
print(json.dumps(rows,indent=2))
