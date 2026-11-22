"""Make a private configuration-only initrd variant and explicit metrics seeds."""
from pathlib import Path
import argparse, hashlib, json, os
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--platform-seed',type=Path,required=True)
p.add_argument('--metrics-seed',type=Path,required=True)
p.add_argument('--trace-observer',type=Path,required=True)
p.add_argument('--publication-client',type=Path,required=True)
p.add_argument('output',type=Path)
a=p.parse_args()
if not os.environ.get('IN_NIX_SHELL'):p.error('Use Nix')
out=a.output.resolve();out.mkdir(parents=True,exist_ok=False)
seed=out/'seed';seed.mkdir();metrics=out/'metrics';metrics.mkdir();inputs={}
def sha(data):return hashlib.sha256(data).hexdigest()
def copy(source,target):
 data=source.read_bytes();inputs[str(source.resolve())]=sha(data);target.write_bytes(data)
def decode(data):
 result=[];pos=0
 while pos+110<=len(data):
  header=data[pos:pos+110];assert header[:6]==b'070701'
  values=[int(header[6+i*8:14+i*8],16) for i in range(13)]
  name=data[pos+110:pos+110+values[11]];assert name.endswith(b'\0')
  start=(pos+110+values[11]+3)&~3;body=data[start:start+values[6]];assert len(body)==values[6]
  result.append((name,values,body));pos=(start+values[6]+3)&~3
  if name==b'TRAILER!!!\0':break
 assert result[-1][0]==b'TRAILER!!!\0';return result
def encode(rows):
 result=bytearray()
 for name,values,body in rows:
  values=list(values);values[6]=len(body)
  result.extend(b'070701'+b''.join(f'{v:08x}'.encode() for v in values));result.extend(name);result.extend(b'\0'*((-len(result))%4));result.extend(body);result.extend(b'\0'*((-len(result))%4))
 result.extend(b'\0'*((-len(result))%512));return bytes(result)

for name in ('cubit_kernel','display.svc','clock.svc','logstore.svc'):
 copy(a.platform_seed/name,seed/name)
original=a.platform_seed/'initrd.img';raw=original.read_bytes();inputs[str(original.resolve())]=sha(raw)
before=decode(raw);rows=list(before);changes=0
for i,(name,values,body) in enumerate(rows):
 if name.rstrip(b'\0').removeprefix(b'./')==b'system.ccl':
  assert b'desktop.metrics.trace' not in body, 'Use an original trace-disabled seed'
  assert body.count(b'(system-config v1')==1
  body=body.replace(b'(system-config v1',b'(system-config v1\n  (setting "desktop.metrics.trace" "true")',1)
  rows[i]=(name,values,body);changes+=1
assert changes==1
modified=encode(rows);after=decode(modified);assert len(before)==len(after)
for old,new in zip(before,after):
 if old[0].rstrip(b'\0').removeprefix(b'./')!=b'system.ccl':assert old==new
(seed/'initrd.img').write_bytes(modified)
for name in ('metrics.svc','desktop-metrics-observer.app'):copy(a.metrics_seed/name,metrics/name)
copy(a.trace_observer,metrics/'desktop-trace-observer.app')
copy(a.publication_client,metrics/'trace-publication.app')
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
(out/'result.json').write_text(json.dumps({'status':'PREPARED_NOT_RUN','scope':'Exactly one operator config setting changed; all other CPIO entries unchanged','original_initrd_sha256':sha(raw),'modified_initrd_sha256':sha(modified)},indent=2)+'\n')
