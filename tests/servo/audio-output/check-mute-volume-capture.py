import struct,json,sys
from pathlib import Path
p=Path(sys.argv[1]);b=(p/'output.wav').read_bytes()
assert b[:4]==b'RIFF' and b[8:16]==b'WAVEfmt ' and b[36:40]==b'data'
assert struct.unpack('<HHIIHH',b[20:36])==(1,2,48000,192000,4,16)
a=list(struct.iter_unpack('<hh',b[44:]));r=list(struct.iter_unpack('<hh',Path(sys.argv[2]).read_bytes()))
pos=0;covered=set();matches=[]
for scale in [0.5,1]:
 expected=[tuple(round(v*scale) for v in frame) for frame in r]
 while pos<=len(a)-len(expected):
  if all(abs(a[pos+k][c]-expected[k][c])<=2 for k in range(32) for c in range(2)):
   error=max(abs(a[pos+k][c]-expected[k][c]) for k in range(len(expected)) for c in range(2))
   if error<=2:break
  pos+=1
 else:raise AssertionError(('missing contiguous scaled clip',scale,matches))
 matches.append({'frame':pos,'scale':scale,'error':error});covered.update(range(pos,pos+len(expected)));pos+=len(expected)
extra=[(i,s) for i,s in enumerate(a) if i not in covered and s!=(0,0)]
assert not extra,(len(extra),extra[:8])
report={'result':'PASS','muted_cycles':1,'matches':matches,'scope':'Actual Penny HTML media mute and volume in CuBit/QEMU; exactly half/full-volume clips and zero other nonzero samples. Not physical hardware.'}
(p/'browser-mute-volume-capture.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report))
