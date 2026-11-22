from pathlib import Path
import struct,json,sys
p=Path(sys.argv[1]);count=int(sys.argv[2]);b=(p/'output.wav').read_bytes()
assert b[:4]==b'RIFF' and b[8:16]==b'WAVEfmt ' and b[36:40]==b'data'
assert struct.unpack('<HHIIHH',b[20:36])==(1,2,48000,192000,4,16)
a=list(struct.iter_unpack('<hh',b[44:]));runs=[]
for i,(l,r) in enumerate(a):
 if abs(l)>1 or abs(r)>1:
  assert abs(l-4096)<=1 and abs(r+4096)<=1,(i,l,r)
  if not runs or i!=runs[-1][-1]+1:runs.append([])
  runs[-1].append(i)
assert len(runs)==count,[len(r) for r in runs]
assert all(len(r)==4096 for r in runs),[len(r) for r in runs]
r={'result':'PASS','contexts':count,'frames_per_context':4096,'starts':[r[0] for r in runs],'max_signal_error':max(max(abs(a[i][0]-4096),abs(a[i][1]+4096)) for run in runs for i in run),'background_sample_bound':1,'scope':'Actual JavaScript AudioContext in Penny/CuBit QEMU; F32 to S16 conversion allows one-unit dither. Not A/V sync or physical hardware.'}
(p/'capture-report.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r))
