from array import array
from pathlib import Path
import sys,math,struct,json
p=Path(sys.argv[1]);b=(p/'output.wav').read_bytes()
assert b[:4]==b'RIFF' and b[36:40]==b'data'
assert struct.unpack_from('<HHI',b,20)==(1,2,48000) and struct.unpack_from('<H',b,34)[0]==16
pcm=array('h',b[44:])
if sys.byteorder!='little':pcm.byteswap()
n=4800;classes=[];mixed=[];counts={};runs=[]
coeff=[2*math.cos(2*math.pi*(300+100*i)/48000) for i in range(12)]
for offset in range(0,len(pcm)//2-n+1,n):
 samples=pcm[2*offset:2*(offset+n):2];energy=sum(x*x for x in samples)
 if energy/n<10000:continue
 powers=[]
 for c in coeff:
  prev=prev2=0.
  for value in samples:
   current=value+c*prev-prev2;prev2=prev;prev=current
  powers.append(prev*prev+prev2*prev2-c*prev*prev2)
 band=max(range(12),key=lambda i:powers[i]);confidence=2*powers[band]/(n*energy)
 if confidence<.65:
  mixed.append({'frame':offset,'confidence':confidence});continue
 counts[band]=counts.get(band,0)+1
 if not classes or classes[-1]!=band:
  classes.append(band);runs.append({'media_second_band':band,'capture_frame':offset})
allowed=[{0,1},{6,7},{2,3},{10,11}];phase=0;visited={0}
for band in classes:
 if band not in allowed[phase]:
  assert phase+1<len(allowed) and band in allowed[phase+1],('unexpected audio sequence',classes)
  phase+=1;visited.add(phase)
assert visited=={0,1,2,3},('missing seek phase',classes)
for band in (0,6,2,10,11):assert counts.get(band,0)>=3,('short/missing segment',band,counts)
assert len(mixed)<=12,('too many unclassified transition windows',mixed)
report={'result':'PASS','observed_media_second_bands':classes,'runs':runs,'windows_per_band':counts,'mixed_transition_windows':mixed,'scope':'100 ms spectral windows verify requested audio segments in order. Not sample-exact seek boundaries, gap detection, or A/V presentation timing.'}
(p/'seek-audio-report.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report))
