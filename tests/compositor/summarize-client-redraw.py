"""Summarize matched native software redraw intervals; not a hardware benchmark."""
import hashlib,json,re,sys
from pathlib import Path

def summarize(directory):
 d=Path(directory);result=json.loads((d/'result.json').read_text())
 if not result.get('pass') or result.get('load_workers')!=4:
  raise ValueError('requires completed four-worker workload')
 t=(d/'serial.log').read_text();rows=[]
 if re.search(r'USER-MEMORY-FAULT|EXCEPTION:|TEST: FAIL|invalid global slot',t):
  raise ValueError('faulted workload')
 for line in t.splitlines():
  if 'desktop: stats ' not in line:continue
  v={k:int(n) for k,n in re.findall(r'\b([a-z_]+)=(\d+)',line)}
  if (v.get('frames')==1 and v.get('full')==0 and v.get('ev')==0
      and v.get('px')==425600 and v.get('scene_px',v.get('repair_px'))==426132):
   if any(v.get(k,1) for k in ['event_busy','input_resync','source_gap','source_reject']):
    raise ValueError('input loss in matched interval')
   rows.append(v['draw_ms'])
 if len(rows)<5:raise ValueError('too few matching client redraw intervals')
 return {'directory':str(d),'log_sha256':hashlib.sha256((d/'serial.log').read_bytes()).hexdigest(),
         'samples':len(rows),'draw_ms':rows,'mean_draw_ms':sum(rows)/len(rows),
         'client_damage_pixels':425600,'scene_render_pixels':426132}
if __name__=='__main__':
 a,b=map(summarize,sys.argv[1:3])
 print(json.dumps({'scope':'Exploratory QEMU TCG software wall-duration comparison on a shared host; not causal isolation, WCET, frame rate or photon latency',
                   'selection':'One-frame intervals, no input, no full redraw, identical client and scene-render pixel counts',
                   'baseline':a,'candidate':b,
                   'observed_mean_reduction_percent':100*(1-b['mean_draw_ms']/a['mean_draw_ms'])},indent=2))
