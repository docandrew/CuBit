"""Export validated compositor observations to Perfetto-compatible Chrome JSON.

Only software submission-to-release records are duration slices. Input,
publication and draw observations are instants; no CPU stack, photon timing,
or client-declared watermark causality is invented.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import runpy
import tempfile

here=Path(__file__).parent
pipeline=runpy.run_path(str(here/'check-render-pipeline.py'))
fields=pipeline['fields']
IDENTITIES={'surface','serial','epoch','ticket','input_after','session','frame',
            'writer_epoch','writer_serial','source_epoch','source_ticket'}
LIMIT=2**53-1

def convert(text):
    report=pipeline['check'](text)
    frames=pipeline['frame']['check'](text)['records']
    sources=pipeline['source']['check'](text)['records']
    inputs=[];renders=[]
    for line in text.splitlines():
        if 'COMPOSITOR-INPUT:' in line:
            inputs.append(fields(line,'COMPOSITOR-INPUT:',{'surface','serial','kind','dequeued_us'}))
        elif 'COMPOSITOR-RENDER:' in line:
            renders.append(fields(line,'COMPOSITOR-RENDER:',
                {'kind','output','buffer','writer_epoch','writer_serial','surface',
                 'source_epoch','source_ticket','session','frame','observed_us'}))
    ticks=[r[k] for rows,keys in ((frames,('submit_us','complete_us')),
           (sources,('accepted_us',)),(inputs,('dequeued_us',)),(renders,('observed_us',)))
           for r in rows for k in keys]
    origin=min(ticks)
    if max(ticks)-origin>LIMIT:
        raise ValueError('capture duration exceeds exact JSON timestamp range')
    events=[{'ph':'M','pid':1,'name':'process_name','args':{'name':'CuBit Desktop capture (synthetic lanes)'}}]
    lanes={}
    def lane(kind,identity):
        key=(kind,identity)
        if key not in lanes:
            lanes[key]=len(lanes)+1
            events.append({'ph':'M','pid':1,'tid':lanes[key],'name':'thread_name',
                           'args':{'name':f'{kind} {identity}'}})
        return lanes[key]
    def arguments(r):
        return {k:str(v) if k in IDENTITIES or k.endswith('_us') else v for k,v in r.items()}
    def instant(name,track,identity,tick,r):
        events.append({'ph':'I','s':'t','pid':1,'tid':lane(track,identity),
                       'ts':tick-origin,'name':name,'cat':'cubit.observed',
                       'args':arguments(r)})
    for r in frames:
        events.append({'ph':'X','pid':1,'tid':lane('Output software release',r['output']),
                       'ts':r['submit_us']-origin,'dur':r['complete_us']-r['submit_us'],
                       'name':'Submission to software release','cat':'cubit.software_completion',
                       'args':arguments(r)})
    for r in sources:
        instant('Publication accepted (input watermark is client-declared)',
                'Surface publications',r['surface'],r['accepted_us'],r)
    for r in inputs:
        instant('Input delivery observation','Surface input',r['surface'],r['dequeued_us'],r)
    for r in renders:
        instant('Successful draw checkpoint' if r['kind']==1 else 'Writer submitted',
                'Output draw checkpoints',r['output'],r['observed_us'],r)
    # Metadata first; stable ordering preserves capture order for equal stamps.
    events.sort(key=lambda e:(e['ph']!='M',e.get('ts',0)))
    return {'traceEvents':events,'displayTimeUnit':'ms','metadata':{
        'schema':'cubit.compositor.timeline.v1','origin_monotonic_us':str(origin),
        'source_sha256':hashlib.sha256(text.encode()).hexdigest(),
        'synthetic_lanes':True,'timestamp_unit':'microseconds',
        'scope':report['scope'],
        'not_measured':['CPU execution stacks','hardware input arrival','display latch','photons'],
        'validation':{k:v for k,v in report.items() if k not in ('records','scope')},
        'software_completion_slices':len(frames),
        'input_delivery_instants':len(inputs),'publication_instants':len(sources),
        'render_checkpoint_instants':len(renders)}}

def export(source,output):
    source,output=Path(source),Path(output)
    if source.resolve()==output.resolve():raise ValueError('output must not replace the source capture')
    result=convert(source.read_text())
    temporary=None
    try:
        with tempfile.NamedTemporaryFile(mode='w',dir=output.parent,prefix='.cubit-timeline-',delete=False) as f:
            temporary=Path(f.name)
            json.dump(result,f,indent=2,allow_nan=False);f.write('\n')
        os.replace(temporary,output)
    finally:
        if temporary is not None and temporary.exists():temporary.unlink()
    return result['metadata']

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('capture',type=Path);p.add_argument('output',type=Path)
    args=p.parse_args()
    try:print(json.dumps(export(args.capture,args.output),indent=2))
    except (ValueError,OSError) as e:raise SystemExit(f'timeline export rejected: {e}')
