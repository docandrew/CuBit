"""Bind successful draw work to submitted writer tickets and observed releases.

This is a work trace, not a visibility or pixel-causality oracle. Retained pixels
without a new draw, occluded/overwritten draws, and physical scanout differ.
"""
import json
from pathlib import Path
import runpy
import sys
here=Path(__file__).parent
source=runpy.run_path(str(here/'check-source-trace.py'))
frame=runpy.run_path(str(here/'check-frame-trace.py'))
input_join=runpy.run_path(str(here/'check-input-publication.py'))
fields=source['fields']


def check(text):
    sources={(r['surface'],r['epoch'],r['ticket']):r for r in source['check'](text)['records']}
    frames={(r['output'],r['session'],r['frame']):r for r in frame['check'](text)['records']}
    input_report=input_join['check'](text,require_match=False)
    inputs={(r['surface'],r['epoch'],r['ticket']):r for r in input_report['records']}
    records=[];pending=0;batches=0;previous=None
    expected={'kind','output','buffer','writer_epoch','writer_serial','surface',
              'source_epoch','source_ticket','session','frame','observed_us'}
    for line in text.splitlines():
        if 'COMPOSITOR-RENDER:' in line:
            r=fields(line,'COMPOSITOR-RENDER:',expected)
            if r['kind'] not in (1,2) or r['output']>1 or not 1<=r['buffer']<=3:
                raise ValueError('invalid render phase/output/buffer')
            if not r['writer_epoch'] or not r['writer_serial'] or r['observed_us']==2**64-1:
                raise ValueError('invalid writer or unavailable draw clock')
            if previous is not None and r['observed_us']<previous:
                raise ValueError('reversed render trace clock')
            previous=r['observed_us']
            src=(r['surface'],r['source_epoch'],r['source_ticket'])
            if (r['kind']==1 and (not all(src) or r['session'] or r['frame'])) or (r['kind']==2 and (any(src) or not r['session'] or not r['frame'])):
                raise ValueError('invalid render phase fields')
            records.append(r);pending+=1
            if pending>64: raise ValueError('render trace storage bound exceeded')
        elif 'COMPOSITOR-RENDER-STATS:' in line:
            r=fields(line,'COMPOSITOR-RENDER-STATS:',{'count','invalid','dropped','unsupported'})
            if r['count']!=pending or r['invalid'] or r['dropped'] or r['unsupported']:
                raise ValueError('incomplete, lossy, invalid or unsupported render batch')
            pending=0;batches+=1
    if not records or pending: raise ValueError('missing or unterminated render batch')
    def writer(r): return r['output'],r['buffer'],r['writer_epoch'],r['writer_serial']
    submissions={};used_frames=set()
    for pos,r in enumerate(records):
        if r['kind']==2:
            key=writer(r);fid=(r['output'],r['session'],r['frame'])
            if key in submissions or fid in used_frames: raise ValueError('reused writer or submission identity')
            if r['writer_epoch']!=r['session']: raise ValueError('writer belongs to another output session')
            submissions[key]=(pos,r);used_frames.add(fid)
    joined=[];unsubmitted=0;uncompleted=0;unobserved_source=0;without_input=0
    for pos,r in enumerate(records):
        if r['kind']!=1: continue
        submission=submissions.get(writer(r))
        if submission is None: unsubmitted+=1;continue
        sub_pos,sub=submission
        if sub_pos<=pos or r['observed_us']>sub['observed_us']:
            raise ValueError('draw follows submission of its writer')
        fid=(sub['output'],sub['session'],sub['frame'])
        completion=frames.get(fid)
        if completion is None: uncompleted+=1;continue
        if completion['submit_us']!=sub['observed_us']:
            raise ValueError('submission clock disagrees with completion identity')
        src=(r['surface'],r['source_epoch'],r['source_ticket'])
        publication=sources.get(src)
        if publication is None: unobserved_source+=1;continue
        if publication['accepted_us']>r['observed_us']:
            raise ValueError('source draw precedes publication acceptance')
        entry=dict(r,accepted_us=publication['accepted_us'],session=sub['session'],
                   frame=sub['frame'],submitted_us=sub['observed_us'],
                   completed_us=completion['complete_us'])
        event=inputs.get(src)
        if event is None: without_input+=1
        else: entry.update(dequeued_us=event['dequeued_us'],input_after=event['input_after'],
                           dequeue_to_completion_us=completion['complete_us']-event['dequeued_us'])
        joined.append(entry)
    if not joined: raise ValueError('no accepted-source draw reaches an observed output completion')
    return {'scope':'Successful source draw work associated with submitted writer tickets and validated software completions; not surviving pixels, visibility, hardware scanout or causal keypress latency',
            'batches':batches,'render_records':len(records),'matched_draws':len(joined),
            'input_records':input_report['input_records'],
            'input_delivery_records':input_report['input_delivery_records'],
            'retried_close_identities':input_report['retried_close_identities'],
            'ambiguous_watermarks':input_report['ambiguous_watermarks'],
            'unknown_watermarks':input_report['unknown_watermarks'],
            'unmatched_watermarks':input_report['unmatched_watermarks'],
            'matched_input_draws':len(joined)-without_input,'unsubmitted_draws':unsubmitted,
            'uncompleted_draws':uncompleted,'unobserved_source_draws':unobserved_source,
            'draws_without_input_match':without_input,'records':joined}

if __name__=='__main__':
    try: print(json.dumps(check(Path(sys.argv[1]).read_text()),indent=2))
    except (ValueError,OSError) as e: raise SystemExit(f'render pipeline evidence rejected: {e}')
