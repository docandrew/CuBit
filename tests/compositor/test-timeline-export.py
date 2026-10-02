from pathlib import Path
import json
import runpy
import tempfile
h=Path(__file__).parent
api=runpy.run_path(str(h/'export-timeline.py'))
# Reuse the independently validated full-identity example corpus.
fixture=runpy.run_path(str(h/'test-render-pipeline.py'))
text=fixture['text']
result=api['convert'](text)
slices=[e for e in result['traceEvents'] if e['ph']=='X']
assert len(slices)==1 and slices[0]['dur']==1 and slices[0]['ts']==3
assert result['metadata']['origin_monotonic_us']=='1'
assert slices[0]['args']['frame']=='90'
assert result['metadata']['synthetic_lanes']
assert all(e.get('ph') not in ('s','f','B','E') for e in result['traceEvents'])
# Rebased clocks and string identities preserve values beyond JS integer precision.
large=text.replace('frame=90',f'frame={2**63+17}')
for field,value in [('dequeued_us',1),('accepted_us',2),('observed_us',3),('observed_us',4),('submit_us',4),('complete_us',5)]:
    large=large.replace(f'{field}={value}',f'{field}={2**63+value}')
big=api['convert'](large)
assert [e for e in big['traceEvents'] if e['ph']=='X'][0]['args']['frame']==str(2**63+17)
assert [e for e in big['traceEvents'] if e['ph']=='X'][0]['ts']==3
# Retry ambiguity survives in metadata; it produces no made-up duration slice.
retry=api['convert'](fixture['close_capture'])
assert retry['metadata']['validation']['ambiguous_watermarks']==1
assert len([e for e in retry['traceEvents'] if e['ph']=='X'])==1
with tempfile.TemporaryDirectory(prefix='cubit-timeline-test-') as tmp:
    d=Path(tmp);src=d/'capture.log';out=d/'trace.json'
    src.write_text(text);api['export'](src,out)
    assert json.loads(out.read_text())==result
    preserved=out.read_bytes()
    src.write_text(text.replace('invalid=0','invalid=1',1))
    try:api['export'](src,out)
    except ValueError:pass
    else:raise AssertionError('lossy capture accepted')
    assert out.read_bytes()==preserved
    try:api['export'](src,src)
    except ValueError:pass
    else:raise AssertionError('source overwrite accepted')
print('TIMELINE: PASS validated slices/instants, 64-bit identities, rebased clocks, retry ambiguity and atomic refusal')
