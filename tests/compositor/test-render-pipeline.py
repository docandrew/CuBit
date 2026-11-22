from pathlib import Path
import runpy
check=runpy.run_path(str(Path(__file__).with_name('check-render-pipeline.py')))['check']
base='''COMPOSITOR-INPUT: surface=42 serial=8 kind=1 dequeued_us=1
COMPOSITOR-INPUT-STATS: count=1 invalid=0 dropped=0
COMPOSITOR-SOURCE: surface=42 epoch=1 ticket=4 input_after=8 accepted_us=2
COMPOSITOR-SOURCE-STATS: count=1 invalid=0 dropped=0
COMPOSITOR-FRAME: output=0 session=7 frame=90 submit_us=4 complete_us=5
COMPOSITOR-FRAME-STATS: count=1 invalid=0 dropped=0
'''
draw='COMPOSITOR-RENDER: kind=1 output=0 buffer=1 writer_epoch=7 writer_serial=3 surface=42 source_epoch=1 source_ticket=4 session=0 frame=0 observed_us=3\n'
submit='COMPOSITOR-RENDER: kind=2 output=0 buffer=1 writer_epoch=7 writer_serial=3 surface=0 source_epoch=0 source_ticket=0 session=7 frame=90 observed_us=4\n'
stats='COMPOSITOR-RENDER-STATS: count=2 invalid=0 dropped=0 unsupported=0\n'
text=base+draw+submit+stats
r=check(text);assert r['matched_input_draws']==1 and r['records'][0]['dequeue_to_completion_us']==4
# Buffer reuse must not associate an old draw with a newer writer serial.
for name,candidate in {
 'writer serial':base+draw+submit.replace('writer_serial=3','writer_serial=4')+stats,
 'output':base+draw+submit.replace('output=0','output=1')+stats,
 'slot':base+draw+submit.replace('buffer=1','buffer=2')+stats,
 'epoch':base+draw+submit.replace('writer_epoch=7','writer_epoch=8')+stats,
 'reverse order':base+submit+draw.replace('observed_us=3','observed_us=4')+stats,
 'duplicate submission':text.replace(stats,submit+stats.replace('count=2','count=3')),
 'clock mismatch':text.replace('submit_us=4','submit_us=3'),
 'source clock':text.replace('accepted_us=2','accepted_us=4'),
 'loss':text.replace('dropped=0 unsupported','dropped=1 unsupported'),
 'unsupported':text.replace('unsupported=0','unsupported=1'),
 'unclosed':text.replace(stats,''),
 'bad phase':text.replace('kind=2 output','kind=3 output'),
}.items():
    try:check(candidate)
    except ValueError:pass
    else:raise AssertionError(name)
# Equal timestamps still preserve ordering; repeated source draws are work,
# not evidence of multiple independent input responses.
r=check(base+draw+draw+submit+stats.replace('count=2','count=3'))
assert r['matched_draws']==2
# A complete unrelated association must not hide a missing join or cause a
# second writer to borrow its completion/source/input identity.
extra=draw.replace('writer_serial=3','writer_serial=4').replace('observed_us=3','observed_us=6')
r=check(base+draw+submit+extra+stats.replace('count=2','count=3'))
assert r['matched_draws']==1 and r['unsubmitted_draws']==1
extra_submit=submit.replace('writer_serial=3','writer_serial=4').replace('frame=90','frame=91').replace('observed_us=4','observed_us=7')
r=check(base+draw+submit+extra+extra_submit+stats.replace('count=2','count=4'))
assert r['matched_draws']==1 and r['uncompleted_draws']==1
extra_frame='COMPOSITOR-FRAME: output=0 session=7 frame=91 submit_us=7 complete_us=8\nCOMPOSITOR-FRAME-STATS: count=1 invalid=0 dropped=0\n'
unknown=extra.replace('source_ticket=4','source_ticket=5')
r=check(base+extra_frame+draw+submit+unknown+extra_submit+stats.replace('count=2','count=4'))
assert r['matched_draws']==1 and r['unobserved_source_draws']==1
unknown_source='COMPOSITOR-SOURCE: surface=42 epoch=1 ticket=5 input_after=0 accepted_us=2\nCOMPOSITOR-SOURCE-STATS: count=1 invalid=0 dropped=0\n'
r=check(base+unknown_source+extra_frame+draw+submit+unknown+extra_submit+stats.replace('count=2','count=4'))
assert r['matched_draws']==2 and r['matched_input_draws']==1 and r['draws_without_input_match']==1
# Startup/idle rendering need not carry an input event. Absence is reported,
# while malformed/lossy input batches still invalidate the capture.
idle='\n'.join(line for line in text.splitlines() if 'COMPOSITOR-INPUT' not in line)+'\n'
r=check(idle.replace('input_after=8','input_after=0'))
assert r['matched_draws']==1 and r['input_records']==0 and r['matched_input_draws']==0
assert r['unknown_watermarks']==1 and r['draws_without_input_match']==1
r=check(idle)
assert r['unmatched_watermarks']==1 and r['draws_without_input_match']==1
for candidate in (idle+'COMPOSITOR-INPUT-STATS: count=0 invalid=0 dropped=1\n',
                  idle+'COMPOSITOR-INPUT: surface=42 serial=8 kind=1 dequeued_us=1\n'):
    try: check(candidate)
    except ValueError: pass
    else: raise AssertionError('idle trace concealed input loss or partial batch')
print('RENDER-PIPELINE: PASS full identity join, repeated draw work and 12 invalid/cross-writer captures rejected')

# Valid repeated close delivery does not erase the source-to-output work join,
# but cannot be used as a unique input-to-completion latency association.
close_capture=text.replace('kind=1 dequeued_us=1','kind=10 dequeued_us=1')
close_capture += 'COMPOSITOR-INPUT: surface=42 serial=8 kind=10 dequeued_us=2\nCOMPOSITOR-INPUT-STATS: count=1 invalid=0 dropped=0\n'
r=check(close_capture)
assert r['matched_draws']==1 and r['matched_input_draws']==0
assert r['ambiguous_watermarks']==1 and r['retried_close_identities']==1
assert 'dequeue_to_completion_us' not in r['records'][0]
print('RENDER-PIPELINE: PASS ambiguous close retries retain work association without input latency')
