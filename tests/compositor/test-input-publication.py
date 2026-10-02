from pathlib import Path
import runpy
check=runpy.run_path(str(Path(__file__).with_name('check-input-publication.py')))['check']
def event(surface, serial, tick):
    return f'COMPOSITOR-INPUT: surface={surface} serial={serial} kind=1 dequeued_us={tick}\n'
def pub(surface, ticket, serial, tick):
    return f'COMPOSITOR-SOURCE: surface={surface} epoch=1 ticket={ticket} input_after={serial} accepted_us={tick}\n'
i='COMPOSITOR-INPUT-STATS: count=2 invalid=0 dropped=0\n'
s='COMPOSITOR-SOURCE-STATS: count=4 invalid=0 dropped=0\n'
# Drain order need not be event order. Equal serials on separate surfaces
# cannot cross-match; repeated watermarks across distinct frames are legal.
text=pub(1,1,7,30)+pub(1,2,7,50)+pub(2,1,99,60)+pub(2,2,0,70)+s+event(1,7,10)+event(2,7,20)+i
report=check(text)
assert [r['dequeue_to_accept_us'] for r in report['records']]==[20,40]
assert report['unmatched_watermarks']==1 and report['unknown_watermarks']==1
cases=[text.replace('dropped=0','dropped=1',1), text.replace(i,''),
       text.replace('count=2','count=3'), text.replace(event(2,7,20),event(1,7,20)),
       text.replace('dequeued_us=20','dequeued_us=9'),
       text.replace('dequeued_us=10',f'dequeued_us={2**64-1}'),
       text.replace('accepted_us=30','accepted_us=5'),
       text.replace('kind=1','kind=11'),text.replace('serial=7 kind','serial=0 kind'),
       text.replace('epoch=1','epoch=0'),text.replace('surface=1 serial','surface=3 serial'),
       text.replace('serial=7 kind','serial=7 serial=7 kind')]
for candidate in cases:
    try: check(candidate)
    except ValueError: pass
    else: raise AssertionError('accepted malformed or uncorrelatable evidence')
print('INPUT-PUBLICATION: PASS exact per-surface joins, repeated/unknown/missing watermarks, 12 invalid captures rejected')

# A retained close identity can be delivered repeatedly, even across batches.
# Its watermark cannot identify one attempt, so exclude it from latency joins.
close=event(1,7,10).replace('kind=1','kind=10')
retry=event(1,7,20).replace('kind=1','kind=10')
close_text=pub(1,1,7,30)+'COMPOSITOR-SOURCE-STATS: count=1 invalid=0 dropped=0\n'+close+'COMPOSITOR-INPUT-STATS: count=1 invalid=0 dropped=0\n'+retry+'COMPOSITOR-INPUT-STATS: count=1 invalid=0 dropped=0\n'
r=check(close_text,require_match=False)
assert r['input_records']==1 and r['input_delivery_records']==2
assert r['retried_close_identities']==1 and r['ambiguous_watermarks']==1
assert r['matched_publications']==0 and not r['records']
# Losing the retry distinction, changing kinds, or reversing retry time must
# not turn ambiguous delivery into apparently valid causal evidence.
for bad in (close_text.replace(retry,event(1,7,20)),
            close_text.replace(close,event(1,7,10)),
            close_text.replace('dequeued_us=20','dequeued_us=9'),
            close_text.replace('accepted_us=30','accepted_us=5')):
    try: check(bad,require_match=False)
    except ValueError: pass
    else: raise AssertionError('invalid close retry capture accepted')
print('INPUT-PUBLICATION: PASS close redelivery reports ambiguity without inventing latency')
