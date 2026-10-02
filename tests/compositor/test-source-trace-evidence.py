from pathlib import Path
import runpy
check = runpy.run_path(str(Path(__file__).with_name('check-source-trace.py')))['check']

def record(ticket, watermark=10, clock=None):
    return (f'COMPOSITOR-SOURCE: surface=1 epoch=1 ticket={ticket} '
            f'input_after={watermark} accepted_us={ticket if clock is None else clock}\n')

def stats(count, invalid=0, dropped=0):
    return f'COMPOSITOR-SOURCE-STATS: count={count} invalid={invalid} dropped={dropped}\n'

a = record(1,0,0)
b = record(2,2**64-1)
good = a+b+stats(2)
r = check(good)
assert len(r['records'])==2 and r['unknown_input_records']==1
# No lifetime cap: more than64 records remain valid when every batch closes.
many = ''.join(record(i)+stats(1) for i in range(1,257))
assert len(check(many)['records'])==256
bad = [good.replace('ticket=2','ticket=1'),good.replace('surface=1','surface=0'),
       good.replace('epoch=1','epoch=0'),good.replace('ticket=2',f'ticket={2**31}'),
       good.replace('accepted_us=2','accepted_us=18446744073709551615'),
       good.replace('accepted_us=0','accepted_us=3'),good.replace('count=2','count=1'),
       good.replace('invalid=0','invalid=1'),good.replace('dropped=0','dropped=1'),
       good.replace('input_after=0',f'input_after={2**64}'),
       good.replace('input_after=0','input_after=0 input_after=1'),
       good.replace('input_after=0','input_after=-1'),
       good.replace('input_after=0','input_after=0 extra=1'),
       good.replace(' input_after=0',''),a+b,'',stats(0),
       ''.join(record(i) for i in range(1,66))+stats(65),
       good+record(3),good+stats(0,dropped=1)]
for value in bad:
    try:
        check(value)
    except ValueError:
        pass
    else:
        raise AssertionError('accepted incomplete or malformed trace')
print(f'SOURCE-EVIDENCE: PASS 256 reusable batches, full-width/unknown input and {len(bad)} rejection controls')
