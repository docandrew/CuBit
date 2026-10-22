import json
from pathlib import Path
from compare import compare, parse, encode
D=Path(__file__).resolve().parent
rows=[r for r in json.loads((D/'reference.json').read_text())['rows'] if r['selection']=='compare']
checks=0

def reject(raw, row):
    global checks
    checks+=1
    try:compare(parse(raw),row)
    except ValueError:return
    raise AssertionError('accepted invalid output: '+repr(raw))

for row in rows:
    good=encode(row);assert compare(parse(good),row);checks+=1
    for cut in range(len(good)):
        reject(good[:cut],row)
    for bad in [b'junk\n'+good,good+b'junk\n',good+b'\n',good.replace(b'\n',b'\r\n'),good+b'\x00',good.replace(b'NOTIFY_REPLAY 1',b'NOTIFY_REPLAY 2'),good.replace(b'STATUS '+row['status'].encode(),b'STATUS UNKNOWN'),good.replace(b'MARKER '+str(row['marker']).encode(),b'MARKER 18446744073709551616'),good.replace(b'EVENTS '+str(len(row['events'])).encode(),b'EVENTS 33'),good.replace(b'MARKER ',b'MARKER +'),good.replace(b'MARKER ',b'MARKER 00')]:reject(bad,row)
    lines=good.splitlines(keepends=True)
    for index in range(len(lines)):
        reject(b''.join(lines[:index]+lines[index+1:]),row)
        reject(b''.join(lines[:index]+[lines[index]]+lines[index:]),row)
    reject(good.replace(b'MARKER '+str(row['marker']).encode(),b'MARKER '+str(row['marker']+1).encode()),row)
    reject(good.replace(b'RESULT '+(str(row['result']).encode() if row['result'] is not None else b'NONE'),b'RESULT NONE' if row['result'] is not None else b'RESULT 0'),row)
    if row.get('local_event_policy'):
        injected=lines[:];injected[4]=b'EVENTS 1\n';injected.insert(-1,b'EVENT SYSTEM DEVICE \\DEV0 0 CURRENT\n');reject(b''.join(injected),row)
    if row['events']:
        for old,new in [(b' CURRENT',b' STALE'),(b' CURRENT',b' BAD'),(b'\\DEV0',b'\\MISS'),(b'EVENT DEVICE',b'EVENT OTHER'),(b'EVENT SYSTEM',b'EVENT OTHER')]:
            bad=good.replace(old,new)
            if bad!=good:reject(bad,row)
        event=lines[5].decode().split();event[4]='65536';reject(b''.join(lines[:5])+(' '.join(event)+'\n').encode()+b''.join(lines[6:]),row)
        # Structurally valid extra event and missing event both fail multiset.
        extra=lines[:];extra[4]=f'EVENTS {len(row["events"])+1}\n'.encode();extra.insert(-1,lines[5]);reject(b''.join(extra),row)
        missing=lines[:];missing[4]=f'EVENTS {len(row["events"])-1}\n'.encode();del missing[5];reject(b''.join(missing),row)
        swapped=lines[:5]+list(reversed(lines[5:-1]))+lines[-1:];assert compare(parse(b''.join(swapped)),row);checks+=1
print(json.dumps({'rows':len(rows),'checks':checks,'status':'PASS','scope':'synthetic protocol only'}))
