import compare
from byte_protocol import matches
cases=compare.verify(); checks=0
for c in cases:
    if 'expected' not in c: continue
    e=c['expected']; lines=['STATUS '+e['status']]
    r=e['result']
    if r:
        if r['kind']=='integer': lines.append('INTEGER '+str(r['value']))
        else: lines.append('BUFFER'+''.join(' '+str(b) for b in bytes.fromhex(r['hex'])))
    lines.append('MARK '+str(e['marker'])); output=('\n'.join(lines)+'\n').encode('ascii')
    assert matches(e,0,output); checks+=1
    for bad in [output+b'EXTRA\n', output.replace(b'STATUS ',b'STATUS BAD',1), output.rsplit(b'MARK ',1)[0],output.replace(('MARK '+str(e['marker'])).encode(),('MARK '+str(e['marker']+1)).encode()),output.replace(b'\n',b'\r\n'),b'STATUS RETURNED\nINTEGER 18446744073709551616\nMARK 0\n']:
        assert not matches(e,0,bad);checks+=1
    assert not matches(e,1,output);checks+=1
print(f'{checks} full-corpus synthetic checks; 70 positive trees, no interpreter execution')
