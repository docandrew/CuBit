"""Strict scalar metadata protocol; preserves coarse errors as a distinct claim."""
import re
U64_MAX=2**64-1
PHASES=('TEST','MARK','POST')
def parse(raw):
    if not isinstance(raw,bytes) or len(raw)>1024:raise ValueError('byte protocol bound')
    try:text=raw.decode('ascii')
    except UnicodeDecodeError:raise ValueError('non-ASCII') from None
    lines=text.split('\n')
    if len(lines)!=6 or lines[0]!='METADATA_REPLAY 1' or lines[4]!='END' or lines[5]!='':raise ValueError('protocol framing')
    result={}
    for phase,line in zip(PHASES,lines[1:4]):
        match=re.fullmatch(phase+r' (RETURNED|UNSUPPORTED_VALUE) (NONE|0|[1-9][0-9]*)',line)
        if not match:raise ValueError('phase syntax/order')
        status,value=match.groups()
        if status=='RETURNED':
            if value=='NONE':raise ValueError('integer required')
            number=int(value)
            if number>U64_MAX:raise ValueError('integer overflow')
        else:
            if phase!='TEST' or value!='NONE':raise ValueError('error/value inconsistency')
            number=None
        result[phase]={'status':status,'value':number}
    return result

def compare(observed,row):
    if row['selection']!='compare':raise ValueError('uncompiled case')
    if set(observed)!=set(PHASES):raise ValueError('phase set')
    for phase in PHASES:
        want=row['phases'][phase]
        if observed[phase]!={'status':want['status'],'value':want['value']}:raise ValueError('phase mismatch: '+phase)
    return True

def encode(row):
    lines=['METADATA_REPLAY 1']
    for phase in PHASES:
        value=row['phases'][phase]
        lines.append(phase+' '+value['status']+' '+('NONE' if value['value'] is None else str(value['value'])))
    return ('\n'.join(lines+['END'])+'\n').encode('ascii')
