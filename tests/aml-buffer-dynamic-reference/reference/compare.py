"""Strict large-buffer scalar protocol; raw upstream errors mapped explicitly."""
import re
U64_MAX=2**64-1
PHASES=('TEST','MARK','POST')
def parse(raw):
    if not isinstance(raw,bytes) or len(raw)>1024:raise ValueError('byte protocol bound')
    try:text=raw.decode('ascii')
    except UnicodeDecodeError:raise ValueError('non-ASCII') from None
    lines=text.split('\n')
    if len(lines)!=6 or lines[0]!='DYNAMIC_BUFFER_REPLAY 1' or lines[4]!='END' or lines[5]!='':raise ValueError('protocol framing')
    result={}
    for phase,line in zip(PHASES,lines[1:4]):
        match=re.fullmatch(phase+r' (RETURNED|DIVISION_BY_ZERO|UNSUPPORTED_VALUE) (NONE|0|[1-9][0-9]*)',line)
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
    validate_scope(row)
    if row['selection']!='compare':raise ValueError('uncompiled case')
    if set(observed)!=set(PHASES):raise ValueError('phase set')
    for phase in PHASES:
        want=row['phases'][phase]
        if observed[phase]!={'status':want['status'],'value':want['value']}:raise ValueError('phase mismatch: '+phase)
    return True

def encode(row):
    lines=['DYNAMIC_BUFFER_REPLAY 1']
    for phase in PHASES:
        value=row['phases'][phase]
        lines.append(phase+' '+value['status']+' '+('NONE' if value['value'] is None else str(value['value'])))
    return ('\n'.join(lines+['END'])+'\n').encode('ascii')


def validate_scope(row):
    phase=row['phases']['TEST'];key=row['key']
    if key in ('1-target-failure','2-target-failure'):
        if phase != {'status':'UNSUPPORTED_VALUE','value':None,'acpica_error':'AE_AML_BUFFER_LIMIT'} or row['comparison_scope']!='coarse_target_failure_status_projection':
            raise ValueError('target projection scope')
    elif key in ('1-count-failure','2-count-failure'):
        if phase != {'status':'DIVISION_BY_ZERO','value':None,'acpica_error':'AE_AML_DIVIDE_BY_ZERO'} or row['comparison_scope']!='division_by_zero_status_mapping':
            raise ValueError('count error scope')
    elif phase['status']!='RETURNED' or row['comparison_scope']!='exact_integer_flow' or 'acpica_error' in phase:
        raise ValueError('unexpected error projection')
