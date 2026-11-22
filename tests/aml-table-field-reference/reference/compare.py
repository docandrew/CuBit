"""Strict positive-only table-field type, length and endpoint protocol."""
import re
PHASES=('TEST','MARK','POST')
SCOPE='table_field_type_length_endpoints'
def parse(raw):
 if not isinstance(raw,bytes) or len(raw)>1024:raise ValueError('byte protocol bound')
 try:text=raw.decode('ascii')
 except UnicodeDecodeError:raise ValueError('ASCII') from None
 lines=text.split('\n')
 if len(lines)!=6 or lines[0]!='TABLE_FIELD_REPLAY 1' or lines[4]!='END' or lines[5]!='':raise ValueError('framing')
 result={}
 for phase,line in zip(PHASES,lines[1:4]):
  m=re.fullmatch(phase+r' RETURNED (0|[1-9][0-9]*)',line)
  if not m:raise ValueError('phase syntax/order/status')
  value=int(m[1])
  if value>2**64-1:raise ValueError('overflow')
  result[phase]={'status':'RETURNED','value':value}
 return result

def compare(observed,row):
 if row['selection']!='compare' or row['comparison_scope']!=SCOPE:raise ValueError('scope')
 if set(observed)!=set(PHASES) or set(row['phases'])!=set(PHASES):raise ValueError('phase set')
 for p in PHASES:
  expected=row['phases'][p]
  if set(expected)!= {'status','value'} or expected['status']!='RETURNED':raise ValueError('expected positive integer')
  if observed[p]!=expected:raise ValueError('mismatch '+p)
 return True

def encode(row):
 return ('\n'.join(['TABLE_FIELD_REPLAY 1']+[p+' '+row['phases'][p]['status']+' '+str(row['phases'][p]['value']) for p in PHASES]+['END'])+'\n').encode('ascii')
