#!/usr/bin/env python3
"""Check preserved ACPICA observations against exact embedded hosted fixtures."""
from pathlib import Path
import hashlib,json,re
here=Path(__file__).resolve().parent
ref=here/'reference'
for name,expected in json.loads((ref/'manifest.json').read_text()).items():
    path=ref/name
    if not path.resolve().is_relative_to(here.resolve()) or hashlib.sha256(path.read_bytes()).hexdigest()!=expected:
        raise RuntimeError('Reference identity mismatch: '+name)
text=(here/'delay_tests.adb').read_text()
calls=re.findall(r'Oracle_Test \(\[([^]]+)\], (5|6), (Returned|Empty_Buffer|Unsupported_Value), (85|102), Bits_(32|64)\);',text)
if len(calls)!=36:raise RuntimeError("Expected36 embedded oracle calls")
matched=[]
rejected=0
for row in json.loads((ref/'outcomes.json').read_text()):
    if row['compile']!=0:
        if not isinstance(row['compile'],int):raise RuntimeError('Non-compiler rejection')
        rejected+=1
        continue
    if row['execution']!=0:raise RuntimeError('Reference process failure')
    key=row['case']+str(row['revision']);data=(ref/(key+'.aml')).read_bytes()
    if data[:4]!=b'DSDT' or len(data)<36 or int.from_bytes(data[4:8],'little')!=len(data) or sum(data)%256 or data[8]!=row['revision']:
        raise RuntimeError('Invalid table: '+key)
    log=(ref/(key+'-execute.out')).read_text()+(ref/(key+'-execute.err')).read_text()
    for node,method,value in [(5,'TEST',85),(6,'TST1',102)]:
        body=log.split('Evaluating \\'+method+'\n',1)[1]
        body=body.split('Evaluating \\',1)[0]
        vals=re.findall(r'\[Integer\] = ([0-9A-F]+)',body)
        errors=re.findall(r'Evaluation of \\'+method+r' failed with status (AE_[A-Z_]+)',body)
        if errors:
            if vals or len(errors)!=1:raise RuntimeError('Ambiguous failure')
            status={'AE_AML_BUFFER_LIMIT':'Empty_Buffer','AE_AML_OPERAND_TYPE':'Unsupported_Value'}[errors[0]]
        else:
            if len(vals)!=1 or int(vals[0],16)!=value:raise RuntimeError('Wrong scalar')
            status='Returned'
        candidates=[]
        for index,(raw,n,s,v,w) in enumerate(calls):
            if re.sub(r'16#[0-9A-Fa-f]{2}#|[\s,]','',raw):continue
            binary=bytes(int(x,16) for x in re.findall(r'16#([0-9A-Fa-f]{2})#',raw))
            if binary==data[36:] and int(n)==node and s==status and int(v)==value and int(w)==(32 if row['revision']==1 else 64):candidates.append(index)
        if len(candidates)!=1 or candidates[0] in matched:raise RuntimeError('Embedded case mismatch: '+key+method)
        matched.extend(candidates)
if len(matched)!=36 or rejected!=10:raise RuntimeError('Expected36 observations')
print('Verified36 cached embedded observations;10 normal compiler rejections not executed; no fresh ACPICA run')
