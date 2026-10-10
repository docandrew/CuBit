import json,sys
from pathlib import Path
import compare,byte_protocol
ROOT=Path(__file__).resolve().parent
checks=0
def check(value):
 global checks
 checks+=1
 assert value,checks
for row in json.loads((ROOT/'cases.json').read_text()):
 exp=compare.expected(row)
 lines=['STATUS '+exp['status']]
 if exp['result']:
  result=exp['result']
  lines.append('INTEGER '+str(result['value']) if result['kind']=='integer' else 'BUFFER' + ''.join(' '+str(v) for v in bytes.fromhex(result['hex'])))
 lines.append('MARK '+str(exp['marker']));good=('\n'.join(lines)+'\n').encode()
 check(compare.classify(row,0,good));check(not compare.classify(row,1,good))
 for malformed in [b'noise\n'+good,good+b'noise\n',good+b'\n',good+b'MARK 0\n',good+b'STATUS RETURNED\n',good+b'INTEGER 0\n',good.replace(b'STATUS ',b'STATUSX '),good.replace(b'MARK ',b'MARKX '),good.replace(b'\n',b'\r\n'),good+b'\x00',good+b'\xff',good.replace(b'MARK ',b'MARK +'),good.replace(b'MARK ',b'MARK -'),good.replace(b'MARK ',b'MARK 999999999999999999999999'),good.replace(b'MARK ',b'MARK \xc2\xb2'),good.replace(b'MARK ',b'MARK 18446744073709551616'),good[:-4],b'x'*(byte_protocol.MAX_OUTPUT_BYTES+1)]:
  check(not compare.classify(row,0,malformed))
for bad in [b'STATUS OBJECT_RETURNED\nBUFFER -1\nMARK 0\n',b'STATUS OBJECT_RETURNED\nBUFFER 256\nMARK 0\n',b'STATUS OBJECT_RETURNED\nBUFFER +1\nMARK 0\n',b'STATUS RETURNED\nINTEGER 18446744073709551616\nMARK 0\n',b'STATUS OBJECT_RETURNED\nBUFFER '+b'0 '*(byte_protocol.MAX_BYTES+1)+b'\nMARK 0\n']:
 try:byte_protocol.parse(bad)
 except (ValueError,UnicodeError):check(True)
 else:check(False)
code,data=compare.capture([sys.executable,'-c','import os; os.write(1,b"x"*(2*1024*1024)); os.write(1,b"z")'])
check(len(data)<=byte_protocol.MAX_OUTPUT_BYTES);check(code!=0)
code,data=compare.capture([sys.executable,'-c','print("STATUS RETURNED\\nINTEGER 1\\nMARK 0")'])
check(code==0);check(byte_protocol.parse(data)['result']['value']==1)
(ROOT/'strict-selftest.json').write_text(json.dumps({'checks':checks,'passed':True,'scope':'synthetic parser negatives and bounded child-output capture; not CuBit replay'},indent=2)+'\n')
print(checks)
