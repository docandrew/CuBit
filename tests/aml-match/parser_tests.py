"""Independent byte-format examples and corruption matrix; no AML corpus."""
from byte_protocol import parse, matches, MAX_BYTES, MAX_DEPTH
checks = 0

def check(value):
    global checks
    checks += 1
    if not value:
        raise AssertionError('Check ' + str(checks))

# Literal framing and expected trees do not reuse the parser's construction logic.
fixtures = [
 (b'STATUS RETURNED\nINTEGER 18446744073709551615\nMARK 9\n',
  {'status':'RETURNED','result':{'kind':'integer','value':18446744073709551615},'marker':9}),
 (b'STATUS OBJECT_RETURNED\nBUFFER 0 10 13 127 128 255\nMARK 0\n',
  {'status':'OBJECT_RETURNED','result':{'kind':'buffer','hex':'000a0d7f80ff'},'marker':0}),
 (b'STATUS OBJECT_RETURNED\nBUFFER\nMARK 0\n',
  {'status':'OBJECT_RETURNED','result':{'kind':'buffer','hex':''},'marker':0}),
 (b'STATUS OBJECT_RETURNED\nPACKAGE 2\nINTEGER 3\nPACKAGE 1\nBUFFER 65 0\nMARK 42\n',
  {'status':'OBJECT_RETURNED','result':{'kind':'package','items':[{'kind':'integer','value':3},{'kind':'package','items':[{'kind':'buffer','hex':'4100'}]}]},'marker':42}),
 (b'STATUS EMPTY_BUFFER\nMARK 10111\n',
  {'status':'EMPTY_BUFFER','result':None,'marker':10111}),
 (b'STATUS UNSUPPORTED_VALUE\nMARK 0\n',
  {'status':'UNSUPPORTED_VALUE','result':None,'marker':0})]
for raw, expected in fixtures:
    check(parse(raw) == expected)
    check(matches(expected,0,raw))
    check(not matches(expected,1,raw))
    for damaged in (raw.split(b'\n',1)[1], raw+ b'INTEGER 7\n', b'STATUS RETURNED\n'+raw,
                    raw.replace(b'MARK ',b'SEEN '),raw.replace(b'STATUS ',b'STATUS BAD'),
                    raw.replace(b'\n',b'\r\n'),raw+b'\n',raw+b'\xff',raw+b'\0'):
        check(not matches(expected,0,damaged))
base, expected = fixtures[3]
for old,new in [(b'PACKAGE 2',b'PACKAGE 1'),(b'PACKAGE 2',b'PACKAGE 3'),(b'PACKAGE 1',b'PACKAGE 2'),
                (b'BUFFER 65 0',b'BUFFER 65 256'),(b'BUFFER 65 0',b'BUFFER -1 0'),
                (b'BUFFER 65 0',b'STRING A'),(b'BUFFER 65 0',b'REFERENCE NAMED_CELL'),
                (b'INTEGER 3',b'INTEGER 18446744073709551616'),(b'INTEGER 3',b'INTEGER +3'),
                (b'MARK 42',b'MARK 43'),(b'MARK 42',b'MARK 18446744073709551616'),
                (b'PACKAGE 2',b'PACKAGE 8193')]:
    check(not matches(expected,0,base.replace(old,new)))
# Complete versus short/trailing tree, status/type coherence, parser quotas.
for raw in [b'STATUS RETURNED\nBUFFER 1\nMARK 0',b'STATUS OBJECT_RETURNED\nINTEGER 1\nMARK 0',
            b'STATUS EMPTY_BUFFER\nBUFFER\nMARK 0',b'STATUS OBJECT_RETURNED\nPACKAGE 1\nMARK 0',
            b'STATUS OBJECT_RETURNED\n'+b'PACKAGE 1\n'*(MAX_DEPTH+2)+b'BUFFER\nMARK 0',
            b'STATUS OBJECT_RETURNED\nBUFFER'+b' 0'*(MAX_BYTES+1)+b'\nMARK 0']:
    try:
        parse(raw)
    except (ValueError,UnicodeError):
        check(True)
    else:
        check(False)
print('BYTE PROTOCOL CHECKS',checks,'synthetic parser checks; no AML corpus or runner replay')
