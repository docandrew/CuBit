from pathlib import Path
import json,shutil,tempfile
from verify import verify,safe,D
checks=0
assert verify()=={'attempts':84,'inputs':510};checks+=1
with tempfile.TemporaryDirectory(prefix='metadata-guards-') as folder:
    root=Path(folder)/'reference';shutil.copytree(D,root,ignore=shutil.ignore_patterns('__pycache__'))
    assert verify(root)=={'attempts':84,'inputs':510};checks+=1
    mapping=json.loads((root/'provenance.json').read_bytes())['relocation'];target=root/next(iter(mapping.values()));raw=target.read_bytes()
    for operation in ['modify','remove']:
        if operation=='modify':target.write_bytes(raw+b'x')
        else:target.unlink()
        try:verify(root)
        except (ValueError,FileNotFoundError):checks+=1
        else:raise AssertionError('bad evidence accepted')
        target.write_bytes(raw)
    for name in ['../outside','/tmp/outside']:
        try:safe(root,name)
        except ValueError:checks+=1
        else:raise AssertionError('unsafe path accepted')
    p=root/'provenance.json';original=p.read_bytes();data=json.loads(original);keys=list(data['relocation']);data['relocation'][keys[1]]=data['relocation'][keys[0]];p.write_text(json.dumps(data))
    try:verify(root)
    except ValueError:checks+=1
    else:raise AssertionError('collision accepted')
    p.write_bytes(original)
print(json.dumps({'checks':checks,'status':'PASS','scope':'synthetic evidence guards'}))
