"""Validate reversible metadata evidence relocation and expected comparisons."""
import hashlib,json
from pathlib import Path
D=Path(__file__).resolve().parent
sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
def safe(root,name):
    rel=Path(name);path=root/rel
    if rel.is_absolute() or '..' in rel.parts or any(p.is_symlink() for p in [path,*path.parents]):raise ValueError('unsafe reference path')
    if not path.resolve(strict=True).is_relative_to(root.resolve()):raise ValueError('reference escape')
    return path

def verify(root=D):
    provenance=json.loads((root/'provenance.json').read_bytes())
    for key,name in [('original_reference_sha256','original-reference.json'),('portable_reference_sha256','reference.json')]:
        if sha(root/name)!=provenance[key]:raise ValueError('reference digest changed')
    original=json.loads((root/'original-reference.json').read_bytes());portable=json.loads((root/'reference.json').read_bytes());mapping=provenance['relocation']
    if set(mapping)!=set(original['inputs']) or len(set(mapping.values()))!=len(mapping):raise ValueError('mapping collision/missing input')
    inverse={new:old for old,new in mapping.items()}
    for old,new in mapping.items():
        if sha(safe(root,new))!=original['inputs'][old]:raise ValueError('evidence changed')
    prior_path=root/'prior-reference.json'
    if sha(prior_path)!=provenance['prior_reference_sha256']:raise ValueError('prior reference drift')
    prior=json.loads(prior_path.read_bytes())
    if portable['rows'][:52]!=prior['rows']:raise ValueError('original52 rows changed')
    if any(portable['inputs'].get(n)!=h for n,h in prior['inputs'].items()):raise ValueError('original evidence changed')
    portable['inputs']={inverse[name]:h for name,h in portable['inputs'].items()}
    for row in portable['rows']:
        if row['selection']=='compare':
            old=inverse[row.pop('aml')];oracle,key=row['key'].split('/')
            if not old.endswith('/'+oracle+'/cases/'+key+'.aml'):raise ValueError('wrong AML selected')
    if portable!=original:raise ValueError('non-path reference change')
    return {'attempts':len(original['rows']),'inputs':len(mapping)}
if __name__=='__main__':print(json.dumps(verify()))
