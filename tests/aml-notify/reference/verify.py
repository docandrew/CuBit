"""Validate exact evidence bytes and path-only relocation of the accepted corpus."""
import hashlib
import json
from pathlib import Path
D=Path(__file__).resolve().parent

def digest(path):return hashlib.sha256(path.read_bytes()).hexdigest()
def safe(name):
    relative=Path(name)
    if relative.is_absolute() or '..' in relative.parts:
        raise ValueError('unsafe reference path')
    path=D/relative
    if any(part.is_symlink() for part in [path,*path.parents]):
        raise ValueError('reference symlink rejected')
    if not path.resolve(strict=True).is_relative_to(D):raise ValueError('reference escape')
    return path

def verify():
    provenance=json.loads((D/'provenance.json').read_bytes())
    original_path=D/'original-reference.json';portable_path=D/'reference.json'
    if digest(original_path)!=provenance['original_reference_sha256']:raise ValueError('original reference drift')
    if digest(portable_path)!=provenance['portable_reference_sha256']:raise ValueError('portable reference drift')
    original=json.loads(original_path.read_bytes());portable=json.loads(portable_path.read_bytes())
    mapping=provenance['relocation']
    if set(mapping)!=set(original['inputs']) or len(set(mapping.values()))!=len(mapping):raise ValueError('inexact relocation mapping')
    if len(mapping)!=422:raise ValueError('expected 422 exact evidence inputs')
    reverse={value:key for key,value in mapping.items()}
    for old,new in mapping.items():
        if digest(safe(new))!=original['inputs'][old]:raise ValueError('evidence drift: '+new)
    restored=json.loads(json.dumps(portable))
    restored['inputs']={reverse[name]:h for name,h in restored['inputs'].items()}
    for row in restored['rows']:
        if 'aml' in row:row['aml']=reverse[row['aml']]
    if restored!=original:raise ValueError('non-path corpus change')
    counts={selection:sum(row['selection']==selection for row in portable['rows']) for selection in ['compare','compiler_rejection','marker_inconclusive']}
    if counts!=dict(compare=66,compiler_rejection=20,marker_inconclusive=2):raise ValueError('selection changed')
    return counts

if __name__=='__main__':print(verify())
