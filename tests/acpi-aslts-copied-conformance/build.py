#!/usr/bin/env python3
"""Build the labelled copied owner profile from verified private source bytes."""
from pathlib import Path, PurePosixPath
import argparse,hashlib,json,shutil,subprocess

H=Path(__file__).resolve().parent
sha=lambda b:hashlib.sha256(b).hexdigest()
p=argparse.ArgumentParser();p.add_argument('--root',type=Path,default=H.parents[1]);p.add_argument('--output',type=Path,required=True);args=p.parse_args()
root=args.root.resolve();catalog_bytes=(H/'sources.json').read_bytes();catalog=json.loads(catalog_bytes)
data={}
for name,h in catalog['inputs'].items():
    rel=PurePosixPath(name)
    if rel.is_absolute() or '..'in rel.parts:raise SystemExit('Nonrelative catalog input: '+name)
    body=(root/name).read_bytes()
    if sha(body)!=h:raise SystemExit('Source drift: '+name)
    data[name]=body
policy=json.loads(data['tests/acpi-aslts-copied-conformance/owner-policy.json'])
if policy['Copied_Table_Capacity']!=2 or policy['Copied_Byte_Capacity']!=256:raise SystemExit('This named profile is exactly two tables/256bytes; regenerate and qualify a different policy explicitly')
if catalog['flags']!=['-O1','-gnat2022','-gnatwa','-gnatwe','-gnatp']:raise SystemExit('Original hosted compiler profile changed')
tools={}
for name in ['gprbuild','gcc','timeout']:
    tool=Path(shutil.which(name)or'/missing').resolve()
    if not str(tool).startswith('/nix/store/'):raise SystemExit('Run inside the pinned CuBit Nix development shell')
    tools[str(tool)]=sha(tool.read_bytes())
key=sha(json.dumps(dict(catalog_sha256=sha(catalog_bytes),builder_sha256=sha(Path(__file__).read_bytes()),tools=tools,flags=catalog['flags']),sort_keys=True).encode())
out=args.output.resolve()/key;receipt=out/'BUILD.json';binary=out/'bin/aslts_corpus_runner'
if receipt.exists():
    prior=json.loads(receipt.read_text())
    if prior['returncode']==0 and binary.exists() and sha(binary.read_bytes())==prior['binary_sha256']:
        print(binary);raise SystemExit(0)
    raise SystemExit('Prior failed or changed build retained at '+str(out)+'; no automatic retry')
if out.exists():raise SystemExit('Incomplete prior build retained at '+str(out)+'; inspect it or choose a fresh output directory')
source=out/'source';source.mkdir(parents=True);names=set()
for name in catalog['ada_sources']:
    filename=Path(name).name
    if filename in names:raise SystemExit('Duplicate Ada source basename: '+filename)
    names.add(filename);(source/filename).write_bytes(data[name])
project=out/'owner.gpr'
project.write_text('project Owner is\nfor Source_Dirs use ('+json.dumps(str(source))+');\nfor Object_Dir use "obj";for Exec_Dir use "bin";for Main use ("aslts_corpus_runner.adb");\npackage Compiler is for Default_Switches("Ada") use ('+','.join(json.dumps(f)for f in catalog['flags'])+');end Compiler;end Owner;\n')
argv=['timeout','--kill-after=2s','240s','gprbuild','-q','-p','-j2','-P',str(project)]
(out/'ACTIVE.json').write_text(json.dumps(dict(argv=argv,key=key),indent=2)+'\n')
try:
    with(out/'build.log').open('w')as log:proc=subprocess.run(argv,cwd=out,stdout=log,stderr=subprocess.STDOUT,timeout=245)
    code=proc.returncode
except subprocess.TimeoutExpired:code=124
warnings='warning:'in(out/'build.log').read_text(errors='replace').lower()
raw_code=code
if code==0 and warnings:code=1
result=dict(key=key,argv=argv,returncode=code,compiler_returncode=raw_code,warnings=warnings,catalog_sha256=sha(catalog_bytes),builder_sha256=sha(Path(__file__).read_bytes()),tools=tools,input_hashes=catalog['inputs'],source_hashes={str(p):sha(p.read_bytes())for p in source.iterdir()},log_sha256=sha((out/'build.log').read_bytes()),native=False,proofs_run=False,owner_policy=policy)
if code==0:result['binary_sha256']=sha(binary.read_bytes())
receipt.write_text(json.dumps(result,indent=2)+'\n')
if code:raise SystemExit('Build failed; exact first result retained at '+str(receipt))
print(binary)
