#!/usr/bin/env python3
"""Build only hosted runner in a disjoint output directory; no proof reruns."""
from pathlib import Path
import argparse,hashlib,json,shutil,subprocess
H=Path(__file__).resolve().parent
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
a=argparse.ArgumentParser();a.add_argument('--output',type=Path,required=True);a.add_argument('--root',type=Path,default=H.parents[1]);v=a.parse_args();v.output=v.output.resolve();v.output.mkdir(parents=True,exist_ok=True);catalog=json.loads((H/'sources.json').read_text())
for p,h in catalog['inputs'].items():
 if sha(v.root/p)!=h:raise SystemExit('source drift: '+p)
compiler=Path(shutil.which('gprbuild') or '/missing').resolve()
if not str(compiler).startswith('/nix/store/'):raise SystemExit('Run in pinned Nix development shell')
pins=dict(catalog['inputs']);pins[str(H/'build.py')]=sha(H/'build.py');pins[str(H/'aslts_corpus_runner.adb')]=sha(H/'aslts_corpus_runner.adb');pins[str(compiler)]=sha(compiler)
key=hashlib.sha256(json.dumps(pins,sort_keys=True).encode()).hexdigest();receipt=v.output/'BUILD.json';binary=v.output/'bin/aslts_corpus_runner'
if receipt.exists() and binary.exists():
 prior=json.loads(receipt.read_text())
 if prior.get('key')==key and prior.get('binary_sha256')==sha(binary):print(binary);raise SystemExit(0)
dirs=[H]+[v.root/p for p in catalog['source_dirs']];project=v.output/'aslts_corpus.gpr';project.write_text('project ASLTS_Corpus is\nfor Source_Dirs use ('+','.join('"'+str(p)+'"' for p in dirs)+');\nfor Object_Dir use "obj"; for Exec_Dir use "bin"; for Main use ("aslts_corpus_runner.adb");\npackage Compiler is for Default_Switches ("Ada") use ("-O1","-gnat2022","-gnatwa","-gnatwe","-gnatp"); end Compiler;\nend ASLTS_Corpus;\n')
argv=[str(compiler),'-p','-P',str(project),'-j2']
with (v.output/'build.log').open('w') as log:r=subprocess.run(argv,stdout=log,stderr=subprocess.STDOUT,timeout=600)
result=dict(key=key,argv=argv,returncode=r.returncode,inputs=pins,native=False,proofs_run=False)
if r.returncode==0:result['binary_sha256']=sha(binary)
receipt.write_text(json.dumps(result,indent=2)+'\n')
if r.returncode:raise SystemExit('build failed; see '+str(v.output/'build.log'))
print(binary)
