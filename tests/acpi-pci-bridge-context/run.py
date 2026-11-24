#!/usr/bin/env python3
from pathlib import Path
import argparse, hashlib, json, os, shutil, subprocess, tempfile

suite=Path(__file__).resolve().parent
root=suite.parents[1]
parser=argparse.ArgumentParser()
parser.add_argument('--mode',choices=['checked','release'],default='release')
parser.add_argument('--validate-only',action='store_true')
args=parser.parse_args()
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
catalog=json.loads((suite/'sources.json').read_text())
pins={str(suite/'sources.json'):sha(suite/'sources.json'),str(Path(__file__).resolve()):sha(Path(__file__).resolve())}
for row in catalog['inputs']:
    rel=Path(row['path'])
    assert not rel.is_absolute() and '..' not in rel.parts
    p=root/rel
    assert p.is_file() and not p.is_symlink() and sha(p)==row['sha256'],p
    pins[str(p)]=row['sha256']
iasl=Path(catalog['iasl']['path'])
assert sha(iasl)==catalog['iasl']['sha256']
pins[str(iasl)]=sha(iasl)
for tool in ['gprbuild']:
    p=Path(shutil.which(tool))
    pins[str(p)]=sha(p)
if args.validate_only:
    print(json.dumps(dict(validated=True,inputs=len(catalog['inputs']))));raise SystemExit
assert os.environ.get('TMPDIR')=='/home/doc/cubit-build-tmp','Export TMPDIR before Nix'
out=Path(tempfile.mkdtemp(prefix='acpi-pci-bridge-context-',dir=os.environ['TMPDIR']))
source=out/'source';source.mkdir()
for row in catalog['inputs']:
    p=root/row['path']
    if p.suffix not in ['.ads','.adb']:continue
    dest=source/p.name
    assert not dest.exists() or sha(dest)==sha(p),'source name collision'
    shutil.copy2(p,dest)
    pins[str(dest)]=sha(dest)
project=out/'test.gpr'
project.write_text('project Test is\nfor Source_Dirs use ("source");\nfor Object_Dir use "obj";\nfor Exec_Dir use "bin";\nfor Main use ("laptop_init_replay.adb","bridge_probe_tests.adb");\npackage Naming is for Body ("ACPI_Target_Heap") use "acpi_target_heap_host.adb";end Naming;\npackage Compiler is for Default_Switches ("Ada") use ("-O1","-gnat2022",'+('"-gnata","-gnato"'if args.mode=='checked'else'"-gnatp"')+');end Compiler;\nend Test;\n')
pins[str(project)]=sha(project)
runs=[]
def verify():
    for p,h in pins.items():assert sha(Path(p))==h,p
def save(done):
    logical=[r for r in runs if r.get('test')]
    (out/'RESULT.json').write_text(json.dumps(dict(completed=done,profile=args.mode,pins=pins,runs=runs,
        passed=sum(r['passed'] for r in logical),total=26,hardware=False,
        scope='Actual namespace/VM/provider integration with synthetic PCI topology and scripted register model; pure probe exhaustive regression. No original firmware included, no hardware execution or whole-service proof.'),indent=2)+'\n')
def run(name,argv,seconds,env=None,test=False,controls=None):
    verify()
    x=subprocess.run(['timeout','--kill-after=2s',str(seconds)+'s','prlimit','--as=25769803776','--stack=134217728',*map(str,argv)],
        cwd=out,capture_output=True,text=True,timeout=seconds+3,env={**os.environ,**(env or {})})
    log=out/(name+'.log');log.write_text(x.stdout+x.stderr)
    count=sum(line.startswith('CONTROL ') for line in x.stdout.splitlines())
    passed=x.returncode==0 and (controls is None or count==controls)
    runs.append(dict(name=name,argv=list(map(str,argv)),env=env,status=x.returncode,passed=passed,
        test=test,controls=count,log=str(log)))
    save(False);print(name,'PASS'if passed else'FAIL',flush=True)
    assert passed,log
run('build',['gprbuild','-P',project,'-j2'],240)
for p in (out/'bin').iterdir():pins[str(p)]=sha(p)
fixtures=out/'fixtures';fixtures.mkdir()
cases=json.loads((suite/'cases.json').read_text())
for c in cases:
    asl=fixtures/c['asl'];shutil.copy2(suite/'fixtures'/c['asl'],asl)
    pins[str(asl)]=sha(asl)
    run('compile-'+c['name'],[iasl,'-p',fixtures/c['name'],asl],15)
    pins[str(fixtures/c['aml'])]=sha(fixtures/c['aml'])
run('probe',[out/'bin/bridge_probe_tests'],30,test=True)
default={'ACPI_BENCH_PROFILE':'off','ACPI_MODEL_PTCI':'0','ACPI_MODEL_SECONDARY_BUS':'0'}
for c in cases:
    run(c['name'],[out/'bin/laptop_init_replay','0','0','pci','100',fixtures/c['aml']],30,
        {**default,**c['env'],'ACPI_BRIDGE_CASE':c['scenario']},True,2)
bridge=next(c for c in cases if c['name']=='bridge')
for mode in ['normal','cached','no-provider','deny-second','fault','bad-byte','unknown','resolve-pending','fuel-zero','request-one']:
    run('control-'+mode,[out/'bin/laptop_init_replay','0','0','pci','100',fixtures/bridge['aml']],30,
        {**default,**bridge['env'],'ACPI_BRIDGE_CASE':mode},True,1 if mode in ['unknown','resolve-pending']else 2)
verify();save(True);print(out/'RESULT.json')
