#!/usr/bin/env python3
from pathlib import Path
import argparse,hashlib,json,subprocess,re
here=Path(__file__).resolve().parent;root=here.parents[1]
p=argparse.ArgumentParser();p.add_argument('--output',required=True,type=Path);a=p.parse_args();out=a.output
if not out.is_absolute():p.error('absolute output required')
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();catalog=json.loads((here/'sources.json').read_text())
pins={str(root/e['path']):e['sha256']for e in catalog['inputs']}
pins.update({str(here/e['path']):e['sha256']for e in catalog['tests']});pins[str(here/'sources.json')]=sha(here/'sources.json');pins[str(Path(__file__))]=sha(Path(__file__))
def verify():
 for p,h in pins.items():assert sha(Path(p))==h,p
verify();out.mkdir(exist_ok=False,parents=True);src=out/'source';src.mkdir()
for p,h in list(pins.items()):
 if Path(p).suffix in ['.adb','.ads']:
  dst=src/Path(p).name;assert not dst.exists(),dst;dst.write_bytes(Path(p).read_bytes());pins[str(dst)]=h
runs=[]
def save(done):
 (out/'RESULT.json').write_text(json.dumps({'completed':done,'runs':runs,'pins':pins,'scope':'Hosted descriptor/codec/Mouse tests; modeled data, no native/hardware. Both profiles runtimechecked/no-gnatp.'},indent=2)+'\n')
def run(label,argv,seconds):
 verify();r=subprocess.run(['timeout','--kill-after=2s',str(seconds)+'s',*argv],cwd=out,capture_output=True,text=True,timeout=seconds+3);log=out/(label+'.log');log.write_text(r.stdout+r.stderr);runs.append({'label':label,'argv':argv,'status':r.returncode,'log':str(log),'sha256':sha(log)});save(False);verify();assert r.returncode==0,log;return r.stdout
for profile in ['checked','optimized']:
 (out/('obj-'+profile)).mkdir();(out/('bin-'+profile)).mkdir();flags=['-O2','-gnat2022','-gnatwa','-gnatwe','-gnatwJ']+(['-gnata','-gnato']if profile=='checked'else[])
 g=out/(profile+'.gpr');g.write_text('project '+profile+' is for Source_Dirs use("source");for Object_Dir use"obj-'+profile+'";for Exec_Dir use"bin-'+profile+'";for Main use("feature_tests.adb","mouse_tests.adb");package Compiler is for Default_Switches("Ada")use('+','.join('"'+f+'"'for f in flags)+');end Compiler;end '+profile+';');pins[str(g)]=sha(g)
 run(profile+'-build',['gprbuild','-P',str(g),'-j2'],180)
 for name,expected,prefix in [('feature_tests',776,'feature checks'),('mouse_tests',2047,'mouse checks')]:
  exe=out/('bin-'+profile)/name;pins[str(exe)]=sha(exe);s=run(profile+'-'+name,[str(exe)],30);m=re.fullmatch(re.escape(prefix)+r'\s+(\d+)\s*',s);assert m and int(m[1])==expected;(runs[-1]).update(checks=expected);print(profile,name,expected,flush=True)
save(True)
