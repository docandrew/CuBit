"""Build a private two-window fixture against a verified frozen Desktop runtime."""
import argparse, json, os, subprocess, shutil, importlib.util, hashlib
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--linked',type=Path,required=True)
p.add_argument('--runtime-archive',type=Path,required=True)
p.add_argument('--output',type=Path,required=True)
p.add_argument('--toolchain-root',type=Path,default=Path(__file__).resolve().parents[3])
a=p.parse_args();assert os.environ.get('IN_NIX_SHELL'), 'Run in Nix'
root=a.toolchain_root.resolve();linked=a.linked.resolve();out=a.output.resolve()
spec=importlib.util.spec_from_file_location('guard',root/'tools/verify_desktop_vulkan_compositor.py')
guard=importlib.util.module_from_spec(spec);spec.loader.exec_module(guard);guard.verify(linked)
cmds=json.loads((linked/'build-commands.json').read_text())
manifest=list(cmds[0]['argv']);native=list(cmds[1]['argv'][:3]);link=list(cmds[-1]['argv'][:4])
assert len(manifest)==7 and manifest[3]=='--schema' and manifest[5]=='--ada-output'
assert native[0]=='bash' and native[2]=='c' and link[:2]==native[:2] and link[2:]==['cpp','--manifest']
out.mkdir(parents=True,exist_ok=False);(out/'obj').mkdir();(out/'generated').mkdir()
for name in ('main.adb','manifest.ccl'):shutil.copyfile(Path(__file__).with_name(name),out/name)
project = 'project Test is for Source_Dirs use (".","generated"); for Object_Dir use "obj"; for Main use ("main.adb"); for Runtime ("Ada") use "'+str(linked/'userspace/runtime')+'"; package Compiler is for Default_Switches("Ada") use ("-O2","-gnat2022","-fno-pic","-mno-red-zone"); end Compiler; package Binder is for Default_Switches("Ada") use ("-d_C"); end Binder; end Test;'
(out/'test.gpr').write_text(project)
inputs=[Path(__file__),Path(manifest[0]),Path(manifest[1]),Path(manifest[4]),Path(native[1]),a.runtime_archive.resolve(),linked/'build-commands.json',out/'main.adb',out/'manifest.ccl']
digest=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
runtime = linked/'userspace/runtime'
assert digest(runtime/'adalib/libgnat-user.a') == digest(a.runtime_archive.resolve()), 'runtime archive mismatch'
inputs.extend(sorted(path for path in (runtime/'gnat').rglob('*') if path.is_file()))
hashes={str(path):digest(path) for path in inputs};commands=[]
def run(argv,**kw):
 commands.append(list(map(str,argv)));subprocess.run(commands[-1],check=True,**kw)
manifest[2]=str(out/'manifest.ccl');manifest[-1]=str(out/'generated/ccl_manifest_bindings.ads')
with (out/'manifest.S').open('w') as f:run(manifest,stdout=f)
run(native+['-c',out/'manifest.S','-o',out/'manifest.o'])
run(['alr','exec','--','gprbuild','-q','-p','-c','-b','-P',out/'test.gpr'],cwd=root/'kernel')
exchange=(out/'obj/main.bexch').read_text();objects=exchange.split('[BOUND OBJECT FILES]\n')[1].split('\n[')[0].splitlines()
run(link+[out/'manifest.o',out/'obj/b__main.o',*objects,a.runtime_archive.resolve(),'-o',out/'focus.app'],cwd=out)
assert all(digest(Path(path))==expected for path,expected in hashes.items());guard.verify(linked)
(out/'inputs.json').write_text(json.dumps(hashes,indent=2)+'\n')
(out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
(out/'result.json').write_text(json.dumps({'status':'LINKED','sha256':digest(out/'focus.app'),'scope':'test fixture only'},indent=2)+'\n')
print(out)
