"""Native component compile in a private snapshot; invoke through kernel/alr."""
from pathlib import Path
import argparse,hashlib,json,os,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('mesa_source',type=Path)
args=parser.parse_args()
mesa_include=(args.mesa_source.resolve()/'include').relative_to(root)
assert (root/mesa_include/'vulkan/vulkan.h').is_file()
out=Path(tempfile.mkdtemp(prefix='vulkan-context-native-',dir=root/'tests/compositor/build'))
inputs={}
def copy(p):
 data=p.read_bytes();target=out/p.relative_to(root);target.parent.mkdir(parents=True,exist_ok=True)
 target.write_bytes(data);inputs[str(p.relative_to(root))]=hashlib.sha256(data).hexdigest()
for rel in ('userspace/lib/compositor','userspace/lib/display'):
 for p in (root/rel).iterdir():
  if p.is_file() and p.suffix in ('.ads','.adb','.h','.c'):copy(p)
for rel in ('userspace/runtime/gnat','userspace/runtime/adalib',str(mesa_include)):
 for p in (root/rel).rglob('*'):
  if p.is_file():copy(p)
for rel in ('userspace/runtime/ada_source_path','userspace/runtime/ada_object_path','userspace/runtime/runtime.xml',
 'userspace/runtime/target_properties','userspace/mesa/service-device.h',
 'tests/compositor/vulkan_submission_native.gpr','tests/compositor/vulkan_context_native.gpr'):copy(root/rel)
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n');print(out,flush=True)
result={'status':'INCOMPLETE','scope':'native Ada/C component compile, no final link or execution'}
try:
 subprocess.run(['gprbuild','-q','-c','-p','-P','tests/compositor/vulkan_context_native.gpr'],cwd=out,check=True,
  env={**os.environ,'NIX_HARDENING_ENABLE':''})
 for name in ('vulkan_context','vulkan_submission_native'):
  subprocess.run(['bash',str(root/'tests/mesa-anv/native-compiler.sh'),'c','-std=c11','-O2','-Wall','-Wextra','-Werror',
   '-I'+str(out/mesa_include),'-c',
   str(out/'userspace/lib/compositor'/f'{name}.c'),'-o',str(out/f'{name}.o')],check=True)
 for rel,digest in inputs.items():assert hashlib.sha256((out/rel).read_bytes()).hexdigest()==digest,rel
 drift=[rel for rel,digest in inputs.items() if hashlib.sha256((root/rel).read_bytes()).hexdigest()!=digest]
 assert not any(Path(rel).name.startswith('vulkan_context') for rel in drift),drift
 result.update(status='COMPILED',root_drift=drift)
 print('PASS native context owner/FFI and C bridge compiled against frozen inputs; no link or boot',flush=True)
finally:(out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
