"""Exercise actual copy boundaries with memory canaries and real memcpy counting."""
from pathlib import Path
import argparse,json,hashlib,os,shutil,subprocess,tempfile
here=Path(__file__).resolve().parent
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--source-root',type=Path,default=here.parents[2]);p.add_argument('--toolchain-root',type=Path,required=True);p.add_argument('--output',type=Path)
a=p.parse_args()
if not os.environ.get('IN_NIX_SHELL'):p.error('Use the repository Nix environment')
out=a.output.resolve() if a.output else Path(tempfile.mkdtemp(prefix='copy-boundaries-'))
if a.output:out.mkdir(parents=True,exist_ok=False)
inputs={}
def copy(src):
 data=src.read_bytes();inputs[str(src)]=hashlib.sha256(data).hexdigest();(out/src.name).write_bytes(data)
for name in ['main.adb','test.gpr','count_memcpy.c']:copy(here/name)
for unit in ['compositor_readback_copy','compositor_row_copy','compositor_upload','compositor_upload_copy']:
 for ext in ['ads','adb']:copy(a.source_root/'userspace/lib/compositor'/(unit+'.'+ext))
for ext in ['ads','adb']:copy(a.source_root/'userspace/lib/display'/('cubit-display_geometry.'+ext))
copy(a.source_root/'userspace/runtime/gnat/cubit.ads')
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
def run(c):subprocess.run(list(map(str,c)),cwd=a.toolchain_root/'kernel',check=True)
run(['alr','exec','--','gcc','-c','-O1',out/'count_memcpy.c','-o',out/'count_memcpy.o'])
run(['alr','exec','--','gprbuild','-p','-P',out/'test.gpr','-largs',out/'count_memcpy.o','-Wl,--wrap=memcpy'])
run([out/'obj/main'])
assert all(hashlib.sha256(Path(k).read_bytes()).hexdigest()==v for k,v in inputs.items())
(out/'result.json').write_text(json.dumps({'status':'PASS','scope':'Hosted actual memcpy boundaries; rectangle byte counts and canaries verified; not yet integrated into Desktop output, no native GPU or latency claim'},indent=2)+'\n')
print(out)
