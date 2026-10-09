"""Hosted production-startup control-flow test with explicit external mocks."""
from pathlib import Path
import argparse,hashlib,json,os,shutil,subprocess,tempfile
here=Path(__file__).resolve().parent
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source-root',type=Path,default=here.parents[2])
parser.add_argument('--toolchain-root',type=Path,required=True)
parser.add_argument('--output',type=Path)
a=parser.parse_args()
if not os.environ.get('IN_NIX_SHELL'):parser.error('Run in the repository Nix environment')
out=a.output.resolve() if a.output else Path(tempfile.mkdtemp(prefix='startup-denial-'))
if a.output:out.mkdir(parents=True,exist_ok=False)
inputs={}
def copy(src,name):
 data=src.read_bytes();inputs[str(src)]=hashlib.sha256(data).hexdigest();(out/name).write_bytes(data)
for p in (here/'mocks').iterdir():copy(p,p.name)
for name in ['startup_denial_tests.adb','test.gpr']:copy(here/name,name)
for directory,names in [
 ('userspace/services/desktop/startup-vulkan',['desktop_renderer_startup.adb']),
 ('userspace/services/desktop',['desktop_startup_layout.ads','desktop_startup_layout.adb','desktop_capability_io.ads']),
 ('userspace/lib/compositor',['compositor_backend_selection.ads','compositor_backend_selection.adb'])]:
 for name in names:copy(a.source_root/directory/name,name)
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
subprocess.run(['alr','exec','--','gprbuild','-p','-P',str(out/'test.gpr')],cwd=a.toolchain_root/'kernel',check=True)
subprocess.run([str(out/'startup_denial_tests')],check=True)
assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in inputs.items())
(out/'result.json').write_text(json.dumps({'status':'PASS','cases':13,'scope':'Hosted control flow; mocked foreign operations, not native allocation-denial evidence'},indent=2)+'\n')
print(out)
