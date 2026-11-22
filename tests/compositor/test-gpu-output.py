"""Whole-output completion and software fallback gate under Nix."""
from pathlib import Path
import os,subprocess,tempfile,hashlib,json
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='gpu-output-',dir=root/'tests/compositor/build'));print(w,flush=True)
paths=[root/'tests/compositor/desktop_gpu_output_tests.adb',root/'userspace/services/desktop/desktop_gpu_scene.ads',root/'userspace/services/desktop/desktop_gpu_scene.adb']
hashes={}
for p in paths:
 data=p.read_bytes();(w/p.name).write_bytes(data);hashes[str(p.relative_to(root))]=hashlib.sha256(data).hexdigest()
files=', '.join('"'+p.name+'"' for p in paths)
(w/'output.gpr').write_text(f'''project Output_Tests extends "{root/'tests/compositor/desktop_gpu_scene.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({files});
 for Main use ("desktop_gpu_output_tests.adb"); for Object_Dir use "obj"; for Exec_Dir use ".";
end Output_Tests;
''')
for command in [['gprbuild','-q','-p','-P',str(w/'output.gpr')],['gnatprove','-P',str(w/'output.gpr'),'-u','desktop_gpu_scene.adb','--level=2','--timeout=30','-j2']]:
 subprocess.run(['alr','exec','--',*command],cwd=root/'kernel',check=True)
for scenario in range(2):subprocess.run([str(w/'desktop_gpu_output_tests'),str(scenario)],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
for name,digest in hashes.items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'scope':'Hosted output gate and mocked renderer; not native execution','inputs':hashes},indent=2))
