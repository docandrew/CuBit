"""SPARK atlas window validation and exact C ABI, hosted under Nix."""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='region-binding-',dir=root/'tests/compositor/build'));print(w,flush=True)
names=['compositor_source_region.ads','vulkan_region_ffi.ads','vulkan_region_ffi.adb','vulkan_affine_binding-regions.ads','vulkan_affine_binding-regions.adb','vulkan_submission-regions.ads','vulkan_submission-regions.adb']
paths=['userspace/lib/compositor/'+n for n in names]+['tests/compositor/region_binding_tests.adb','tests/compositor/region_binding_mock.c','tests/compositor/region_submission_tests.adb','tests/compositor/region_scene_tests.adb']
hashes={}
for n in paths:
 data=(root/n).read_bytes();(w/Path(n).name).write_bytes(data);hashes[n]=hashlib.sha256(data).hexdigest()
(w/'inputs.json').write_text(json.dumps(hashes,indent=2))
files=', '.join('"'+Path(n).name+'"' for n in paths)
(w/'region.gpr').write_text(f'''project Region extends "{root/'tests/compositor/vulkan_submission_mock.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({files});
 for Main use ("region_binding_tests.adb","region_submission_tests.adb","region_scene_tests.adb"); for Object_Dir use "obj"; for Exec_Dir use ".";
end Region;
''')
for c in [['gprbuild','-q','-p','-P',str(w/'region.gpr')],['gnatprove','-P',str(w/'region.gpr'),'-u','vulkan_affine_binding-regions.adb','vulkan_submission-regions.adb','vulkan_scene.adb','--level=2','--timeout=30','-j2']]:
 subprocess.run(['alr','exec','--',*c],cwd=root/'kernel',check=True)
subprocess.run([str(w/'region_binding_tests')],check=True)
subprocess.run([str(w/'region_submission_tests')],check=True)
subprocess.run([str(w/'region_scene_tests')],check=True)
text=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in text.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
for n,h in hashes.items():assert hashlib.sha256((root/n).read_bytes()).hexdigest()==h,n
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'scope':'SPARK region binding and hosted C mock, no native/GPU execution'},indent=2))
