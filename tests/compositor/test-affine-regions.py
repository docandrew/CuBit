"""Hosted source-window pixels through the production Vulkan recorder."""
from pathlib import Path
import os, tempfile, subprocess, hashlib,json
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='affine-regions-',dir=root/'tests/compositor/build'));print(w,flush=True)
paths=['userspace/lib/compositor/'+n for n in ['vulkan_affine.c','vulkan_affine.h','vulkan_affine.frag','vulkan_backdrop.c']]
inputs={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in paths}
subprocess.run(['python3',str(root/'tests/compositor/build-vulkan-affine-shaders.py'),str(w/'generated')],check=True)
env={**os.environ,'C_INCLUDE_PATH':str(w/'generated')+':'+os.environ.get('C_INCLUDE_PATH',''),'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json','XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
subprocess.run(['cc','-std=c11','-O2','-Wall','-Wextra','-Werror','-I'+str(root/'userspace/lib/compositor'),'-I'+str(w/'generated'),str(root/'tests/compositor/affine_region_boundary_test.c'),str(root/'userspace/lib/compositor/vulkan_affine.c'),'-o',str(w/'region-boundary')],env=env,check=True)
subprocess.run([str(w/'region-boundary')],env=env,check=True)
for variant in ['full','window']:
 d=w/variant;d.mkdir()
 c=(root/'userspace/lib/compositor/vulkan_affine.c').read_text()
 host=(root/'tests/compositor/vulkan_affine_host.c').read_text()
 if variant=='window':
  a='return record_affine(borrowed,d,c,width,height,mask,argb,NULL);'
  assert c.count(a)==1
  c=c.replace(a,'const struct cubit_vulkan_source_region region={2,3,7,9,32,24};\n    return cubit_vulkan_record_affine_region(borrowed,d,c,width,height,mask,argb,&region);')
  a='*sx=(int)(u*W/ud);*sy=(int)(v*H/vd);return 1;';assert host.count(a)==1
  host=host.replace(a,'*sx=2+(int)(u*7/ud);*sy=3+(int)(v*9/vd);return 1;')
 (d/'vulkan_affine.c').write_text(c);(d/'vulkan_affine_host.c').write_text(host)
 (d/'regions.gpr').write_text(f'''project Regions extends "{root/'tests/compositor/vulkan_affine.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ("vulkan_affine.c","vulkan_affine_host.c");
 for Object_Dir use "obj"; for Exec_Dir use ".";
end Regions;
''')
 subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(d/'regions.gpr')],cwd=root/'kernel',env=env,check=True)
 r=subprocess.run([str(d/'vulkan_affine_tests')],env=env,text=True,capture_output=True,timeout=120)
 (d/'pixels.log').write_text(r.stdout+r.stderr);print(variant,r.stdout+r.stderr,flush=True);r.check_returncode();assert 'validation errors=0' in r.stdout
for p,h in inputs.items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==h,p
(w/'result.json').write_text(json.dumps({'status':'PASS','scope':'hosted full-image and fixed subregion Vulkan pixel oracle; no SPARK region policy or native execution','inputs':inputs},indent=2))
