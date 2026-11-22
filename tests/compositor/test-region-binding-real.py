"""Hosted SPARK source-window binding through the production Vulkan recorder."""
from pathlib import Path
import os, tempfile, subprocess, hashlib,json
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='region-binding-real-',dir=root/'tests/compositor/build'));print(w,flush=True)
paths=['userspace/lib/compositor/'+n for n in ['vulkan_affine.c','vulkan_affine.h','vulkan_affine.frag','vulkan_backdrop.c']]
inputs={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in paths}
subprocess.run(['python3',str(root/'tests/compositor/build-vulkan-affine-shaders.py'),str(w/'generated')],check=True)
env={**os.environ,'C_INCLUDE_PATH':str(w/'generated')+':'+os.environ.get('C_INCLUDE_PATH',''),'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json','XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
subprocess.run(['cc','-std=c11','-O2','-Wall','-Wextra','-Werror','-I'+str(root/'userspace/lib/compositor'),'-I'+str(w/'generated'),str(root/'tests/compositor/affine_region_boundary_test.c'),str(root/'userspace/lib/compositor/vulkan_affine.c'),'-o',str(w/'region-boundary')],env=env,check=True)
subprocess.run([str(w/'region-boundary')],env=env,check=True)
for variant in ['window']:
 d=w/variant;d.mkdir()
 c=(root/'userspace/lib/compositor/vulkan_affine.c').read_text()
 host=(root/'tests/compositor/vulkan_affine_host.c').read_text()
 if variant=='window':

  a='*sx=(int)(u*W/ud);*sy=(int)(v*H/vd);return 1;';assert host.count(a)==1
  host=host.replace(a,'if(p->mask){*sx=(int)(u*W/ud);*sy=(int)(v*H/vd);}else{*sx=2+(int)(u*7/ud);*sy=3+(int)(v*9/vd);}return 1;')
 (d/'vulkan_affine.c').write_text(c);(d/'vulkan_affine_host.c').write_text(host)
 bridge=(root/'tests/compositor/vulkan_affine_test_bridge.adb').read_text()
 bridge=bridge.replace('with Vulkan_Affine_Binding;', 'with Vulkan_Affine_Binding;\nwith Vulkan_Affine_Binding.Regions;')
 call=bridge[bridge.index('      B.Draw_Output'):bridge.index('      return B.Outcome')]
 region=call.replace('B.Draw_Output','B.Regions.Draw_Output').replace('V.Over /= 0, V.Mask /= 0, V.Tint, Result, Straight_Alpha => V.Over = 2','(2, 3, 7, 9, 32, 24), V.Over /= 0, V.Over = 2, Result')
 bridge=bridge.replace(call,'      if V.Mask = 0 then\n'+region+'      else\n'+call+'      end if;\n')
 (d/'vulkan_affine_test_bridge.adb').write_text(bridge)
 extra=['compositor_source_region.ads','vulkan_region_ffi.ads','vulkan_region_ffi.adb','vulkan_affine_binding-regions.ads','vulkan_affine_binding-regions.adb']
 for name in extra:
  data=(root/'userspace/lib/compositor'/name).read_bytes();(d/name).write_bytes(data)
  inputs['userspace/lib/compositor/'+name]=hashlib.sha256(data).hexdigest()
 source_names=', '.join('"'+name+'"' for name in ['vulkan_affine.c','vulkan_affine_host.c','vulkan_affine_test_bridge.adb']+extra)
 (d/'regions.gpr').write_text(f'''project Regions extends "{root/'tests/compositor/vulkan_affine.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({source_names});
 for Object_Dir use "obj"; for Exec_Dir use ".";
end Regions;
''')
 subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(d/'regions.gpr')],cwd=root/'kernel',env=env,check=True)
 r=subprocess.run([str(d/'vulkan_affine_tests')],env=env,text=True,capture_output=True,timeout=120)
 (d/'pixels.log').write_text(r.stdout+r.stderr);print(variant,r.stdout+r.stderr,flush=True);r.check_returncode();assert 'validation errors=0' in r.stdout
for p,h in inputs.items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==h,p
(w/'result.json').write_text(json.dumps({'status':'PASS','scope':'hosted production SPARK region binding and Vulkan pixel oracle; no scene integration or native execution','inputs':inputs},indent=2))
