"""Nix-only native recovery test with a private Main overlay.

The overlay arms the injected font fault only inside a culled scene pass.
Production sources and staged service binaries are never overwritten.
"""
from pathlib import Path
import subprocess,tempfile,hashlib,json,os,fcntl
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='culled-raster-fault-',dir=r/'tests/compositor/build'));print(w,flush=True)
source=w/'raster-fault.c';source.write_text("""/* Private fixture: fail after one raster in a wallpaper-culled pass. */
extern unsigned __real_cubit_font_raster_mask(const void *, void *, void *);
static unsigned armed, calls, failed;
void cubit_test_begin_pass(void) { armed = 0; calls = 0; }
void cubit_test_arm_cull(void) { armed = 1; }
unsigned __wrap_cubit_font_raster_mask(const void *request, void *pixels, void *metrics) {
 if (failed) return 1;
 if (armed && calls++ > 0) { failed = 1; return 1; }
 return __real_cubit_font_raster_mask(request, pixels, metrics);
}
""")
main_source=r/'userspace/services/desktop/main.adb'
original=main_source.read_text()
private_main=original.replace('   wallpaperCullAnnounced : Boolean := False;',
 '   procedure Test_Begin_Pass with Import, Convention => C, External_Name => "cubit_test_begin_pass";\n'
 '   procedure Test_Arm_Cull with Import, Convention => C, External_Name => "cubit_test_arm_cull";\n'
 '   wallpaperCullAnnounced : Boolean := False;')
needle='                     if not isEmpty (Body_Area) and then physicalClip (Body_Area) = outputDamage then'
assert private_main.count(needle)==1
private_main=private_main.replace(needle,needle+'\n                        Test_Arm_Cull;')
needle='         textSceneRetry := False;'
assert private_main.count(needle)==1
private_main=private_main.replace(needle,needle+'\n         Test_Begin_Pass;')
(w/'src').mkdir();(w/'src/main.adb').write_text(private_main)
project=w/'fault.gpr'
project.write_text('project Fault extends "'+str(r/'userspace/services/desktop/desktop.gpr')+'" is for Source_Dirs use ("src"); for Object_Dir use "obj"; for Exec_Dir use "."; for Runtime ("Ada") use "'+str(r/'userspace/runtime')+'"; package Linker is for Default_Switches ("Ada") use ("-nostdlib", "-static", "-no-pie", "-Wl,-z,stack-size=16777216", "-lgnat-user", "-T", "'+str(r/'userspace/c/link.ld')+'", "'+str(r/'userspace/c/build/crt0.o')+'", "'+str(r/'userspace/services/desktop/build/manifest.o')+'", "'+str(r/'userspace/services/desktop/build/wallpaper.o')+'", "'+str(r/'userspace/services/desktop/build/wallpaper_cubie.o')+'"); end Linker; end Fault;')
exe=w/'desktop-vulkan-link.svc'
with (r/'coordination/build.lock').open('a') as lock:
 fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 subprocess.run(['alr','exec','--','gprbuild','-p','-c','-b','-P',str(project),'-XCUBIT_COMPOSITOR=legacy'],cwd=r/'kernel',check=True)
 subprocess.run(['alr','exec','--','gcc','-c','-O2','-ffreestanding','-fno-pic','-mno-red-zone',str(source),'-o',str(w/'raster-fault.o')],cwd=r/'kernel',check=True)
 subprocess.run(['alr','exec','--','gprbuild','-p','-l','-P',str(project),'-XCUBIT_COMPOSITOR=legacy','-o',str(exe),'-largs',str(w/'raster-fault.o'),'-Wl,--wrap=cubit_font_raster_mask'],cwd=r/'kernel',check=True)
assert main_source.read_text() == original, "Main changed during fixture build"
inputs={str(p.relative_to(r)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [r/'userspace/services/desktop/main.adb',r/'userspace/services/desktop/backend-legacy/desktop_compositor.adb',*[r/'userspace/lib/compositor'/('compositor_software_text'+suffix) for suffix in ['.ads','.adb','_target.ads','_target.adb']]]}
(w/'result.json').write_text(json.dumps({'status':'LINKED','gpu_enabled':False,'backend':'legacy','binary_sha256':hashlib.sha256(exe.read_bytes()).hexdigest(),'changed_sources':inputs,'fault':'persistent font raster failure after one raster in a culled pass','fault_source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'private_main_sha256':hashlib.sha256((w/'src/main.adb').read_bytes()).hexdigest()},indent=2))
print('LINKED',w,flush=True)
subprocess.run(['python3',str(r/'tests/compositor/test-desktop-vulkan-boot.py'),
 '--scaled-cursor-motion',str(w),str(r/'kernel/isodir/boot'),str(w/'native-evidence')],check=True)
text=(w/'native-evidence/serial.log').read_text(errors='replace')
assert text.count('desktop: internal shell active')==1
assert text.count('desktop: text batch failed; repainting scene in software')==1
assert text.count('desktop: CPU text fallback active')==1
assert 'restarting' not in text
assert text.index('desktop: fully covered wallpaper skipped') < text.index('desktop: text batch failed; repainting scene in software')
for path,digest in inputs.items():assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
result=json.loads((w/'native-evidence/result.json').read_text())
assert result['status']=='PASS'
result['failure_during_culled_pass']=True
result['fault_recovery']='PASS one scene replay, basic glyph fallback, single Desktop startup'
(w/'fault-result.json').write_text(json.dumps(result,indent=2))
print('PASS native raster failure recovery',w,flush=True)
