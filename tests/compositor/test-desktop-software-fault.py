from pathlib import Path
import subprocess,tempfile,hashlib,json,os,fcntl
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='software-raster-fault-',dir=r/'tests/compositor/build'));print(w,flush=True)
source=w/'raster-fault.c';source.write_text('''/* Private native fault fixture: permit one retained glyph, then fail forever. */
extern unsigned __real_cubit_font_raster_mask(const void *, void *, void *);
static unsigned calls;
unsigned __wrap_cubit_font_raster_mask(const void *request, void *pixels, void *metrics) {
 if (calls) return 1;
 calls = 1;
 return __real_cubit_font_raster_mask(request, pixels, metrics);
}
''')
exe=w/'desktop-vulkan-link.svc'
with (r/'coordination/build.lock').open('a') as lock:
 fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 subprocess.run(['alr','exec','--','gprbuild','-p','-c','-b','-P',str(r/'userspace/services/desktop/desktop.gpr'),'-XCUBIT_COMPOSITOR=legacy'],cwd=r/'kernel',check=True)
 subprocess.run(['alr','exec','--','gcc','-c','-O2','-ffreestanding','-fno-pic','-mno-red-zone',str(source),'-o',str(w/'raster-fault.o')],cwd=r/'kernel',check=True)
 subprocess.run(['alr','exec','--','gprbuild','-p','-l','-P',str(r/'userspace/services/desktop/desktop.gpr'),'-XCUBIT_COMPOSITOR=legacy','-o',str(exe),'-largs',str(w/'raster-fault.o'),'-Wl,--wrap=cubit_font_raster_mask'],cwd=r/'kernel',check=True)
inputs={str(p.relative_to(r)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [r/'userspace/services/desktop/main.adb',r/'userspace/services/desktop/backend-legacy/desktop_compositor.adb',*[r/'userspace/lib/compositor'/('compositor_software_text'+suffix) for suffix in ['.ads','.adb','_target.ads','_target.adb']]]}
(w/'result.json').write_text(json.dumps({'status':'LINKED','gpu_enabled':False,'backend':'legacy','binary_sha256':hashlib.sha256(exe.read_bytes()).hexdigest(),'changed_sources':inputs,'fault':'persistent font raster failure after one call','fault_source_sha256':hashlib.sha256(source.read_bytes()).hexdigest()},indent=2))
print('LINKED',w,flush=True)
subprocess.run(['python3',str(r/'tests/compositor/test-desktop-vulkan-boot.py'),
 '--scaled-cursor-motion',str(w),str(r/'kernel/isodir/boot'),str(w/'native-evidence')],check=True)
text=(w/'native-evidence/serial.log').read_text(errors='replace')
assert text.count('desktop: internal shell active')==1
assert text.count('desktop: text batch failed; repainting scene in software')==1
assert text.count('desktop: CPU text fallback active')==1
assert 'restarting' not in text
for path,digest in inputs.items():assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
result=json.loads((w/'native-evidence/result.json').read_text())
assert result['status']=='PASS'
result['fault_recovery']='PASS one scene replay, basic glyph fallback, single Desktop startup'
(w/'fault-result.json').write_text(json.dumps(result,indent=2))
print('PASS native raster failure recovery',w,flush=True)
