from pathlib import Path
import subprocess,tempfile,hashlib,json,fcntl
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='desktop-logs-native-',dir=r/'tests/compositor/build'));print(w,flush=True)
with (r/'coordination/build.lock').open('a') as lock:
 fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 d=r/'userspace/services/desktop';k=r/'kernel'
 with (d/'build/manifest.S').open('w') as out:subprocess.run([str(r/'userspace/ccl/build/manifest/ccl-manifest'),str(r/'userspace/ccl/catalogs/native-runtime-services.ccl'),str(d/'manifest.ccl'),'--ada-output',str(d/'build/generated/ccl_manifest_bindings.ads')],stdout=out,check=True)
 subprocess.run(['alr','exec','--','gcc','-c',str(d/'build/manifest.S'),'-o',str(d/'build/manifest.o')],cwd=k,check=True)
 exe=w/'desktop-vulkan-link.svc'
 subprocess.run(['alr','exec','--','gprbuild','-p','-P',str(d/'desktop.gpr'),'-XCUBIT_COMPOSITOR=legacy','-XCUBIT_COMPOSITOR_METRICS=off','-o',str(exe)],cwd=k,check=True)
 (w/'result.json').write_text(json.dumps({'status':'LINKED','gpu_enabled':False,'backend':'legacy','binary_sha256':hashlib.sha256(exe.read_bytes()).hexdigest()},indent=2))
 s=(r/'tests/compositor/test-desktop-vulkan-boot.py').read_text().replace('root = Path(__file__).resolve().parents[2]',f"root = Path({str(r)!r})")
 s=s.replace('"clock.svc", "logstore.svc")','"clock.svc", "logstore.svc", "boot-logs.app")')
 s=s.replace("+ '))')", "+ ') (start \"boot-logs.app\" (priority 3)))')")
 s=s.replace('"display.svc", "desktop.svc")],','"display.svc", "desktop.svc", "boot-logs.app")],')
 a=s.index('    time.sleep(2)\n    baseline = screenshot');b=s.index('\nfinally:',a)
 s=s[:a]+'''    wait(lambda: 'boot-logs: desktop: retained software text active' in text(), 60)
    wait(lambda: 'boot-logs: desktop: internal shell active' in text(), 60)
    screenshot('desktop-logstore')
    healthy()
    result.update(status='PASS', desktop_records_via_logstore=True)
'''+s[b:]
 (w/'observer.py').write_text(s)
 subprocess.run(['python3',str(w/'observer.py'),str(w),str(r/'kernel/isodir/boot'),str(w/'observer-evidence')],check=True)
 print('PASS Desktop logstore observer',w,flush=True)
