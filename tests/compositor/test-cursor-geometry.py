from pathlib import Path
import subprocess,tempfile,os,hashlib,json
assert os.environ.get('IN_NIX_SHELL')
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='cursor-geometry-',dir=r/'tests/compositor/build'));print(w,flush=True)
inputs={}
for path in ['userspace/lib/compositor/compositor_cursor.ads','userspace/lib/compositor/compositor_cursor.adb','userspace/lib/display/cubit-display_geometry.ads','userspace/lib/display/cubit-display_geometry.adb','userspace/runtime/gnat/cubit.ads','userspace/services/desktop/desktop_cursors.ads','tests/compositor/cursor_geometry_tests.adb']:
 p=r/path;data=p.read_bytes();(w/p.name).write_bytes(data);inputs[path]=hashlib.sha256(data).hexdigest()
(w/'cursor.gpr').write_text('project Cursor is\n for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("cursor_geometry_tests.adb"); package Compiler is for Default_Switches ("Ada") use ("-gnat2022","-gnata","-O2"); end Compiler; end Cursor;')
for cmd in [['gprbuild','-q','-p','-P',str(w/'cursor.gpr')],['gnatprove','-P',str(w/'cursor.gpr'),'-u','compositor_cursor.adb','--level=2','--timeout=30','-j2']]:subprocess.run(['alr','exec','--',*cmd],cwd=r/'kernel',check=True)
subprocess.run([str(w/'cursor_geometry_tests')],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total;print(total)

for path,digest in inputs.items():assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
(w/"result.json").write_text(json.dumps({"status":"PASS","proof":total,"inputs":inputs},indent=2))
