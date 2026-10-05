"""Packed icon atlas ownership, bounded upload and retirement under Nix."""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='icon-atlas-owner-',dir=root/'tests/compositor/build'));print(w,flush=True)
paths=list((root/'userspace/services/desktop').glob('desktop_icon*.ad?'))
paths+=[root/'userspace/services/desktop/desktop_cursors.ads',root/'userspace/services/desktop/desktop_window_icons.ads',root/'tests/compositor/icon_atlas_owner_tests.adb']
hashes={}
for p in paths:
 data=p.read_bytes();(w/p.name).write_bytes(data);hashes[str(p.relative_to(root))]=hashlib.sha256(data).hexdigest()
files=', '.join('"'+p.name+'"' for p in paths)
(w/'owner.gpr').write_text(f'''project Owner extends "{root/'tests/compositor/backdrop_owner_tests.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({files});
 for Main use ("icon_atlas_owner_tests.adb"); for Object_Dir use "obj"; for Exec_Dir use ".";
end Owner;
''')
for c in [['gprbuild','-q','-p','-P',str(w/'owner.gpr')],['gnatprove','-P',str(w/'owner.gpr'),'-u','desktop_icon_atlas_owner.adb','desktop_icon_upload.adb','--level=2','--timeout=30','-j2']]:
 subprocess.run(['alr','exec','--',*c],cwd=root/'kernel',check=True)
for case in range(3):subprocess.run([str(w/'icon_atlas_owner_tests'),str(case)],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
for p,h in hashes.items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==h,p
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'scope':'SPARK atlas owner/upload and mock provider lifecycle; no GPU/native execution','inputs':hashes},indent=2))
