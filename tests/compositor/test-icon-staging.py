from pathlib import Path
import os, tempfile, subprocess, hashlib, json
root=Path.cwd()
assert os.environ.get("IN_NIX_SHELL")
work=Path(tempfile.mkdtemp(prefix="icon-staging-",dir=root/"tests/compositor/build"))
print(work,flush=True)
files=["userspace/services/desktop/"+n for n in ["desktop_icon_pixels-atlases.ads","desktop_icon_pixels-atlases.adb","desktop_icon_pixels.ads","desktop_icon_pixels.adb","desktop_icon_mapping.ads","desktop_icon_mapping.adb","desktop_icons.ads","desktop_window_icons.ads","desktop_cursors.ads"]]
files += ["userspace/lib/compositor/compositor_source_region.ads","userspace/lib/compositor/compositor_upload.ads","userspace/lib/compositor/compositor_upload.adb","tests/compositor/icon_pixels_tests.adb","tests/compositor/icon_mapping_tests.adb","tests/compositor/icon_atlas_tests.adb"]
hashes={}
for p in files:
 data=(root/p).read_bytes();hashes[p]=hashlib.sha256(data).hexdigest();(work/Path(p).name).write_bytes(data)
(work/"inputs.json").write_text(json.dumps(hashes,indent=2))
(work/"icons.gpr").write_text('project Icons is\n for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("icon_pixels_tests.adb", "icon_mapping_tests.adb", "icon_atlas_tests.adb"); package Compiler is for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-O2"); end Compiler; end Icons;\n')
for cmd in [["gprbuild","-q","-p","-P",str(work/"icons.gpr")],["gnatprove","-P",str(work/"icons.gpr"),"-u","desktop_icon_pixels.adb","desktop_icon_pixels-atlases.adb","--level=2","--timeout=30","-j2"]]:
 subprocess.run(["alr","exec","--",*cmd],cwd=root/"kernel",check=True)
for name in ["icon_pixels_tests","icon_mapping_tests","icon_atlas_tests"]:
 r=subprocess.run([str(work/name)],capture_output=True,text=True);print(r.stdout+r.stderr,flush=True);r.check_returncode()
report=(work/"obj/gnatprove/gnatprove.out").read_text();total=next(l for l in report.splitlines() if l.startswith("Total "));assert total.split()[-2:]==[".","."],total
for p,h in hashes.items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==h,p
(work/"result.json").write_text(json.dumps({"status":"PASS","proof":total,"scope":"hosted typed icon staging, no GPU upload or residency"},indent=2))
