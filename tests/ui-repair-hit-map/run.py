"""Nix-hosted retained hit-map and clipped-paint regression; native evidence is separate."""
from pathlib import Path
import argparse, hashlib, json, os, shutil, subprocess
here = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source-root', type=Path, default=here.parents[1])
parser.add_argument('--ui-source', type=Path)
parser.add_argument('--output', type=Path, required=True)
a = parser.parse_args()
assert os.environ.get('IN_NIX_SHELL'), 'Run in the repository Nix environment'
r = a.source_root.resolve(); out = a.output.resolve(); out.mkdir(parents=True, exist_ok=False)
inputs = {}
def copy(src, dst):
    data = src.read_bytes(); inputs[str(src)] = hashlib.sha256(data).hexdigest()
    dst.parent.mkdir(parents=True, exist_ok=True); dst.write_bytes(data)
for name, src in [('ui', a.ui_source or r/'userspace/lib/ui'),
                  ('theme', r/'userspace/lib/theme'),
                  ('host', r/'userspace/ccl/tools/ccl-ui-preview/host'),
                  ('compositor', r/'userspace/lib/compositor'),
                  ('display', r/'userspace/lib/display'),
                  ('allocator', r/'userspace/allocator/src')]:
    for f in src.iterdir():
        if f.is_file() and f.suffix in ('.ads', '.adb', '.c', '.h'): copy(f, out/name/f.name)
copy(here/'check.adb', out/'check.adb')
copy(r/'tests/ui-menus/menus_tests.adb', out/'menus_tests.adb')
copy(r/'tests/ui-polish/combo_tests.adb', out/'combo_tests.adb')
copy(r/'userspace/rust/build/font-host/libcubit_fonts.a', out/'font/libcubit_fonts.a')
(out/'fonts_host.gpr').write_text('library project Fonts_Host is for Externally_Built use "true"; for Languages use ("C"); for Source_Dirs use (); for Object_Dir use "font"; for Library_Dir use "font"; for Library_Name use "cubit_fonts"; for Library_Kind use "static"; end Fonts_Host;')
(out/'test.gpr').write_text('with "fonts_host.gpr"; project Test is for Source_Dirs use (".","ui","theme","host","compositor","display","allocator"); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb","menus_tests.adb","combo_tests.adb"); package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler; end Test;')
(out/'inputs.json').write_text(json.dumps(inputs, indent=2)+'\n')
subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(out/'test.gpr')], cwd=r/'kernel', check=True)
for name in ['check','menus_tests','combo_tests']: subprocess.run([str(out/name)], check=True)
assert all(hashlib.sha256(Path(f).read_bytes()).hexdigest()==h for f,h in inputs.items())
(out/'result.json').write_text(json.dumps({'status':'HOSTED_PASS','scales_percent':[100,125,200],'native_validated':False,'formal_proof':False,'inputs_verified':len(inputs)},indent=2)+'\n')
print(out)
