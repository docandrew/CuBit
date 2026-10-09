#!/usr/bin/env python3
"""Hosted grouped keyboard policy proof and actual PS2 caller regressions.
Native HID, transport and consumer replacement evidence is separate.
"""
import argparse, hashlib, json, os, subprocess, tempfile
from pathlib import Path
here = Path(__file__).resolve().parent
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--source-root', type=Path, required=True)
p.add_argument('--toolchain-root', type=Path, required=True)
a = p.parse_args()
assert os.environ.get('IN_NIX_SHELL'), 'Run in the CuBit Nix shell'
r, t = a.source_root.resolve(), a.toolchain_root.resolve()
w = Path(tempfile.mkdtemp(prefix='cubit-keyboard-regression-', dir='/tmp'))
policy, driver = w/'policy', w/'driver'
policy.mkdir(); driver.mkdir()
inputs = {str(Path(__file__).resolve()): hashlib.sha256(Path(__file__).read_bytes()).hexdigest()}
def copy(src, dest):
    data = src.read_bytes(); digest = hashlib.sha256(data).hexdigest()
    assert str(src) not in inputs or inputs[str(src)] == digest, "Input changed while copying: " + str(src)
    inputs[str(src)] = digest
    dest.write_bytes(data)
for unit in ('input_pending','keyboard_pending'):
    for ext in ('.ads','.adb'):
        src = r/'userspace/lib/input'/(unit+ext)
        copy(src,policy/src.name);copy(src,driver/src.name)
copy(here/'keyboard_tests.adb',policy/'keyboard_tests.adb')
for name in ('main.adb','ps2_boot_probe.ads','ps2_boot_probe.adb'):
    copy(r/'userspace/services/ps2'/name, driver/name)
for name in ('cubit.ads','cubit-input.ads','cubit-input.adb'):
    copy(r/'userspace/runtime/gnat'/name,driver/name)
for src in sorted((here/'fixture').glob('*.ad?')):copy(src,driver/src.name)
for directory, main in ((policy,'keyboard_tests.adb'),(driver,'driver_tests.adb')):
    (directory/'test.gpr').write_text('''project Test is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("'''+main+'''");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Test;
''')
def run(argv):
    with (w/'run.log').open('a') as log:
        subprocess.run(argv,cwd=t/'kernel',check=True,stdout=log,stderr=subprocess.STDOUT)
for d in (policy,driver):run(['alr','exec','--','gprbuild','-q','-p','-P',str(d/'test.gpr')])
run([str(policy/'keyboard_tests')])
modes=['normal','overflow','replace','key-prefix','key-suffix','key-overflow','key-replace','key-partial-switch']
for mode in modes:run([str(driver/'driver_tests'),mode])
run(['alr','exec','--','gnatprove','-P',str(policy/'test.gpr'),'-u','keyboard_pending.adb','input_pending.adb','--level=2','--timeout=30','-j2'])
report=(policy/'obj/gnatprove/gnatprove.out').read_text()
summary=next(line for line in report.splitlines() if line.startswith('Total '))
assert summary.split()[-2:]==['.','.'],summary
for path, expected in inputs.items():assert hashlib.sha256(Path(path).read_bytes()).hexdigest()==expected,'Input changed: '+path
(w/'result.json').write_text(json.dumps({'status':'PASS','scope':'hosted policy proof and actual PS2 Main with mocked ports/IPC; not native HID','modes':modes,'proof_summary':summary,'source_sha256':inputs},indent=2)+'\n')
print(w)
