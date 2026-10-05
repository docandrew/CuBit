from pathlib import Path
import shutil,tempfile,subprocess
root=Path(__file__).resolve().parents[2]; dst=Path(tempfile.mkdtemp(prefix='penny-ring-negative-'))
for name in ['cubit.ads','cubit-audio_ring.ads']:
 shutil.copyfile(root/'userspace/runtime/gnat'/name,dst/name)
p=dst/'cubit-audio_ring.ads'; p.write_text(p.read_text().replace('2**13','8128'))
s=(root/'tests/audio-ring/main.adb').read_text().replace('   pragma Assert (Capacity /= 0 and then (Capacity and (Capacity - 1)) = 0);\n','')
(dst/'main.adb').write_text(s)
(dst/'ring.gpr').write_text('project Ring is\n for Source_Dirs use (".");\n for Object_Dir use "build";\n for Exec_Dir use "build";\n for Main use ("main.adb");\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");\n end Compiler;\nend Ring;\n')
subprocess.run(['gprbuild','-p','-P',str(dst/'ring.gpr')],check=True)
r=subprocess.run([str(dst/'build/main')],capture_output=True,text=True)
assert r.returncode!=0 and 'ASSERTION_ERROR' in r.stderr,(r.returncode,r.stdout,r.stderr)
print('Old 8128-byte geometry rejected by sample comparison:',r.stderr.strip(), 'artifacts:',dst)
