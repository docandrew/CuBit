"""Validate bounded Desktop resource partitions in private output directories under Nix."""
from pathlib import Path
import subprocess,tempfile,json,hashlib,os
r=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='source-partition-',dir=r/'tests/compositor/build')); print(w,flush=True)
inputs = [p for directory in ('userspace/lib/compositor', 'userspace/services/desktop', 'tests/compositor')
          for p in (r/directory).glob('*') if p.is_file() and p.suffix in ('.ads','.adb','.c','.h','.gpr')]
hashes = {str(p.relative_to(r)):hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
(w/'inputs.json').write_text(json.dumps(hashes,indent=2))
proofs=[]
for project,main in [('desktop_source_capacity','desktop_source_capacity_tests'),('desktop_strided','desktop_strided_tests'),('vulkan_submission_mock','vulkan_submission_mock_tests')]:
 d=w/project;d.mkdir();p=d/'check.gpr';p.write_text(f'''project Check extends "{r/'tests/compositor'/ (project+'.gpr')}" is
 for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("{main}.adb"); end Check;''')
 subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(p)],cwd=r/'kernel',check=True)
 subprocess.run([str(d/main)],check=True)
 if project=='desktop_source_capacity':
  subprocess.run(['alr','exec','--','gnatprove','-P',str(p),'-u','desktop_vulkan_startup.adb','vulkan_context_owner.adb','vulkan_image_owner.adb','--level=2','--timeout=30','-j2'],cwd=r/'kernel',check=True)
  report=(d/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
  proofs.append(total)
print('PASS expanded source partition, full backing budget, strided upload and submission regressions',flush=True)

for name,digest in hashes.items():
 assert hashlib.sha256((r/name).read_bytes()).hexdigest()==digest,name
(w/'result.json').write_text(json.dumps({'status':'PASS','proofs':proofs,'scope':'Hosted mock full-capacity and submission regression; not native execution','inputs':hashes},indent=2))
