from pathlib import Path
import argparse, tempfile, subprocess, os, json, hashlib
assert os.environ.get('IN_NIX_SHELL')
root=Path(__file__).resolve().parents[2]
a=argparse.ArgumentParser();a.add_argument('--source-root',dest='source',type=Path,default=root);a.add_argument('--toolchain-root',dest='toolchain',type=Path,default=root);args=a.parse_args()
r=args.source.resolve(); fixture=Path(__file__).resolve().parent/'software-facade'
w=Path(tempfile.mkdtemp(prefix='cubit-software-facade-',dir=os.environ.get('TMPDIR', '/tmp'))); print(w,flush=True)
names='compositor_software_text compositor_software_text_target desktop_composition compositor_text compositor_glyph_cache compositor_glyph_storage compositor_glyph_software compositor_glyph_placement compositor_glyph_layout compositor_glyph_memory compositor_glyph_arena compositor_glyph_ffi compositor_identity compositor_affine compositor_transform compositor_formats heap_extents cubit-display_geometry cubit compositor_damage compositor_pool compositor_backend_selection cubit-appearance'.split()
inputs={}
def copy(p):
 data=p.read_bytes();(w/p.name).write_bytes(data);inputs[str(p)]=hashlib.sha256(data).hexdigest()
for name in names:
 for ext in ['ads','adb']:
  hits=[p for folder in ['userspace/lib/compositor','userspace/lib/display','userspace/lib/theme','userspace/runtime/gnat','userspace/allocator/src'] if (p:=r/folder/(name+'.'+ext)).exists()]
  if hits:copy(hits[0])
for ext in ['ads','adb']:copy(r/'userspace/services/desktop/backend-legacy'/('desktop_compositor.'+ext))
for name in ['software_facade_tests.adb','raster.c','test.gpr']:copy(fixture/name)
for cmd in [['gprbuild','-q','-p','-P',str(w/'test.gpr')],['gnatprove','-P',str(w/'test.gpr'),'-u','desktop_compositor.adb','--level=2','--timeout=30','-j2']]:
 subprocess.run(['alr','exec','--',*cmd],cwd=args.toolchain/'kernel',check=True)
for mode in [[],['first-fault']]:subprocess.run([str(w/'software_facade_tests'),*mode],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(line for line in report.splitlines() if line.startswith('Total '))
assert total.split()[-2:]==['.','.'],total
for name,digest in inputs.items():assert hashlib.sha256(Path(name).read_bytes()).hexdigest()==digest,name
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'inputs':inputs,'scope':'Hosted facade glyph fault/replay and startup/recovery contract checks'},indent=2)+'\n')
print(total,flush=True)
