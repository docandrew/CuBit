from pathlib import Path
import tempfile,subprocess,hashlib,json
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='row-copy-',dir=r/'tests/compositor/build'));print(w,flush=True);inputs={}
for folder,names in [('userspace/lib/compositor',['compositor_row_copy','compositor_sampling']),('userspace/lib/display',['cubit-display_geometry']),('userspace/runtime/gnat',['cubit'])]:
 for name in names:
  for ext in ['ads','adb']:
   p=r/folder/(name+'.'+ext)
   if p.exists():data=p.read_bytes();(w/p.name).write_bytes(data);inputs[str(p.relative_to(r))]=hashlib.sha256(data).hexdigest()
(w/'row_tests.adb').write_text('''with Ada.Text_IO;
with Compositor_Row_Copy;
with Compositor_Sampling;
procedure Row_Tests is
 package R renames Compositor_Row_Copy;
 package G renames R.G;
 use type G.Logical_Coordinate, G.Pixel_Edge;
 S : G.Output := (31, 27, G.Unrotated, (1, 1), 0, 0);
 Bounds : G.Logical_Rectangle;
 P : R.Region;
 M : Compositor_Sampling.Sample;
 Count : Natural := 0;
begin
 for O in -3 .. 3 loop
 S.X := G.Output_Origin (O); S.Y := G.Output_Origin (-O);
 for X in -35 .. 35 loop
 for Y in -30 .. 30 loop
 Bounds := (G.Logical_Coordinate (X), G.Logical_Coordinate (Y), G.Logical_Coordinate (X+17), G.Logical_Coordinate (Y+13));
 P := R.Plan (S, Bounds, 17, 13, (2, 3, 30, 25));
 if P.Width > 0 then
 for DY in 0 .. P.Height-1 loop
 for DX in 0 .. P.Width-1 loop
 M := Compositor_Sampling.Map (S, (G.Pixel_Index (P.Target_X+DX), G.Pixel_Index (P.Target_Y+DY)), Bounds, 17, 13);
 pragma Assert (M.Valid and then Natural (M.X)=P.Source_X+DX and then Natural (M.Y)=P.Source_Y+DY);
 Count := Count+1;
 end loop; end loop;
 end if;
 end loop; end loop; end loop;
 S.Scale := (5,4); P := R.Plan (S, (0,0,17,13), 17,13,(0,0,31,27)); pragma Assert (P.Width=0);
 S.Scale := (16,16); S.Rotation := G.Clockwise_90;
 P := R.Plan (S, (0,0,17,13),17,13,(0,0,31,27)); pragma Assert (P.Width=0);
 S.Rotation := G.Unrotated;
 P := R.Plan (S, (G.Logical_Coordinate'First,G.Logical_Coordinate'First,G.Logical_Coordinate'Last,G.Logical_Coordinate'Last),17,13,(0,0,31,27)); pragma Assert (P.Width=0);
 Ada.Text_IO.Put_Line ("PASS row-copy sampler pixels=" & Count'Image);
end Row_Tests;
''')
(w/'test.gpr').write_text('project Test is for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("row_tests.adb"); package Compiler is for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2"); end Compiler; end Test;')
for cmd in [['gprbuild','-q','-p','-P',str(w/'test.gpr')],['gnatprove','-P',str(w/'test.gpr'),'-u','compositor_row_copy.adb','--level=2','--timeout=30','-j2']]:subprocess.run(['alr','exec','--',*cmd],cwd=r/'kernel',check=True)
subprocess.run([str(w/'row_tests')],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'inputs':inputs},indent=2));print(total)

for path,digest in inputs.items():assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
