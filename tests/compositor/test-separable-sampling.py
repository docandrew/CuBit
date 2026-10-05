from pathlib import Path
import tempfile,subprocess,hashlib,json
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='separable-sampling-',dir=r/'tests/compositor/build'));print(w,flush=True);inputs={}
for folder,names in [('userspace/lib/compositor',['compositor_row_copy','compositor_sampling']),('userspace/lib/display',['cubit-display_geometry']),('userspace/runtime/gnat',['cubit'])]:
 for name in names:
  for ext in ['ads','adb']:
   p=r/folder/(name+'.'+ext)
   if p.exists():data=p.read_bytes();(w/p.name).write_bytes(data);inputs[str(p.relative_to(r))]=hashlib.sha256(data).hexdigest()
(w/'row_tests.adb').write_text('with Ada.Text_IO;\nwith Compositor_Sampling;\nprocedure Row_Tests is\n package S renames Compositor_Sampling;\n package G renames S.G;\n use type G.Pixel_Index;\n Screen : G.Output := (31,27,G.Unrotated,(1,1),0,0);\n Bounds : G.Logical_Rectangle;\n MX, MY : S.Axis_Result;\n M : S.Sample;\n Count : Natural := 0;\nbegin\n for N in 1 .. 16 loop\n for D in 1 .. 16 loop\n Screen.Scale := (G.Scale_Component (N),G.Scale_Component (D));\n for Origin in -2 .. 2 loop\n Screen.X := G.Output_Origin (Origin); Screen.Y := G.Output_Origin (-Origin);\n for Offset in -2 .. 2 loop\n Bounds := (G.Logical_Coordinate (Offset),G.Logical_Coordinate (-Offset),\n            G.Logical_Coordinate (Offset+17),G.Logical_Coordinate (13-Offset));\n for Y in 0 .. 26 loop\n MY := S.Axis (G.Pixel_Index (Y),Screen.Scale,Screen.Y,Bounds.Top,13,23);\n for X in 0 .. 30 loop\n MX := S.Axis (G.Pixel_Index (X),Screen.Scale,Screen.X,Bounds.Left,17,29);\n M := S.Map (Screen,(G.Pixel_Index(X),G.Pixel_Index(Y)),Bounds,29,23);\n pragma Assert (M.Valid = (MX.Valid and MY.Valid));\n if M.Valid then pragma Assert (M.X = MX.Index and M.Y = MY.Index); end if;\n Count := Count+1;\n end loop; end loop; end loop; end loop; end loop; end loop;\n Ada.Text_IO.Put_Line ("PASS separable sampling pixels=" & Count\'Image);\nend Row_Tests;\n')
(w/'test.gpr').write_text('project Test is for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("row_tests.adb"); package Compiler is for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2"); end Compiler; end Test;')
for cmd in [['gprbuild','-q','-p','-P',str(w/'test.gpr')],['gnatprove','-P',str(w/'test.gpr'),'-u','compositor_sampling.adb','--level=2','--timeout=30','-j2']]:subprocess.run(['alr','exec','--',*cmd],cwd=r/'kernel',check=True)
subprocess.run([str(w/'row_tests')],check=True)
report=(w/'obj/gnatprove/gnatprove.out').read_text();total=next(l for l in report.splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'inputs':inputs},indent=2));print(total)

for path,digest in inputs.items():assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
