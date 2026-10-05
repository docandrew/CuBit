"""Packed icon atlas ownership and regions over real Vulkan images; hosted Vulkan."""
from pathlib import Path
import os,subprocess,tempfile,hashlib,json,argparse
parser=argparse.ArgumentParser()
parser.add_argument("--cursors",action="store_true")
args=parser.parse_args()
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='icon-atlas-real-',dir=root/'tests/compositor/build'));print(w,flush=True)
inputs={}
for directory in ['tests/compositor','userspace/lib/compositor','userspace/services/desktop','userspace/lib/display']:
 for p in (root/directory).iterdir():
  if p.is_file() and p.suffix in ['.ads','.adb','.gpr','.c','.h','.vert','.frag']:inputs[str(p.relative_to(root))]=hashlib.sha256(p.read_bytes()).hexdigest()
s=(root/'tests/compositor/desktop_backdrop_real_bridge.adb').read_text()
s='with Desktop_GPU_Scene.Drawing; with Desktop_Icon_Pixels.Atlases; with Desktop_Icons; with Desktop_Window_Icons; with Compositor_Sampling;\n'+s
s=s.replace('Desktop_Backdrop_Owner','Desktop_Icon_Atlas_Owner')
s=s.replace('   Scene : G.State;', '''   package I renames Desktop_Icon_Pixels;
   use type Interfaces.Unsigned_32, Vulkan_Scene.A.G.Pixel_Edge;
   Scene : G.State;
   function Item return I.Asset;
''')
s=s.replace('   Style : A.Preferences;', '''   function Family return I.Family;
''')
a='   function Open return Interfaces.C.int is'
s=s.replace(a,'''   function Family return I.Family is (if Selected = 0 then I.Application else I.Window_Control);
   function Item return I.Asset is
     (if Selected = 0 then (I.Application, Desktop_Icons.Icon_ID'Val ((Version / 2) mod 8))
      else (I.Window_Control, Desktop_Window_Icons.Icon_ID'Val ((Version / 2) mod 5)));
'''+a)
s=s.replace('      N : constant Natural := Natural (Version) mod 24;','')
a=s.index('      Style :=');b=s.index('      O.Acquire',a)
s=s[:a]+'      Selected := Natural (Version) mod 2;\n'+s[b:]
s=s.replace('O.Slot (128 + Selected), Style.Backdrop','O.Slot (130 + Selected), Family')
s=s.replace('      if Style.Backdrop in A.Wallpaper | A.Cubie then','      if True then')
s=s.replace('         if G.Image_Reader_Count (Scene) /= 1', '         if Clip.Left >= Clip.Right or else Clip.Top >= Clip.Bottom then\n            return (if G.Image_Reader_Count (Scene) = 0 then 0 else 9);\n         end if;\n         if G.Image_Reader_Count (Scene) /= 1')
s=s.replace('      G.Backdrop.Capture (Scene, Style, Source, OK); if not OK then return 3; end if;', '''      G.Drawing.Image_Region (Scene, Source, (-12, 18, 12, 42), Clip,
        I.Atlases.Region (Item), OK, Over => True, Straight_Alpha => True);
      if not OK then return 3; end if;''')
a=s.index('      Desktop_Wallpaper.Paint');b=s.index('   end Reference;',a)
s=s[:a]+'''      for Y in Natural (Area.Top) .. Natural (Area.Bottom) - 1 loop
         for X in Natural (Area.Left) .. Natural (Area.Right) - 1 loop
            declare
               M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                 ((96, 64, Vulkan_Scene.A.G.Orientation'Val (Version / 96),
                   (case (Version / 24) mod 4 is when 0 => (1, 1), when 1 => (5, 4), when 2 => (3, 2), when others => (2, 1)), -20, 10),
                  (Vulkan_Scene.A.G.Pixel_Index (X), Vulkan_Scene.A.G.Pixel_Index (Y)),
                  (-12, 18, 12, 42), Vulkan_Scene.A.G.Physical_Extent (I.Size (Item)), Vulkan_Scene.A.G.Physical_Extent (I.Size (Item)));
            begin
               if M.Valid then
                  declare
                     P : constant Interfaces.Unsigned_32 := I.Pixel (Item, Natural (M.X), Natural (M.Y));
                     Alpha : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right (P, 24);
                     function Channel (Shift : Natural) return Interfaces.Unsigned_32 is
                       (Interfaces.Shift_Left (((Interfaces.Shift_Right (P, Shift) and 255) * Alpha + 127) / 255, Shift));
                  begin
                     Pixels (Y * 96 + X) := 16#FF000000# or Channel (0) or Channel (8) or Channel (16);
                  end;
               end if;
            end;
         end loop;
      end loop;
'''+s[b:]
s=s.replace('G.Outcome, G.Phase', 'G.Outcome, G.Output_Completion, G.Phase')
s=s.replace('Result : G.Outcome', 'Result : G.Output_Completion')
s=s.replace('G.Finish (Scene, Result)', 'G.Complete_Output (Scene, False, True, Result)')
s=s.replace('G.Poll (Scene, Result)', 'G.Complete_Output (Scene, True, True, Result)')
s=s.replace('Result = G.Pending', 'Result = G.Output_Pending').replace('Result /= G.Complete', 'Result /= G.Output_Complete')
if args.cursors:
 s='with Compositor_Cursor; with Desktop_Cursors;\n'+s
 s=s.replace('   function Item return I.Asset;', '''   function Item return I.Asset;
   function Cursor return Desktop_Cursors.Cursor_ID;
   function Shape return Vulkan_Scene.A.G.Logical_Rectangle;
   function Sample_Width return Positive;
   function Sample_Height return Positive;
''')
 s=s.replace('   function Open return Interfaces.C.int is', '''   function Cursor return Desktop_Cursors.Cursor_ID is
     (Desktop_Cursors.Cursor_ID'Val ((Version / 2) mod 5));
   function Sample_Width return Positive is
     (if Selected = 1 then Desktop_Cursors.Metadata (Cursor).Width else I.Size (Item));
   function Sample_Height return Positive is
     (if Selected = 1 then Desktop_Cursors.Metadata (Cursor).Height else I.Size (Item));
   function Shape return Vulkan_Scene.A.G.Logical_Rectangle is
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Cursor);
      X : constant Vulkan_Scene.A.G.Logical_Coordinate := -12 - Vulkan_Scene.A.G.Logical_Coordinate (M.Hotspot_X);
      Y : constant Vulkan_Scene.A.G.Logical_Coordinate := 18 - Vulkan_Scene.A.G.Logical_Coordinate (M.Hotspot_Y);
   begin
      if Selected = 0 then return (-12, 18, 12, 42); end if;
      return (X, Y, X + Vulkan_Scene.A.G.Logical_Coordinate (M.Width), Y + Vulkan_Scene.A.G.Logical_Coordinate (M.Height));
   end Shape;
   function Open return Interfaces.C.int is''')
 s=s.replace('   function Open return Interfaces.C.int is', '''   function GPU_Shape return Vulkan_Scene.A.G.Logical_Rectangle is
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Cursor);
      P : constant Compositor_Cursor.Plan := Compositor_Cursor.Build
        ((-12, 18), M.Width, M.Height, M.Hotspot_X, M.Hotspot_Y);
   begin
      if Selected = 0 then return (-12, 18, 12, 42); end if;
      if not P.Valid then raise Program_Error; end if;
      return P.Surface;
   end GPU_Shape;
   function Open return Interfaces.C.int is''')
 s=s.replace('(-12, 18, 12, 42), Clip', 'GPU_Shape, Clip')
 s=s.replace('I.Atlases.Region (Item), OK, Over => True, Straight_Alpha => True', '(if Selected = 1 then I.Atlases.Cursor_Region (Cursor) else I.Atlases.Region (Item)), OK, Over => True, Straight_Alpha => Selected = 0')
 s=s.replace('(-12, 18, 12, 42), Vulkan_Scene.A.G.Physical_Extent (I.Size (Item)), Vulkan_Scene.A.G.Physical_Extent (I.Size (Item))', 'Shape, Vulkan_Scene.A.G.Physical_Extent (Sample_Width), Vulkan_Scene.A.G.Physical_Extent (Sample_Height)')
 s=s.replace('I.Pixel (Item, Natural (M.X), Natural (M.Y))', '(if Selected = 1 then Desktop_Cursors.Pixels (Desktop_Cursors.Metadata (Cursor).Offset + Natural (M.Y) * Sample_Width + Natural (M.X)) else I.Pixel (Item, Natural (M.X), Natural (M.Y)))')
 s=s.replace('((Interfaces.Shift_Right (P, Shift) and 255) * Alpha + 127) / 255', '(if Selected = 1 then Interfaces.Shift_Right (P, Shift) and 255 else ((Interfaces.Shift_Right (P, Shift) and 255) * Alpha + 127) / 255)')
(w/'desktop_backdrop_real_bridge.adb').write_text(s)
host=(root/'tests/compositor/desktop_backdrop_real_host.c').read_text().replace('216','2').replace('retained wallpaper','retained icon atlases')
host=host.replace('CHECK(desktop_backdrop_real_import()==0);', 'int imported=desktop_backdrop_real_import(); if(imported)fprintf(stderr,"icon version=%u import=%d\\n",version,imported); CHECK(imported==0);')
(w/'desktop_backdrop_real_host.c').write_text(host)
extra=['desktop_gpu_scene-drawing.ads','desktop_gpu_scene-drawing.adb','desktop_window_icons.ads','desktop_cursors.ads']
extra += [p.name for p in (root/'userspace/services/desktop').glob('desktop_icon*.ad?')]
for n in extra:(w/n).write_bytes((root/'userspace/services/desktop'/n).read_bytes())
for n in ['compositor_shadow.ads','compositor_shadow.adb','compositor_cursor.ads','compositor_cursor.adb']:(w/n).write_bytes((root/'userspace/lib/compositor'/n).read_bytes());extra.append(n)
files=', '.join('"'+n+'"' for n in ['desktop_backdrop_real_bridge.adb','desktop_backdrop_real_host.c']+extra)
(w/'icons.gpr').write_text(f'''project Icons extends "{root/'tests/compositor/desktop_backdrop_real.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({files});
 for Object_Dir use "obj"; for Exec_Dir use ".";
end Icons;
''')
subprocess.run(['python3',str(root/'tests/compositor/build-vulkan-affine-shaders.py'),str(w/'generated')],check=True)
env={**os.environ,'CUBIT_FONT_HOST_ARCHIVE':str(root/'userspace/rust/build/font-host/libcubit_fonts.a'),'C_INCLUDE_PATH':str(w/'generated')+':'+os.environ.get('C_INCLUDE_PATH',''),'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json','XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(w/'icons.gpr')],cwd=root/'kernel',env=env,check=True)
r=subprocess.run([str(w/'desktop_backdrop_real_tests')],env=env,text=True,capture_output=True,timeout=120)
(w/'pixels.log').write_text(r.stdout+r.stderr);print(r.stdout+r.stderr,flush=True);r.check_returncode();assert 'validation errors=0' in r.stdout
for n,h in inputs.items():assert hashlib.sha256((root/n).read_bytes()).hexdigest()==h,n
(w/'result.json').write_text(json.dumps({'status':'PASS','scope':'hosted packed asset upload/residency and scene replay; cursor mode='+str(args.cursors)+'; no native GPU execution','inputs':inputs},indent=2))
