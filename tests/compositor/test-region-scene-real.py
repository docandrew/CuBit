"""Desktop atlas subregions over retained real wallpaper images; hosted Vulkan."""
from pathlib import Path
import os,subprocess,tempfile,hashlib,json
root=Path(__file__).resolve().parents[2]
assert os.environ.get('IN_NIX_SHELL')
w=Path(tempfile.mkdtemp(prefix='region-scene-real-',dir=root/'tests/compositor/build'));print(w,flush=True)
inputs={}
for directory in ['tests/compositor','userspace/lib/compositor','userspace/services/desktop','userspace/lib/display']:
 for p in (root/directory).iterdir():
  if p.is_file() and p.suffix in ['.ads','.adb','.gpr','.c','.h','.vert','.frag']:inputs[str(p.relative_to(root))]=hashlib.sha256(p.read_bytes()).hexdigest()
s=(root/'tests/compositor/desktop_backdrop_real_bridge.adb').read_text()
s='with Desktop_GPU_Scene.Drawing; with Desktop_Backdrop_Style; with Compositor_Sampling;\n'+s
s=s.replace('   Scene : G.State;','''   Scene : G.State;
   Active_Output : Vulkan_Scene.A.G.Output;
   type Asset_Pixels is array (Natural range <>) of Interfaces.Unsigned_32;
   Wallpaper : constant Asset_Pixels (0 .. 2048 * 576 - 1) with Import, Convention => C, External_Name => "cubit_desktop_wallpaper";
   Cubie : constant Asset_Pixels (0 .. 2048 * 1152 - 1) with Import, Convention => C, External_Name => "cubit_desktop_wallpaper_cubie";
''')
a='''      G.Begin_Frame (Scene, (96, 64, Vulkan_Scene.A.G.Orientation'Val (Version / 96),
        Scales ((Version / 24) mod 4), -20, 10), 0, OK);'''
assert s.count(a)==1;s=s.replace(a,'''      Active_Output := (96, 64, Vulkan_Scene.A.G.Orientation'Val (Version / 96), Scales ((Version / 24) mod 4), -20, 10);
      G.Begin_Frame (Scene, Active_Output, 0, OK);''')
a='      G.Backdrop.Capture (Scene, Style, Source, OK); if not OK then return 3; end if;'
assert s.count(a)==1;s=s.replace(a,a+'''
      if Style.Backdrop in A.Wallpaper | A.Cubie then
         G.Drawing.Image_Region (Scene, Source, (-12, 18, -4, 26), Clip,
           (137, 29, 7, 9, 2048, Interfaces.Unsigned_32 (Desktop_Backdrop_Style.Height (Style.Backdrop))), OK);
         if not OK then return 8; end if;
      end if;''')
s=s.replace('      Area : constant Vulkan_Scene.A.G.Physical_Rectangle := Clip;', '''      Area : constant Vulkan_Scene.A.G.Physical_Rectangle := Clip;
      Reference_Output : constant Vulkan_Scene.A.G.Output :=
        (96, 64, Vulkan_Scene.A.G.Orientation'Val (Version / 96),
         (case (Version / 24) mod 4 is when 0 => (1, 1), when 1 => (5, 4), when 2 => (3, 2), when others => (2, 1)), -20, 10);''')
a='   end Reference;';assert s.count(a)==1
s=s.replace(a,'''      if Style.Backdrop in A.Wallpaper | A.Cubie then
         for Y in Natural (Area.Top) .. Natural (Area.Bottom) - 1 loop
            for X in Natural (Area.Left) .. Natural (Area.Right) - 1 loop
               declare
                  M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                    (Reference_Output, (Vulkan_Scene.A.G.Pixel_Index (X), Vulkan_Scene.A.G.Pixel_Index (Y)),
                     (-12, 18, -4, 26), 7, 9);
               begin
                  if M.Valid then
                     Pixels (Y * 96 + X) := (if Style.Backdrop = A.Cubie then
                       Cubie ((29 + Natural (M.Y)) * 2048 + 137 + Natural (M.X)) else
                       Wallpaper ((29 + Natural (M.Y)) * 2048 + 137 + Natural (M.X)));
                  end if;
               end;
            end loop;
         end loop;
      end if;
   end Reference;''')
(w/'desktop_backdrop_real_bridge.adb').write_text(s)
extra=['desktop_gpu_scene-drawing.ads','desktop_gpu_scene-drawing.adb']
for n in extra:(w/n).write_bytes((root/'userspace/services/desktop'/n).read_bytes())
for n in ['compositor_shadow.ads','compositor_shadow.adb']:(w/n).write_bytes((root/'userspace/lib/compositor'/n).read_bytes());extra.append(n)
files=', '.join('"'+n+'"' for n in ['desktop_backdrop_real_bridge.adb']+extra)
(w/'regions.gpr').write_text(f'''project Regions extends "{root/'tests/compositor/desktop_backdrop_real.gpr'}" is
 for Source_Dirs use ("."); for Source_Files use ({files});
 for Object_Dir use "obj"; for Exec_Dir use ".";
end Regions;
''')
subprocess.run(['python3',str(root/'tests/compositor/build-vulkan-affine-shaders.py'),str(w/'generated')],check=True)
env={**os.environ,'CUBIT_FONT_HOST_ARCHIVE':str(root/'userspace/rust/build/font-host/libcubit_fonts.a'),'C_INCLUDE_PATH':str(w/'generated')+':'+os.environ.get('C_INCLUDE_PATH',''),'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json','XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
subprocess.run(['alr','exec','--','gprbuild','-q','-p','-P',str(w/'regions.gpr')],cwd=root/'kernel',env=env,check=True)
r=subprocess.run([str(w/'desktop_backdrop_real_tests')],env=env,text=True,capture_output=True,timeout=120)
(w/'pixels.log').write_text(r.stdout+r.stderr);print(r.stdout+r.stderr,flush=True);r.check_returncode();assert 'validation errors=0' in r.stdout
for n,h in inputs.items():assert hashlib.sha256((root/n).read_bytes()).hexdigest()==h,n
(w/'result.json').write_text(json.dumps({'status':'PASS','scope':'hosted production Desktop region capture/replay and actual retained Vulkan images; no native GPU execution','inputs':inputs},indent=2))
