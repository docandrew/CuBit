"""Full Desktop shadow capture -> SPARK scene replay -> real Vulkan pixels.

Private hosted fixture reuses the existing glyph/lifetime oracle. Run in Nix.
"""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
ROOT=Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL")
work=Path(tempfile.mkdtemp(prefix="shadow-scene-real-",dir=ROOT/"tests/compositor/build"));print(work,flush=True)
inputs={}
for directory in ("tests/compositor","userspace/lib/compositor","userspace/lib/display","userspace/services/desktop","userspace/runtime/gnat","userspace/mesa"):
    for p in (ROOT/directory).iterdir():
        if p.is_file() and p.suffix in (".c",".h",".ads",".adb",".gpr",".vert",".frag"):
            inputs[str(p.relative_to(ROOT))]=hashlib.sha256(p.read_bytes()).hexdigest()
font=ROOT/"userspace/rust/build/font-host/libcubit_fonts.a"
inputs[str(font.relative_to(ROOT))]=hashlib.sha256(font.read_bytes()).hexdigest()
ada=(ROOT/"tests/compositor/desktop_gpu_scene_real_bridge.adb").read_text()
anchor='      G.Drawing.Text (Scene, Items, 32, (0, 0, 96, 64), 16#FFFFFFFF#, OK, Key.Face);'
assert ada.count(anchor)==1
ada=ada.replace(anchor,anchor+'''
      if not OK then return; end if;
      G.Drawing.Shadow (Scene, (35, 20, 42, 24), (0, 0, 96, 64), 16#2468AC#, OK);''')
(work/"desktop_gpu_scene_real_bridge.adb").write_text(ada)
c=(ROOT/"tests/compositor/desktop_gpu_scene_real_host.c").read_text()
anchor='            uint32_t expected=0xff000000u|alpha*0x010101u;'
assert c.count(anchor)==1
c=c.replace(anchor,anchor+'''
            /* Forward logical-cell union, independently of the GPU inverse
               coverage implementation. Two strips match the Desktop loop. */
            for(unsigned cy=23;cy<27;cy++)for(unsigned cx=38;cx<45;cx++){
                const int in_shadow=cx>=42||(cy>=24&&cx<42);
                if(in_shadow&&(cx+cy)%2==0&&x>=cx*n/d&&x<((cx+1)*n+d-1)/d&&
                   y>=cy*n/d&&y<((cy+1)*n+d-1)/d)expected=0xff2468acu;
            }''')
c=c.replace('HOST ONLY real fonts -> owned Vulkan staging -> glyph scene:', 'HOST ONLY real fonts + bounded shadow -> SPARK scene -> Vulkan:')
(work/"desktop_gpu_scene_real_host.c").write_text(c)
(work/"shadow.gpr").write_text(f'''project Shadow extends "{ROOT/'tests/compositor/desktop_gpu_scene_real.gpr'}" is
   for Source_Dirs use (".");
   for Source_Files use ("desktop_gpu_scene_real_bridge.adb", "desktop_gpu_scene_real_host.c");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
end Shadow;
''')
(work/"inputs.json").write_text(json.dumps(inputs,indent=2)+"\n")
subprocess.run(["python3",str(ROOT/"tests/compositor/build-vulkan-affine-shaders.py"),str(work/"generated")],check=True)
env={**os.environ,"CUBIT_FONT_HOST_ARCHIVE":str(font),"C_INCLUDE_PATH":str(work/"generated")+":"+os.environ.get("C_INCLUDE_PATH",""),
     "VK_DRIVER_FILES":os.environ["MESA_DRIVER_ROOT"]+"/share/vulkan/icd.d/lvp_icd.x86_64.json","XDG_DATA_DIRS":os.environ["MESA_DRIVER_ROOT"]+"/share"}
subprocess.run(["alr","exec","--","gprbuild","-q","-p","-P",str(work/"shadow.gpr")],cwd=ROOT/"kernel",env=env,check=True)
run=subprocess.run([str(work/"desktop_gpu_scene_real_tests")],env=env,text=True,capture_output=True,timeout=120)
(work/"pixels.log").write_text(run.stdout+run.stderr);print(run.stdout+run.stderr,end="",flush=True);run.check_returncode()
assert "bounded shadow -> SPARK scene -> Vulkan" in run.stdout and "validation errors=0" in run.stdout
for n,h in inputs.items():assert hashlib.sha256((ROOT/n).read_bytes()).hexdigest()==h,n
(work/"result.json").write_text(json.dumps({"status":"PASS","scope":"hosted full SPARK Desktop capture/replay through real Vulkan and lifecycle; not native CuBit"})+"\n")
