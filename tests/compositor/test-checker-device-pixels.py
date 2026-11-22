"""Exercise the real checker device adapter inside the owned-target oracle.

Uses private generated host/GPR/output files. Run in vulkan-affine-shell.nix.
This is hosted lavapipe, not CuBit execution or hardware scanout.
"""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
ROOT=Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL")
work=Path(tempfile.mkdtemp(prefix="checker-device-pixels-",dir=ROOT/"tests/compositor/build"));print(work,flush=True)
inputs={}
for directory in ("tests/compositor","userspace/lib/compositor","userspace/lib/display","userspace/runtime/gnat","userspace/mesa"):
    for p in (ROOT/directory).iterdir():
        if p.is_file() and p.suffix in (".c",".h",".ads",".adb",".gpr",".vert",".frag"):
            inputs[str(p.relative_to(ROOT))]=hashlib.sha256(p.read_bytes()).hexdigest()
source=(ROOT/"tests/compositor/vulkan_affine_host.c").read_text()
def replace(a,b):
    global source
    assert source.count(a)==1,(a[:80],source.count(a))
    source=source.replace(a,b,1)
replace('#include "vulkan_affine.h"','#include "vulkan_affine.h"\n#include "vulkan_checker.h"')
replace('vkCmdClearAttachments(command,1,&color,1,&area);vkCmdEndRenderPass(command);','''vkCmdClearAttachments(command,1,&color,1,&area);
        struct cubit_vulkan_checker_request checker={.left=0,.top=0,.right=65535,.bottom=65535,
            .numerator=1,.denominator=1,.width=W,.height=H,.rotation=i,
            .clip_x=3,.clip_y=4,.clip_w=W-6,.clip_h=H-8,.rgb=0x123456};
        CHECK(cubit_vulkan_device_checker_record(context_request,&checker)==1);
        checker.width=W+1;CHECK(cubit_vulkan_device_checker_record(owned,&checker)==1);checker.width=W;
        CHECK(cubit_vulkan_device_checker_record(owned,&checker)==0);
        vkCmdEndRenderPass(command);''')
replace('for(unsigned j=0;j<W*H;j++)CHECK(((uint32_t *)pixels)[j]==expected[i]);','''for(unsigned j=0;j<W*H;j++){
            unsigned x=j%W,y=j/W,u=x,v=y;
            if(i==1){u=y;v=W-1-x;}else if(i==2){u=W-1-x;v=H-1-y;}
            const int checker=x>=3&&x<W-3&&y>=4&&y<H-4&&((u+v)%2==0);
            CHECK(((uint32_t *)pixels)[j]==(checker?0xff123456:expected[i]));
        }
        if(i==2)puts("HOST ONLY checker device adapter: three owned targets, context/extent rejection and 2304 exact pixels PASS");''')
(work/"vulkan_affine_host.c").write_text(source)
(work/"checker.gpr").write_text(f'''project Checker extends "{ROOT/'tests/compositor/vulkan_target_bundle.gpr'}" is
   for Source_Dirs use (".");
   for Source_Files use ("vulkan_affine_host.c");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
end Checker;
''')
(work/"inputs.json").write_text(json.dumps(inputs,indent=2)+"\n")
subprocess.run(["python3",str(ROOT/"tests/compositor/build-vulkan-affine-shaders.py"),str(work/"generated")],check=True)
env={**os.environ,"C_INCLUDE_PATH":str(work/"generated")+":"+os.environ.get("C_INCLUDE_PATH",""),
     "VK_DRIVER_FILES":os.environ["MESA_DRIVER_ROOT"]+"/share/vulkan/icd.d/lvp_icd.x86_64.json",
     "XDG_DATA_DIRS":os.environ["MESA_DRIVER_ROOT"]+"/share"}
subprocess.run(["alr","exec","--","gprbuild","-q","-p","-P",str(work/"checker.gpr")],cwd=ROOT/"kernel",env=env,check=True)
run=subprocess.run([str(work/"vulkan_target_bundle_tests")],env=env,text=True,capture_output=True,timeout=120)
(work/"pixels.log").write_text(run.stdout+run.stderr);print(run.stdout+run.stderr,end="",flush=True);run.check_returncode()
assert "checker device adapter:" in run.stdout and "validation errors=0" in run.stdout
for n,h in inputs.items():assert hashlib.sha256((ROOT/n).read_bytes()).hexdigest()==h,n
(work/"result.json").write_text(json.dumps({"status":"PASS","scope":"hosted real device adapter and Vulkan target/resource regressions; no native boot"})+"\n")
