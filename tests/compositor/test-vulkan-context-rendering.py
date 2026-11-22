"""Run the existing real Vulkan scene oracle through the new context owner.

Invoke in vulkan-affine-shell.nix. All test instrumentation stays private.
"""
from pathlib import Path
import hashlib,json,os,re,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
out=Path(tempfile.mkdtemp(prefix='context-rendering-',dir=root/'tests/compositor/build'));print(out,flush=True)
inputs={}
def copy(p):
 data=p.read_bytes();q=out/p.relative_to(root);q.parent.mkdir(parents=True,exist_ok=True);q.write_bytes(data)
 inputs[str(p.relative_to(root))]=hashlib.sha256(data).hexdigest()
gpr=root/'tests/compositor/vulkan_submission.gpr';text=gpr.read_text()
names=re.findall(r'"([^"]+)"',re.search(r'for Source_Files use \((.*?)\);',text,re.S)[1])
extra=['vulkan_context.c','vulkan_context.h','vulkan_context_owner.ads','vulkan_context_owner.adb','vulkan_context_ffi.ads','vulkan_context_ffi.adb']
for name in names+extra:
 matches=[root/d/name for d in ('tests/compositor','userspace/lib/compositor','userspace/lib/display','userspace/runtime/gnat') if (root/d/name).is_file()]
 assert len(matches)==1,(name,matches)
 copy(matches[0])
for rel in ('userspace/mesa/service-device.h','userspace/lib/compositor/vulkan_affine.vert','userspace/lib/compositor/vulkan_affine.frag','userspace/lib/compositor/vulkan_checker.frag','tests/compositor/build-vulkan-affine-shaders.py'):
 copy(root/rel)
copy(gpr)
p=out/'tests/compositor/vulkan_submission.gpr';s=p.read_text();s=s.replace('for Source_Files use (','for Source_Files use ('+','.join('"'+n+'"' for n in extra)+',',1)
s=s.replace('"-DCUBIT_VULKAN_SUBMISSION_TEST=1"','"-DCUBIT_VULKAN_SUBMISSION_TEST=1", "-DCUBIT_VULKAN_BACKDROP_SCENE_TEST=1"');p.write_text(s)
p=out/'tests/compositor/vulkan_submission_test_bridge.ads';s=p.read_text();s=s.replace('   type Input is record','''   function Context_Open (Description : System.Address) return System.Address
     with Export, Convention => C, External_Name => "test_context_open";
   procedure Context_Children_Retired
     with Export, Convention => C, External_Name => "test_context_children_retired";
   function Context_Close return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_context_close";
   type Input is record''');p.write_text(s)
p=out/'tests/compositor/vulkan_submission_test_bridge.adb';s='with Vulkan_Context_Owner;\n'+p.read_text();anchor='   procedure Damage_Region (';i=s.index(anchor)
s=s[:i]+'''   package CO renames Vulkan_Context_Owner;
   Context_Owner : CO.State;
   Children : CO.Child;
   function Context_Open (Description : System.Address) return System.Address is
      OK : Boolean;
      use type CO.Child;
   begin
      CO.Initialize (Context_Owner, Description, OK);
      if not OK then return System.Null_Address; end if;
      CO.Register_Child (Context_Owner, Children);
      pragma Assert (Children /= CO.No_Child and CO.Held (Context_Owner, Children));
      return CO.Context (Context_Owner);
   end Context_Open;
   procedure Context_Children_Retired is
   begin
      CO.Retire_Child (Context_Owner, Children, True);
   end Context_Children_Retired;
   function Context_Close return Interfaces.C.int is
      OK : Boolean;
   begin
      CO.Close (Context_Owner, Session, OK);
      return (if OK then 0 else 2);
   end Context_Close;
'''+s[i:];p.write_text(s)
p=out/'tests/compositor/vulkan_affine_host.c';s=p.read_text()
s='''#include "vulkan_context.h"
extern void *test_context_open(void *description);
extern void test_context_children_retired(void);
extern int test_context_close(void);
'''+s
start=s.index('    const VkCommandPoolCreateInfo pi=',s.index('int run_vulkan_affine_tests(void)'))
end=s.index('    const VkCommandBufferBeginInfo bi=',start)
s=s[:start]+'''    const struct cubit_mesa_service_device device_view={inst,phy,d,queue,family,vkGetInstanceProcAddr};
    struct cubit_vulkan_context context={0};
    struct cubit_vulkan_context_request context_request={&context,&device_view};
    void *borrowed_context=test_context_open(&context_request);
    CHECK(borrowed_context==&context.submission);
    VkCommandBuffer cmd=context.submission.command; VkFence fence=context.fence;
'''+s[end:]
start=s.index('    VkAttachmentDescription attachment=',s.index('int run_vulkan_affine_tests(void)'))
end=s.index('\n#ifdef CUBIT_VULKAN_OWNED_TARGET_TEST',start)
s=s[:start]+'    VkRenderPass pass=context.pass;'+s[end:]
old='''    struct cubit_vulkan_submission submission;
    CHECK(cubit_vulkan_submission_init(&submission,d,queue,cmd,fence,vkGetDeviceProcAddr)==0);
    submission.status=delayed_fence;test_submission_open(&submission);'''
assert s.count(old)==1;s=s.replace(old,'''    context.submission.status=delayed_fence;test_submission_open(borrowed_context);
    CHECK(test_context_close()==2);''')
s=s.replace('submission_guards(&submission,&mismatch_scene)', 'submission_guards(&context.submission,&mismatch_scene)')
s=s.replace('    vkDestroyRenderPass(d,pass,NULL);','    CHECK(test_context_close()==2);',1)
old='    vkDestroyFence(d,fence,NULL);vkDestroyCommandPool(d,pool,NULL);';assert s.count(old)==1
s=s.replace(old,'''    test_context_children_retired();
    CHECK(test_context_close()==0);CHECK(test_context_close()==2);
    CHECK(!context.live&&!context.pool&&!context.fence&&!context.pass);
    printf("HOST ONLY owned context: actual scene rendering, retained-child rejection and exact context retirement PASS\\n");''')
p.write_text(s)
(out/'original-inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
files=[p for p in out.rglob('*') if p.is_file() and p.name!='original-inputs.json']
frozen={str(p.relative_to(out)):hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
(out/'frozen-inputs.json').write_text(json.dumps(frozen,indent=2)+'\n')
result={'status':'INCOMPLETE','scope':'hosted real Mesa pixel oracle through SPARK context owner; not native CuBit GPU'}
try:
 generated=out/'tests/compositor/build/vulkan-submission/generated'
 subprocess.run(['python3',str(out/'tests/compositor/build-vulkan-affine-shaders.py'),str(generated)],check=True)
 env={**os.environ,'C_INCLUDE_PATH':str(generated)+':'+os.environ.get('C_INCLUDE_PATH',''),
  'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json',
  'XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
 subprocess.run(['gprbuild','-q','-p','-P',str(p.parent/'vulkan_submission.gpr')],cwd=out,env=env,check=True)
 with (out/'pixels.log').open('w') as log:
  subprocess.run([str(out/'tests/compositor/build/vulkan-submission/vulkan_submission_tests')],cwd=out,env=env,check=True,stdout=log,stderr=subprocess.STDOUT,timeout=120)
 log=(out/'pixels.log').read_text();print(log,flush=True)
 assert 'HOST ONLY owned context: actual scene rendering, retained-child rejection and exact context retirement PASS' in log
 assert 'HOST ONLY retained wallpaper: 96 ordered scenes' in log and 'validation errors=0' in log
 for rel,h in frozen.items():assert hashlib.sha256((out/rel).read_bytes()).hexdigest()==h,rel
 result.update(status='PASS',root_drift=[rel for rel,h in inputs.items() if hashlib.sha256((root/rel).read_bytes()).hexdigest()!=h])
finally:(out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
