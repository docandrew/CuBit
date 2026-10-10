"""Link-only checkpoint; run in vulkan-affine-shell.nix under build.lock.

Copies Desktop into a private tree, includes Mesa/context bindings in its own
Ada binder and links the verified production Mesa bundle. Default mode only links. Explicit startup options build private admission
fixtures; --admitted-startup can initialize an admitted Mesa device/context.
No mode enables GPU drawing or stages an executable.
"""
from pathlib import Path
import argparse,hashlib,importlib.util,json,os,shutil,subprocess
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--device-startup-probe', action='store_true', help='exercise no-authority startup/retirement in private Desktop')
parser.add_argument('--optional-render-probe', action='store_true', help='test-only optional capability metadata and empty-slot check')
parser.add_argument('--prepare-pipeline', action='store_true', help='opt-in existing Vulkan texture pipeline/source pool startup; requires target preparation')
parser.add_argument('--prepare-targets', action='store_true', help='opt-in first-output GPU target allocation with a 64 MiB image budget; no GPU drawing')
parser.add_argument('--admitted-startup', action='store_true', help='opt-in real Mesa startup on an inspected admitted render endpoint; software drawing retained')
parser.add_argument('bundle',type=Path)
parser.add_argument('mesa_source',type=Path)
parser.add_argument('output',type=Path,help='new directory')
args=parser.parse_args()
if args.optional_render_probe and not (args.device_startup_probe or args.admitted_startup):parser.error('--optional-render-probe requires a startup mode')
if args.prepare_pipeline and not args.prepare_targets:parser.error('--prepare-pipeline requires --prepare-targets')
if args.prepare_targets and not args.admitted_startup:parser.error('--prepare-targets requires --admitted-startup')
if args.admitted_startup and (not args.optional_render_probe or args.device_startup_probe):parser.error('--admitted-startup requires --optional-render-probe and excludes --device-startup-probe')
root=Path(__file__).resolve().parents[1]
if not os.environ.get('IN_NIX_SHELL'):raise SystemExit('Use the pinned Nix development environment')
for command in ('glslangValidator','spirv-val','alr','nm'):
 if not shutil.which(command):raise SystemExit('Missing '+command+'; use tests/compositor/vulkan-affine-shell.nix')
spec=importlib.util.spec_from_file_location('bundle',root/'tools/verify_mesa_service_bundle.py')
bundle=importlib.util.module_from_spec(spec);spec.loader.exec_module(bundle)
prefix,flags=bundle.verify(args.bundle.resolve())
mesa=args.mesa_source.resolve();out=args.output.resolve();out.mkdir(parents=True,exist_ok=False)
inputs={}
def copy(path):
 data=path.read_bytes();dest=out/path.relative_to(root);dest.parent.mkdir(parents=True,exist_ok=True);dest.write_bytes(data)
 inputs[str(path.relative_to(root))]=hashlib.sha256(data).hexdigest()
for rel in ('userspace/services/desktop','userspace/lib/compositor','userspace/lib/display','userspace/lib/image','userspace/lib/theme',
 'userspace/lib/ui','userspace/ccl/src','userspace/allocator/src','userspace/services/display/production'):
 for path in (root/rel).rglob('*'):
  if path.is_file() and path.suffix in ('.ads','.adb','.gpr','.c','.h','.vert','.frag') and not any(p.startswith('build') for p in path.relative_to(root/rel).parts):copy(path)
for rel in ('userspace/runtime/gnat','userspace/runtime/adalib'):
 for path in (root/rel).rglob('*'):
  if path.is_file():copy(path)
for rel in ('userspace/runtime/ada_source_path','userspace/runtime/ada_object_path','userspace/runtime/runtime.xml','userspace/runtime/target_properties',
 'userspace/mesa/mesa_service.ads','userspace/mesa/mesa_service.adb','userspace/mesa/service-device.h',
 'userspace/services/desktop/build/generated/ccl_manifest_bindings.ads','userspace/services/desktop/build/manifest.o',
 'userspace/rust/build/font-native/libcubit_fonts.a','tests/compositor/build-vulkan-affine-shaders.py'):
 copy(root/rel)
desktop=out/'userspace/services/desktop';main=desktop/'main.adb';main.write_text('with Mesa_Service;\npragma Elaborate_All (Mesa_Service);\nwith Vulkan_Context_Owner;\npragma Elaborate_All (Vulkan_Context_Owner);\n'+main.read_text())
if args.optional_render_probe:
 helper=root/'tests/render-startup/desktop_optional_manifest.py';copy(helper)
 spec=importlib.util.spec_from_file_location('optional_manifest',out/helper.relative_to(root))
 optional=importlib.util.module_from_spec(spec);spec.loader.exec_module(optional)
 caps=out/'original.caps';manifest=desktop/'build/manifest.o'
 subprocess.run(['objcopy','--dump-section','.cubit.caps='+str(caps),str(manifest)],check=True)
 original=caps.read_bytes();modified=optional.append_optional(original)
 caps=out/'optional.caps';caps.write_bytes(modified)
 subprocess.run(['objcopy','--update-section','.cubit.caps='+str(caps),str(manifest)],check=True)
 verify=out/'verified.caps'
 subprocess.run(['objcopy','--dump-section','.cubit.caps='+str(verify),str(manifest)],check=True)
 assert verify.read_bytes()==modified
 (out/'optional-render.json').write_text(json.dumps({'test_only':True,'slot':62,'original_sha256':hashlib.sha256(original).hexdigest(),'optional_sha256':hashlib.sha256(modified).hexdigest()},indent=2)+'\n')
if args.device_startup_probe:
 source=main.read_text()
 source='with Vulkan_Device_Owner;\nwith Vulkan_Submission;\n'+source
 anchor='   displayInfoOk : Boolean := False;\nbegin\n'
 assert source.count(anchor)==1
 source=source.replace(anchor, '   displayInfoOk : Boolean := False;\n'
  '   GPU_Device : Vulkan_Device_Owner.State;\n'
  '   GPU_Context : Vulkan_Context_Owner.State;\n'
  '   GPU_Submission : Vulkan_Submission.State := Vulkan_Submission.Open (System.Null_Address);\n'
  '   use type Vulkan_Device_Owner.Phase;\nbegin\n'
  '   Vulkan_Device_Owner.Start (GPU_Device, GPU_Context, GPU_Submission, 0);\n'
  '   if Vulkan_Device_Owner.Current (GPU_Device) /= Vulkan_Device_Owner.Software then\n'
  '      debugPrint ("DESKTOP-GPU-STARTUP: FAIL software" & LF); return;\n'
  '   end if;\n'
  '   Vulkan_Device_Owner.Close (GPU_Device, GPU_Context, GPU_Submission);\n'
  '   if Vulkan_Device_Owner.Current (GPU_Device) /= Vulkan_Device_Owner.Retired then\n'
  '      debugPrint ("DESKTOP-GPU-STARTUP: FAIL retirement" & LF); return;\n'
  '   end if;\n'
  '   debugPrint ("DESKTOP-GPU-STARTUP: PASS no authority" & LF);\n')
 if args.optional_render_probe:
  source=source.replace('   use type Vulkan_Device_Owner.Phase;\nbegin\n',
   '   use type Vulkan_Device_Owner.Phase;\n'
   '   type Probe_Words is array (0 .. 5) of Unsigned_64;\n'
   '   Probe_Cap : aliased Probe_Words := (others => 0);\n'
   '   Probe_Result : Unsigned_64;\nbegin\n'
   '   Probe_Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), 62,\n'
   "      Unsigned_64 (System.Storage_Elements.To_Integer (Probe_Cap'Address)));\n"
   '   if Probe_Result /= 1 or else Probe_Cap (0) /= 0 then\n'
   '      debugPrint ("DESKTOP-OPTIONAL-RENDER: FAIL slot" & LF); return;\n'
   '   end if;\n'
   '   debugPrint ("DESKTOP-OPTIONAL-RENDER: PASS empty slot" & LF);\n')
 main.write_text(source)
if args.admitted_startup:
 source='with Desktop_Vulkan_Startup;\nwith Vulkan_Device_Owner;\n'+main.read_text()
 anchor='   displayInfoOk : Boolean := False;\nbegin\n'
 assert source.count(anchor)==1
 source=source.replace(anchor, '   displayInfoOk : Boolean := False;\n'
  '   use type Vulkan_Device_Owner.Phase;\n'
  '   type Probe_Words is array (0 .. 5) of Unsigned_64;\n'
  '   Probe_Cap : aliased Probe_Words := (others => 0);\n'
  '   Probe_Result : Unsigned_64;\n'
  '   GPU_Usable : Boolean;\n'
  '   function GPU_Phase_Name return String is\n'
  '   begin\n'
  '      case Desktop_Vulkan_Startup.Current is\n'
  '         when Vulkan_Device_Owner.Fresh => return "FRESH";\n'
  '         when Vulkan_Device_Owner.Software => return "SOFTWARE";\n'
  '         when Vulkan_Device_Owner.Ready => return "READY";\n'
  '         when Vulkan_Device_Owner.Retiring => return "RETIRING";\n'
  '         when Vulkan_Device_Owner.Retired => return "RETIRED";\n'
  '         when Vulkan_Device_Owner.Quarantined => return "QUARANTINED";\n'
  '      end case;\n'
  '   end GPU_Phase_Name;\nbegin\n'
  '   Probe_Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), 62,\n'
  "      Unsigned_64 (System.Storage_Elements.To_Integer (Probe_Cap'Address)));\n"
  '   if Probe_Result = 1 and then Probe_Cap (0) = 0 then\n'
  '      debugPrint ("DESKTOP-OPTIONAL-RENDER: PASS empty slot" & LF);\n'
  '      Desktop_Vulkan_Startup.Initialize (0);\n'
  '   elsif Probe_Result = 1 and then Probe_Cap (0) = 1 and then\n'
  '     Probe_Cap (1) = 3 and then Probe_Cap (3) /= 0\n'
  '   then\n'
  '      debugPrint ("DESKTOP-VULKAN: admitted endpoint; starting Mesa" & LF);\n'
  '      Desktop_Vulkan_Startup.Initialize (62);\n'
  '   else\n'
  '      debugPrint ("DESKTOP-VULKAN: invalid render authority; software retained" & LF);\n'
  '      Desktop_Vulkan_Startup.Initialize (0);\n'
  '   end if;\n'
  '   debugPrint ("DESKTOP-VULKAN: startup=" &\n'
  "      GPU_Phase_Name & LF);\n"
  '   if Desktop_Vulkan_Startup.Current = Vulkan_Device_Owner.Ready then\n'
  '      Desktop_Vulkan_Startup.Check_Health (GPU_Usable);\n'
  '      debugPrint ("DESKTOP-VULKAN: initial health=" & Boolean\'Image (GPU_Usable) & LF);\n'
  '   elsif Desktop_Vulkan_Startup.Current = Vulkan_Device_Owner.Software then\n'
  '      Desktop_Vulkan_Startup.Stop;\n'
  '   end if;\n')
 anchor='   releaseDisplayBuffer;\n\n   if syscall (SYSCALL_EXIT'
 assert source.count(anchor)==1
 source=source.replace(anchor, '   releaseDisplayBuffer;\n'
  '   Desktop_Vulkan_Startup.Stop;\n'
  '   debugPrint ("DESKTOP-VULKAN: shutdown=" &\n'
  "      GPU_Phase_Name & LF);\n\n"
  '   if syscall (SYSCALL_EXIT',1)
 if args.prepare_targets:
  anchor='   debugPrint ("desktop: display info ready" & LF);\n'
  assert source.count(anchor)==1
  source=source.replace(anchor, anchor+
   '   if Desktop_Vulkan_Startup.Can_Prepare_Targets then\n'
   '      Desktop_Vulkan_Startup.Configure_Targets\n'
   '        (Unsigned_32 (fbWidth), Unsigned_32 (fbHeight), 1, 64 * 1024 * 1024, GPU_Usable);\n'
   '      if GPU_Usable then\n'
   '         debugPrint ("DESKTOP-VULKAN: targets ready=TRUE" & LF);\n'
   '      else\n'
   '         debugPrint ("DESKTOP-VULKAN: targets ready=FALSE; software retained" & LF);\n'
   '      end if;\n'
   '      debugPrint ("DESKTOP-VULKAN: target bytes=" &\n'
   "         Natural'Image (Desktop_Vulkan_Startup.Charged_Bytes) &\n"
   '         " limit=" & Natural\'Image (Desktop_Vulkan_Startup.Configured_Limit) & LF);\n'
   '   else\n'
   '      debugPrint ("DESKTOP-VULKAN: targets skipped; device unavailable" & LF);\n'
   '   end if;\n',1)
 if args.prepare_pipeline:
  anchor='         debugPrint ("DESKTOP-VULKAN: targets ready=TRUE" & LF);\n'
  assert source.count(anchor)==1
  source=source.replace(anchor, anchor+
   '         Desktop_Vulkan_Startup.Prepare_Pipeline (GPU_Usable);\n'
   '         if GPU_Usable then\n'
   '            debugPrint ("DESKTOP-VULKAN: pipeline ready=TRUE" & LF);\n'
   '         else\n'
   '            debugPrint ("DESKTOP-VULKAN: pipeline ready=FALSE; software retained" & LF);\n'
   '         end if;\n',1)
 main.write_text(source)
gpr=desktop/'desktop.gpr';text=gpr.read_text();assert text.count('for Source_Dirs use (')==2
text=text.replace('for Source_Dirs use (','for Source_Dirs use ("../../mesa", ');gpr.write_text(text)
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
env={**os.environ,'CUBIT_STACK_SIZE':'16777216','NIX_HARDENING_ENABLE':''}
def run(command,cwd=out):subprocess.run(list(map(str,command)),cwd=cwd,env=env,check=True)
result={'status':'INCOMPLETE','gpu_enabled':False,'gpu_drawing_enabled':False,'admitted_device_startup_enabled':args.admitted_startup,'executed':False,'device_startup_probe':args.device_startup_probe,'optional_render_probe':args.optional_render_probe,'admitted_startup':args.admitted_startup,'prepare_targets':args.prepare_targets,'prepare_pipeline':args.prepare_pipeline}
try:
 run(['alr','exec','--','gprbuild','-q','-p','-c','-b','-P',gpr,'-XCUBIT_COMPOSITOR=legacy'],root/'kernel')
 generated=out/'generated';run(['python3',out/'tests/compositor/build-vulkan-affine-shaders.py',generated])
 objects=[]
 for name in ('vulkan_context','vulkan_submission_native','vulkan_affine','vulkan_backdrop','vulkan_sources','vulkan_device_storage','vulkan_upload_buffer','vulkan_upload_record','vulkan_targets','vulkan_owned_image','vulkan_owned_target_binding'):
  obj=out/(name+'.o');objects.append(obj)
  run(['bash',root/'tests/mesa-anv/native-compiler.sh','c','-std=c11','-O2','-Wall','-Wextra','-Werror',
   '-I'+str(mesa/'include'),'-I'+str(generated),'-c',out/'userspace/lib/compositor'/(name+'.c'),'-o',obj])
 directory=desktop/'build';exchange=(directory/'main.bexch').read_text()
 bound=exchange.split('[BOUND OBJECT FILES]\n',1)[1].split('\n[',1)[0].splitlines()
 exe=out/'desktop-vulkan-link.svc'
 run([*prefix,'--manifest',directory/'manifest.o',directory/'b__main.o',*bound,*objects,
  out/'userspace/rust/build/font-native/libcubit_fonts.a',*flags,'-o',exe])
 if args.optional_render_probe:
  final_caps=out/'linked.caps'
  run(['objcopy','--dump-section','.cubit.caps='+str(final_caps),exe])
  assert final_caps.read_bytes()==modified
 assert not subprocess.check_output(['nm','-u',str(exe)],text=True).strip()
 symbols={line.split()[-1] for line in subprocess.check_output(['nm','--defined-only',str(exe)],text=True).splitlines() if line.split()}
 required=['mesa_service__start','mesa_service__close','vulkan_context_owner__initialize','vulkan_context_owner__close',
  'cubit_vulkan_context_create','cubit_vulkan_context_release','cubit_mesa_service_start','cubit_mesa_service_close']
 if args.device_startup_probe:required += ['vulkan_device_owner__start','vulkan_device_owner__close','cubit_vulkan_device_context_request']
 if args.prepare_pipeline:required += ['desktop_vulkan_startup__prepare_pipeline','cubit_vulkan_device_pipeline_create','cubit_vulkan_device_pipeline_close']
 if args.prepare_targets:required += ['desktop_vulkan_startup__configure_targets','cubit_vulkan_device_targets_prepare','cubit_vulkan_owned_image_bind']
 if args.admitted_startup:required += ['desktop_vulkan_startup__begin_write','desktop_vulkan_startup__submit_write','desktop_vulkan_startup__poll_upload','desktop_vulkan_startup__import_backing','cubit_vulkan_upload_record','desktop_vulkan_startup__configure_upload','desktop_vulkan_startup__release_upload','cubit_vulkan_device_upload_prepare','cubit_vulkan_upload_bind','desktop_vulkan_startup__initialize','desktop_vulkan_startup__stop','vulkan_device_owner__check_health']
 assert all(name in symbols for name in required),[name for name in required if name not in symbols]
 bundle.verify(args.bundle.resolve())
 for rel,h in inputs.items():assert hashlib.sha256((root/rel).read_bytes()).hexdigest()==h,rel
 result.update(status='LINKED',binary_sha256=hashlib.sha256(exe.read_bytes()).hexdigest(),binary_bytes=exe.stat().st_size,required_symbols=required)
 print(exe,flush=True)
finally:(out/'result.json').write_text(json.dumps(result,indent=2)+'\n')
