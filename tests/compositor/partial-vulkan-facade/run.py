"""Real hosted Vulkan facade pixels; positive and forced-full-redraw negative control."""
from pathlib import Path
import argparse, hashlib, json, os, subprocess
HERE=Path(__file__).resolve().parent
p=argparse.ArgumentParser(); p.add_argument('--source-root',type=Path,required=True); p.add_argument('--support-root',type=Path,required=True);p.add_argument('--font-archive',type=Path,required=True);p.add_argument('--output',type=Path,required=True)
a=p.parse_args(); assert os.environ.get('IN_NIX_SHELL'), 'Use vulkan-affine-shell.nix'
source=a.source_root.resolve(); support=a.support_root.resolve(); out=a.output.resolve();out.mkdir(parents=True,exist_ok=False)
sha=lambda b:hashlib.sha256(b).hexdigest()
recipe=json.loads((HERE/'inputs.json').read_text()); inputs={}; payload={}
for item in recipe:
 origin=(HERE/item['fixture'] if 'fixture' in item else source/item['source'] if 'source' in item else support/item['support'])
 data=origin.read_bytes();inputs[str(origin)]=sha(data)
 if item['name']=='vulkan_context.h':data=data.replace(b'../../mesa/service-device.h',b'service-device.h')
 payload[item['name']]=data
font=a.font_archive.resolve(); inputs[str(font)]=sha(font.read_bytes())
for name in ['run.py','inputs.json','test.gpr.in']:inputs[str(HERE/name)]=sha((HERE/name).read_bytes())
results=[]
for mode in ['positive','forced-full','forced-copy','wrong-history','forced-transfer']:
 case=out/mode;case.mkdir()
 for name,data in payload.items():(case/name).write_bytes(data)
 if mode=='forced-full':
  file=case/'desktop_compositor.adb';s=file.read_text();anchor='      for I in 1 .. Compositor_Damage.Count (Repaint) loop'
  assert s.count(anchor)==1
  s=s.replace(anchor,'      D.Damage_Output ((0, 0, Natural (Screen.Width), Natural (Screen.Height)), OK);\n'+anchor)
  file.write_text(s)
 if mode=='forced-copy':
  file=case/'desktop_gpu_scene-output.adb';s=file.read_text()
  assert 'R.Begin_Region_Transfer (Copy,' in s
  s=s.replace('R.Begin_Region_Transfer (Copy,','R.Begin_Transfer (Copy,').replace('Natural (Target.Pitch), Repair, OK);','Natural (Target.Pitch), OK);')
  file.write_text(s)
 if mode=='wrong-history':
  file=case/'desktop_compositor.adb';s=file.read_text()
  assert s.count('Copy_Repair := Writer_Repair;')==1
  file.write_text(s.replace('Copy_Repair := Writer_Repair;','Copy_Repair := Repaint;'))
 if mode=='forced-transfer':
  file=case/'desktop_readback_output.adb';s=file.read_text()
  assert s.count('D.Submit_Region_Readback (S.Source, Repair, Accepted);')==1
  file.write_text(s.replace('D.Submit_Region_Readback (S.Source, Repair, Accepted);','D.Submit_Readback (S.Source, Accepted);'))
 (case/'test.gpr').write_text((HERE/'test.gpr.in').read_text().replace('@FONT@',str(font)))
 subprocess.run(['python3',str(support/'tests/compositor/build-vulkan-affine-shaders.py'),str(case/'generated')],check=True)
 env={**os.environ,'C_INCLUDE_PATH':str(case/'generated')+':'+os.environ.get('C_INCLUDE_PATH',''),'VK_DRIVER_FILES':os.environ['MESA_DRIVER_ROOT']+'/share/vulkan/icd.d/lvp_icd.x86_64.json','XDG_DATA_DIRS':os.environ['MESA_DRIVER_ROOT']+'/share'}
 with (case/'build.log').open('w') as log:subprocess.run(['alr','exec','--','gprbuild','-p','-P',str(case/'test.gpr')],cwd=support/'kernel',env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
 run=subprocess.run([str(case/'obj/desktop_gpu_scene_real_tests')],env=env,text=True,capture_output=True,timeout=120);output=run.stdout+run.stderr;(case/'run.log').write_text(output)
 if mode=='positive':assert run.returncode==0 and '98304 exact CPU output pixels' in output and 'accounted cleanup PASS' in output and 'validation errors=0' in output,output
 elif mode=='forced-full':assert run.returncode!=0 and 'frame=3 begin=13' in output,output
 elif mode=='forced-copy':assert run.returncode!=0 and 'frame=2 copy-state=4' in output,output
 elif mode=='wrong-history':assert run.returncode!=0 and 'frame=1 copy-state=4' in output,output
 else:assert run.returncode!=0 and 'frame=2 copy-state=5' in output,output
 results.append({'mode':mode,'exit_code':run.returncode,'oracle':'PASS' if mode=='positive' else 'expected sparse-area rejection' if mode=='forced-full' else 'expected excess copy-byte rejection' if mode=='forced-copy' else 'expected wrong repair-history rejection' if mode=='wrong-history' else 'expected excess GPU transfer-byte rejection'})
for path,digest in inputs.items():assert sha(Path(path).read_bytes())==digest,path
(out/'result.json').write_text(json.dumps({'status':'PASS','backend':'Linux-hosted Mesa llvmpipe','scope':'Real compositor facade, sparse repair, GPU readback, CPU copy and cleanup; fill-only. No native main/IPC/display hardware/performance claim.','cases':results,'inputs':inputs},indent=2)+'\n')
print(out/'result.json',flush=True)
