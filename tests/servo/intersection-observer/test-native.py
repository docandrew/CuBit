from pathlib import Path
import hashlib,json,os,shutil,socket,subprocess,tempfile,time
import argparse
parser=argparse.ArgumentParser(description="Native Penny IntersectionObserver regression; use a private build workspace")
for name in ('seed','app','desktop','kernel'):
 parser.add_argument('--'+name,type=Path,required=True)
parser.add_argument('--keep-images',action='store_true',help='Retain disposable VM images and copied binaries for debugging')
options=parser.parse_args()
seed=options.seed.resolve()
d=Path(tempfile.mkdtemp(prefix='penny-input-'));print('ARTIFACTS:',d,flush=True)
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from native_artifacts import NativeArtifacts
artifacts=NativeArtifacts(d,options.keep_images)
shutil.copyfile(__file__,d/'runner.py')
profile=(seed/'init.ccl').read_text().rstrip();assert profile.endswith(')')
(d/'init.ccl').write_text(profile[:-1]+' (start "cubitshell.app" (priority 3) (network approve-declared))\n)\n')
from urllib.parse import quote
html='<meta charset=utf-8><title>CuBitBrowserObserverReady</title><style>body{margin:0}#target{position:absolute;left:20px;top:20px;width:50px;height:50px;background:green}</style><div id=target></div><script>\n(async()=>{\n const target=document.getElementById(\'target\');let receive;\n const observer=new IntersectionObserver(entries=>{if(receive)receive(entries)}, {threshold:[0,.5,1]});\n async function next(action){return new Promise((resolve,reject)=>{const t=setTimeout(()=>reject(Error(\'callback-timeout\')),8000);receive=entries=>{clearTimeout(t);receive=null;resolve(entries[entries.length-1]);};action();});}\n const first=await next(()=>observer.observe(target));\n if(!first.isIntersecting || first.intersectionRatio!==1 || first.rootBounds===null)throw Error(\'initial-visible\');\n document.title=\'CuBitBrowserObserverInitial\';\n observer.disconnect();target.style.left=\'10000px\';\n const second=await next(()=>observer.observe(target));\n if(second.isIntersecting)throw Error(\'reobserve-outside\');\n document.title=\'CuBitBrowserObserverReobserved\';\n const third=await next(()=>{target.style.left=\'20px\'});\n if(!third.isIntersecting)throw Error(\'return-visible\');\n observer.unobserve(target);\n target.style.left=\'10000px\';\n const fourth=await next(()=>observer.observe(target));\n if(fourth.isIntersecting)throw Error(\'last-unobserve\');\n observer.disconnect();\n document.title=\'CuBitBrowserObserverUnobserved\';\n const root=document.createElement(\'div\');\n root.style.cssText=\'position:absolute;left:100px;top:100px;width:200px;height:100px;overflow:hidden\';\n const child=document.createElement(\'div\');child.style.cssText=\'width:20px;height:20px\';root.appendChild(child);document.body.appendChild(root);\n const margin=await new Promise((resolve,reject)=>{\n   const timer=setTimeout(()=>reject(Error(\'margin-timeout\')),8000);\n   const o=new IntersectionObserver(entries=>{clearTimeout(timer);o.disconnect();resolve(entries[0]);},{root,rootMargin:\'10%\'});o.observe(child);\n });\n if(!margin.rootBounds || Math.abs(margin.rootBounds.width-240)>.1 || Math.abs(margin.rootBounds.height-140)>.1)throw Error(\'percent-width-basis\');\n document.title=\'CuBitBrowserObserverMargins\';\n const frame=document.createElement(\'iframe\');frame.sandbox=\'allow-scripts\';\n frame.srcdoc=`<div id=t style="width:20px;height:20px">target</div><script>new IntersectionObserver(e=>parent.postMessage({observerPrivacy:true,nullBounds:e[0].rootBounds===null},\'*\'),{rootMargin:\'1000px\',scrollMargin:\'1000px\'}).observe(document.getElementById(\'t\'))<\\/script>`;\n await new Promise((resolve,reject)=>{\n   const timer=setTimeout(()=>reject(Error(\'privacy-timeout\')),8000);\n   const receive=e=>{if(e.source!==frame.contentWindow || !e.data.observerPrivacy)return;clearTimeout(timer);window.removeEventListener(\'message\',receive);if(e.data.nullBounds)resolve();else reject(Error(\'cross-origin-bounds\'));};\n   window.addEventListener(\'message\',receive);document.body.appendChild(frame);\n });\n document.title=\'CuBitBrowserObserverPass\';\n})().catch(e=>{document.title=\'CuBitBrowserObserverFail-\'+e.message;document.body.style.background=\'red\';});\n</script>'
page="data:text/html,<body style='background:%23fff'>"+quote(html,safe='')
(d/'pages').write_text((page+'\n')*3)
subprocess.run(['cp','--reflink=auto','--sparse=always',str(seed/'desktop.img'),str(d/'base.img')],check=True)
with (d/'fsck.log').open('w') as log:
 result=subprocess.run(['e2fsck','-fy',str(d/'base.img')],stdout=log,stderr=subprocess.STDOUT)
 assert result.returncode in (0,1), 'private base repair failed'
app=str(options.app.resolve())
# Existing disposable snapshot has ample free space; avoid resizing it.
subprocess.run(['cp','--reflink=auto','--sparse=always',str(d/'base.img'),str(d/'desktop.img')],check=True)
(d/'base.img').unlink()
files=[('init.ccl',d/'init.ccl'),('servo/pages',d/'pages')]
if app:files.append(('cubitshell.app',Path(app)))
files.append(('desktop.svc',options.desktop.resolve()))
(d/'browser-check').write_text('1\n')
files.append(('servo/browser-check',d/'browser-check'))
(d/'perf-check').write_text('1\n');files.append(('servo/perf-check',d/'perf-check'))
with (d/'overlay.log').open('w') as log:
 for index,(dest,source) in enumerate(files):
  staged=d/('payload-'+str(index));shutil.copyfile(source,staged)
  for command in ('rm /'+dest,'write '+staged.name+' /'+dest):
   subprocess.run(['debugfs','-w','-R',command,'desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
  dumped=d/('verified-'+str(index))
  subprocess.run(['debugfs','-R','dump /'+dest+' '+dumped.name,'desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
  assert dumped.read_bytes()==source.read_bytes()
 subprocess.run(['e2fsck','-fn','desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
with (d/'iso.log').open('w') as log:
 kernel=options.kernel.resolve()
 subprocess.run(['xorriso','-indev',str(seed/'boot.iso'),'-outdev',str(d/'boot.iso'),'-boot_image','any','replay','-map',str(kernel),'/boot/cubit_kernel','-commit'],stdout=log,stderr=log,check=True)
 subprocess.run(['xorriso','-osirrox','on','-indev',str(d/'boot.iso'),'-extract','/boot/cubit_kernel',str(d/'verified-kernel')],stdout=log,stderr=log,check=True)
 assert (d/'verified-kernel').read_bytes()==kernel.read_bytes()
 (d/'kernel.sha256').write_text(hashlib.sha256(kernel.read_bytes()).hexdigest()+'\n')
serial=d/'serial.log';monitor=d/'monitor.sock'
cmd=['qemu-system-x86_64','-accel','tcg,thread=multi','-machine','q35','-cpu','Broadwell','-smp','4','-m','2G','-cdrom',str(d/'boot.iso'),'-display','none','-serial','file:'+str(serial),'-monitor',f'unix:{monitor},server,nowait','-vga','none','-device','virtio-vga,xres=1024,yres=768','-drive',f'file={d}/desktop.img,if=none,id=nvme0,format=raw','-device','nvme,serial=cubitnvme,drive=nvme0','-netdev','user,id=net0','-device','virtio-net-pci,netdev=net0','-no-reboot']
with (d/'qemu.log').open('w') as log:vm=subprocess.Popen(cmd,stdout=log,stderr=log)
artifacts.vm=vm
try:
 deadline=time.monotonic()+150
 while time.monotonic()<deadline:
  text=serial.read_text(errors='replace') if serial.exists() else ''
  assert vm.poll() is None,'VM exited'
  if 'CUBITSHELL: FAIL' in text:
   result='FIXTURE-FAIL';break
  if 'USER-MEMORY-FAULT' in text:
   result='CRASH';break
  if 'CUBITSHELL-BROWSER: window ready' in text:
   result='PASS';break
  time.sleep(.2)
 else:result='TIMEOUT'
 (d/'result.json').write_text(json.dumps({'result':result,'candidate':app,'sha256':hashlib.sha256(Path(app).read_bytes()).hexdigest() if app else 'saved staged 00de8bae1b8c70dc86527a431d5178b74aa0dfc700e918c1d69d816e065efca4'}))
 print(result,serial,flush=True)
 assert result == 'PASS', result
 if result=='PASS' and app:
  def command(value):
   with socket.socket(socket.AF_UNIX) as sock:
    sock.settimeout(5);sock.connect(str(monitor))
    def prompt():
     data=b''
     while not data.endswith(b'(qemu) '):data+=sock.recv(65536)
    prompt();sock.sendall((value+'\n').encode());prompt()
  def wait(marker,count=0):
   deadline=time.monotonic()+30
   while time.monotonic()<deadline:
    text=serial.read_text(errors='replace')
    assert 'USER-MEMORY-FAULT' not in text and 'CUBITSHELL: panic' not in text,text[-1500:]
    if text.count(marker)>count:return
    time.sleep(.1)
   screenshot('failure');raise RuntimeError('missing '+marker)
  recoveries=[]
  cursor=[80,80]
  def move(x,y):
   while cursor!=[x,y]:
    dx=max(-60,min(60,x-cursor[0]));dy=max(-60,min(60,y-cursor[1]))
    command(f'mouse_move {dx} {dy}');cursor[0]+=dx;cursor[1]+=dy;time.sleep(.01)
  def click(x,y):
   move(x,y);command('mouse_button 1');time.sleep(.15);command('mouse_button 0');time.sleep(.3)
  def screenshot(name):
   command('screendump '+str(d/(name+'.ppm')))
   from PIL import Image
   Image.open(d/(name+'.ppm')).save(d/(name+'.png'))
  deadline=time.monotonic()+40
  while time.monotonic()<deadline:
   text=serial.read_text(errors='replace')
   assert not any(x in text for x in ['PENNY-ABORT:','USER-MEMORY-FAULT','CUBITSHELL: panic'])
   if 'CuBitBrowserObserverPass' in text or 'CuBitBrowserObserverFail-' in text:break
   time.sleep(.2)
  screenshot('observer-result')
  messages=[line for line in text.splitlines() if 'CUBITSHELL-BROWSER: title CuBitBrowserObserver' in line]
  passed=any('CuBitBrowserObserverPass' in line for line in messages)
  (d/'observer-result.json').write_text(json.dumps({'result':'PASS' if passed else 'FAIL','messages':messages}))
  print(json.dumps({'result':'PASS' if passed else 'FAIL','messages':messages,'artifacts':str(d)}),flush=True)
  assert passed,'Observer lifecycle failed'

finally:
 vm.terminate()
 try:vm.wait(timeout=5)
 except subprocess.TimeoutExpired:vm.kill();vm.wait()
