from pathlib import Path
import hashlib,json,os,shutil,socket,subprocess,tempfile,time
import argparse
parser=argparse.ArgumentParser(description="Native Penny cancellation regression; use a private build workspace")
for name in ('seed','app','desktop','kernel'):
 parser.add_argument('--'+name,type=Path,required=True)
parser.add_argument('--no-capture',action='store_true')
parser.add_argument('--iframe',action='store_true')
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
capture = not options.no_capture
iframe = options.iframe
html = """<meta charset=utf-8><style>body{margin:0;background:rgb(20,40,60)}button{position:absolute;left:40px;top:40px;width:400px;height:220px}</style><button>Pointer cancellation regression</button><script>
const b=document.querySelector('button');
let d=0,u=0,c=0,x=0,l=0,bad=0;
function report(){parent.postMessage('CuBitBrowserPointerD'+d+'U'+u+'C'+c+'X'+x+'L'+l+'B'+bad,'*');}
b.addEventListener('pointerdown',e=>{d++;CAPTURE;report();});
b.addEventListener('mouseup',()=>{u++;report();});
b.addEventListener('click',()=>{c++;report();});
b.addEventListener('pointercancel',e=>{x++;if(e.cancelable || e.buttons!==0 || !e.isTrusted)bad++;report();});
b.addEventListener('lostpointercapture',e=>{l++;report();setTimeout(()=>{if(b.hasPointerCapture(e.pointerId))bad++;report();},0);});
parent.postMessage('CuBitBrowserPointerReady','*');
</script>""".replace('CAPTURE', 'b.setPointerCapture(e.pointerId)' if capture else '')
listener = "<script>addEventListener('message',e=>{document.title=e.data})</script>"
if iframe:
 from html import escape
 html = listener + '<iframe style="position:absolute;left:0;top:0;width:100%;height:100%;border:0" srcdoc="'+escape(html,quote=True)+'"></iframe>'
else: html = listener + html
page="data:text/html,<body style='background:%23fff'>"+quote(html,safe='')
(d/'pages').write_text((page+'\n')*3)
shutil.copyfile(seed/'desktop.img',d/'base.img')
with (d/'fsck.log').open('w') as log:
 result=subprocess.run(['e2fsck','-fy',str(d/'base.img')],stdout=log,stderr=subprocess.STDOUT)
 assert result.returncode in (0,1), 'private base repair failed'
app=str(options.app.resolve())
# Existing disposable snapshot has ample free space; avoid resizing it.
shutil.copyfile(d/'base.img',d/'desktop.img')
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
  prefix = 'CUBITSHELL-BROWSER: title CuBitBrowserPointer'
  def state(down,up,clicked,cancelled,lost):
   wait(prefix+f'D{down}U{up}C{clicked}X{cancelled}L{lost}B0')
   assert prefix+'BAD' not in serial.read_text()
  wait('CUBITSHELL-BROWSER: window ready');wait(prefix+'Ready')
  move(260,300);time.sleep(.5);command('mouse_button 1')
  state(1,0,0,0,0);screenshot('pressed')
  command('sendkey alt-e 10');time.sleep(.5);command('sendkey s 10')
  wait('CUBITSHELL-BROWSER: settings opened')
  state(1,0,0,1,int(capture));screenshot('cancelled')
  command('mouse_button 0');command('sendkey esc 10');time.sleep(.7)
  # The real release belongs to the cancelled gesture, not to the page.
  assert prefix+'D1U1' not in serial.read_text()
  click(260,300);state(2,1,1,1,2*int(capture))
  click(260,300);state(3,2,2,1,3*int(capture))
  screenshot('ordinary-clicks-after-cancel')
  (d/'pointer-result.json').write_text(json.dumps({'result':'PASS','capture':capture,'iframe':iframe,'checks':['cancel without mouseup/click','capture released','no activation on physical release','two subsequent ordinary clicks work']}))
  print('PASS pointer cancellation',capture,iframe,d,flush=True)

finally:
 vm.terminate()
 try:vm.wait(timeout=5)
 except subprocess.TimeoutExpired:vm.kill();vm.wait()
