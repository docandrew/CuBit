from pathlib import Path
import hashlib,json,os,shutil,socket,subprocess,tempfile,time
import argparse
parser=argparse.ArgumentParser(description="Penny shutdown-cancellation regression in a disposable native fixture")
for name in ('seed','app','desktop','kernel'):parser.add_argument('--'+name,type=Path,required=True)
parser.add_argument('--directory',action='store_true',help='Create the Bookmarks output directory; otherwise seed must omit it')
parser.add_argument('--keep-images',action='store_true',help='Retain disposable VM images and copied binaries for debugging')
parser.add_argument('--host',help='Navigate to a DNS host before closing and collecting the profile')
parser.add_argument('--accel',choices=('tcg','kvm'),default='tcg',help='Explicit QEMU accelerator; KVM requires host device access')
parser.add_argument('--no-profile',action='store_true',help='Run the navigation and shutdown control with profiling disabled')
parser.add_argument("--click-page",action="store_true",help="Click the page after navigation, then verify close/reclaim")
parser.add_argument('--cpu',choices=('host','Broadwell'),help='Override accelerator CPU default; fast desktop uses Broadwell')
parser.add_argument('--hda',action='store_true',help='Expose the fast-desktop HDA device with a silent host audio sink')
parser.add_argument("--click-during-load",action="store_true")
parser.add_argument('--mode',choices=('window','worker'),default='window')
options=parser.parse_args()
import re
if options.host and not re.fullmatch(r'[a-z0-9.-]+',options.host):parser.error('--host must be a lowercase DNS host')
seed=options.seed.resolve()
d=Path(tempfile.mkdtemp(prefix='penny-input-'));print('ARTIFACTS:',d,flush=True)
import sys
sys.path.insert(0,str(Path(__file__).resolve().parent))
from native_artifacts import NativeArtifacts
artifacts=NativeArtifacts(d,options.keep_images)
shutil.copyfile(__file__,d/'runner.py')
profile=(seed/'init.ccl').read_text().rstrip();assert profile.endswith(')')
(d/'init.ccl').write_text(profile[:-1]+' (start "cubitshell.app" (priority 3) (network approve-declared))\n)\n')
from urllib.parse import quote
fixture=Path(__file__).resolve().parent/'shutdown-cancellation'/(options.mode+'.html')
page='data:text/html,'+quote(fixture.read_text().strip(),safe="<>='\" /:;(){}[],.?!-")
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
if not options.no_profile:files.append(('servo/profile-check',d/'perf-check'))
with (d/'overlay.log').open('w') as log:
 if options.no_profile:subprocess.run(['debugfs','-w','-R','rm /servo/profile-check',str(d/'desktop.img')],stdout=log,stderr=log,check=True)
 if options.directory:subprocess.run(['debugfs','-w','-R','mkdir /Bookmarks',str(d/'desktop.img')],stdout=log,stderr=log,check=True)
 for index,(dest,source) in enumerate(files):
  staged=d/('payload-'+str(index));shutil.copyfile(source,staged)
  for command in ('rm /'+dest,'write '+staged.name+' /'+dest):
   subprocess.run(['debugfs','-w','-R',command,'desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
  dumped=d/('verified-'+str(index))
  subprocess.run(['debugfs','-R','dump /'+dest+' '+dumped.name,'desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
  assert dumped.read_bytes()==source.read_bytes()
 subprocess.run(['e2fsck','-fn','desktop.img'],cwd=d,stdout=log,stderr=log,check=True)
with (d/'iso.log').open('w') as log:
 subprocess.run(['xorriso','-indev',str(seed/'boot.iso'),'-outdev',str(d/'boot.iso'),'-boot_image','any','replay','-map',str(options.kernel.resolve()),'/boot/cubit_kernel','-commit'],stdout=log,stderr=log,check=True)
 subprocess.run(['xorriso','-osirrox','on','-indev',str(d/'boot.iso'),'-extract','/boot/cubit_kernel',str(d/'verified-kernel')],stdout=log,stderr=log,check=True)
 assert (d/'verified-kernel').read_bytes()==options.kernel.read_bytes()
 (d/'kernel.sha256').write_text(hashlib.sha256((d/'verified-kernel').read_bytes()).hexdigest()+'\n')
serial=d/'serial.log';monitor=d/'monitor.sock'
cmd=['qemu-system-x86_64','-accel',('kvm' if options.accel=='kvm' else 'tcg,thread=multi'),'-machine','q35','-cpu',(options.cpu or ('host' if options.accel=='kvm' else 'Broadwell')),'-smp','4','-m','2G','-cdrom',str(d/'boot.iso'),'-display','none','-serial','file:'+str(serial),'-monitor',f'unix:{monitor},server,nowait','-vga','none','-device','virtio-vga,xres=1024,yres=768','-drive',f'file={d}/desktop.img,if=none,id=nvme0,format=raw','-device','nvme,serial=cubitnvme,drive=nvme0','-netdev','user,id=net0','-device','virtio-net-pci,netdev=net0','-no-reboot']
if options.hda:cmd += ['-audiodev','none,id=snd0','-device','intel-hda','-device','hda-output,audiodev=snd0']
(d/'execution.json').write_text(json.dumps({'accelerator':options.accel,'qemu_argv':cmd},indent=2))
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
 assert 'PENNY-DIAGNOSTIC: stderr capture ready' in text, 'stderr capture missing'
 if result=='PASS' and app:
  def command(value):
   with socket.socket(socket.AF_UNIX) as sock:
    sock.settimeout(5);sock.connect(str(monitor))
    def prompt():
     data=b''
     while not data.endswith(b'(qemu) '):data+=sock.recv(65536)
    prompt();sock.sendall((value+'\n').encode());prompt()
  def wait(marker,count=0,timeout=30):
   deadline=time.monotonic()+timeout
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
  wait('CUBITSHELL-BROWSER: window ready');time.sleep(1);screenshot('gradient')
  # One short motion immediately followed by release exercises the final packet.
  previous=serial.read_text().count('CUBITSHELL-BROWSER: viewport')
  move(900,714);command('mouse_button 1');time.sleep(.15)
  command('mouse_move 30 12');cursor[:]=[930,726]
  command('mouse_button 0')
  try:
   wait('CUBITSHELL-BROWSER: viewport',previous)
   outcome='PASS'
  except RuntimeError:
   outcome='NO_RESIZE'
  screenshot('resize-final')
  text=serial.read_text(errors='replace')
  evidence=[x for x in text.splitlines() if 'desktop: ptr ' in x or 'CUBITSHELL-BROWSER: viewport' in x]
  (d/'resize-result.json').write_text(json.dumps({'result':outcome,'evidence':evidence},indent=2))
  print(outcome,evidence,flush=True)
  assert outcome=='PASS','Final motion/release did not resize'
  if options.host:
   command('sendkey ctrl-l 10');time.sleep(.2)
   for key in options.host:
    command('sendkey '+({'-':'minus','.':'dot'}.get(key,key))+' 10');time.sleep(.1)
   count=serial.read_text().count('stage=Complete')
   command('sendkey ret 10')
   if options.click_during_load:click(624,230)
   wait('stage=Complete',count,90);time.sleep(5);screenshot('loaded')
   if options.click_page:
    click(624,230);time.sleep(2);screenshot('after-click')
  pid=re.findall(r'Process.Loader: Loaded module cubitshell.app w/ process ID (\d+)',serial.read_text())
  assert len(pid)==1,pid
  wait('CUBITSHELL-BROWSER: title CuBitBrowserSpinStarted',timeout=20)
  closed_at=time.monotonic()
  command('sendkey alt-f 10');time.sleep(.3);command('sendkey c 10');wait('CUBITSHELL-BROWSER: window closed')
  wait('Process.reclaimProcess: stopped PID '+pid[0],timeout=60)
  wait('netstack: released the scopes of exited process '+pid[0],timeout=10)
  (d/'shutdown-timing.json').write_text(json.dumps({'pid':int(pid[0]),'close_to_reclaim_host_seconds':time.monotonic()-closed_at,'host':options.host}))
  text=serial.read_text(errors='replace')
  assert not any(x in text for x in ('PENNY-ABORT:','CUBITSHELL: panic','USER-MEMORY-FAULT')),text[-2500:]
  assert 'CUBITSHELL: closed' in text
  marker = ('PENNY-SHUTDOWN: preserving uncatchable JavaScript cancellation' if options.mode=='window' else 'evaluate_script failed (terminated)')
  assert marker in text, 'shutdown cancellation path not exercised'
  if not options.directory and not options.no_profile:assert 'Could not write timing profile:' in text


finally:
 vm.terminate()
 try:vm.wait(timeout=5)
 except subprocess.TimeoutExpired:vm.kill();vm.wait()

if options.directory and not options.no_profile:
 with (d/'profile-extract.log').open('w') as log:
  subprocess.run(['debugfs','-R','dump /Bookmarks/penny-profile.tsv '+str(d/'penny-profile.tsv'),str(d/'desktop.img')],stdout=log,stderr=log,check=True)
 assert (d/'penny-profile.tsv').read_text().startswith('_category_')
(d/'profile-result.json').write_text(json.dumps({'result':'PASS','directory_present':options.directory,'profiling_enabled':not options.no_profile}))
print('PROFILE_SHUTDOWN_PASS',d,flush=True)
