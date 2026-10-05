from pathlib import Path
import hashlib,json,os,shutil,socket,subprocess,tempfile,time
import argparse
parser=argparse.ArgumentParser(description="Native float-format regression in a disposable fixture")
for name in ('seed','app','desktop','kernel'):parser.add_argument('--'+name,type=Path,required=True)
parser.add_argument('--keep-images',action='store_true',help='Retain disposable VM images and copied binaries for debugging')
parser.add_argument('--accel',choices=('tcg','kvm'),default='tcg',help='Explicit QEMU accelerator; KVM requires host device access')
options=parser.parse_args()
import re
seed=options.seed.resolve()
d=Path(tempfile.mkdtemp(prefix='penny-float-'));print('ARTIFACTS:',d,flush=True)
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from native_artifacts import NativeArtifacts
artifacts=NativeArtifacts(d,options.keep_images)
shutil.copyfile(__file__,d/'runner.py')
profile=(seed/'init.ccl').read_text().rstrip();assert profile.endswith(')')
(d/'init.ccl').write_text(profile[:-1]+' (start "cubitshell.app" (priority 3) (network approve-declared))\n)\n')
subprocess.run(['cp','--reflink=auto','--sparse=always',str(seed/'desktop.img'),str(d/'base.img')],check=True)
with (d/'fsck.log').open('w') as log:
 result=subprocess.run(['e2fsck','-fy',str(d/'base.img')],stdout=log,stderr=subprocess.STDOUT)
 assert result.returncode in (0,1), 'private base repair failed'
app=str(options.app.resolve())
# Existing disposable snapshot has ample free space; avoid resizing it.
subprocess.run(['cp','--reflink=auto','--sparse=always',str(d/'base.img'),str(d/'desktop.img')],check=True)
(d/'base.img').unlink()
files=[('init.ccl',d/'init.ccl')]
if app:files.append(('cubitshell.app',Path(app)))
files.append(('desktop.svc',options.desktop.resolve()))
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
 subprocess.run(['xorriso','-indev',str(seed/'boot.iso'),'-outdev',str(d/'boot.iso'),'-boot_image','any','replay','-map',str(options.kernel.resolve()),'/boot/cubit_kernel','-commit'],stdout=log,stderr=log,check=True)
 subprocess.run(['xorriso','-osirrox','on','-indev',str(d/'boot.iso'),'-extract','/boot/cubit_kernel',str(d/'verified-kernel')],stdout=log,stderr=log,check=True)
 assert (d/'verified-kernel').read_bytes()==options.kernel.read_bytes()
 (d/'kernel.sha256').write_text(hashlib.sha256((d/'verified-kernel').read_bytes()).hexdigest()+'\n')
serial=d/'serial.log';monitor=d/'monitor.sock'
cmd=['qemu-system-x86_64','-accel',('kvm' if options.accel=='kvm' else 'tcg,thread=multi'),'-machine','q35','-cpu',('host' if options.accel=='kvm' else 'Broadwell'),'-smp','4','-m','2G','-cdrom',str(d/'boot.iso'),'-display','none','-serial','file:'+str(serial),'-monitor',f'unix:{monitor},server,nowait','-vga','none','-device','virtio-vga,xres=1024,yres=768','-drive',f'file={d}/desktop.img,if=none,id=nvme0,format=raw','-device','nvme,serial=cubitnvme,drive=nvme0','-netdev','user,id=net0','-device','virtio-net-pci,netdev=net0','-no-reboot']
(d/'execution.json').write_text(json.dumps({'accelerator':options.accel,'qemu_argv':cmd},indent=2))
with (d/'qemu.log').open('w') as log:vm=subprocess.Popen(cmd,stdout=log,stderr=log)
artifacts.vm=vm
try:
 deadline=time.monotonic()+90
 while time.monotonic()<deadline:
  output=serial.read_text(errors='replace') if serial.exists() else ''
  assert vm.poll() is None, 'VM exited'
  assert 'FLOAT-CHECK: FAIL' not in output and 'USER-MEMORY-FAULT' not in output, output[-2000:]
  if 'FLOAT-CHECK: PASS' in output:
   pid=re.findall(r'Process.Loader: Loaded module cubitshell.app w/ process ID (\d+)',output)
   if len(pid)==1 and ('Process.reclaimProcess: stopped PID '+pid[0]) in output:break
  time.sleep(.1)
 else:raise RuntimeError('float probe timeout: '+output[-2000:])
 (d/'float-result.json').write_text(json.dumps({'result':'PASS','accelerator':options.accel}))
 print('FLOAT_FORMAT_PASS', d, flush=True)
finally:
 vm.terminate()
 try:vm.wait(timeout=5)
 except subprocess.TimeoutExpired:vm.kill();vm.wait()
