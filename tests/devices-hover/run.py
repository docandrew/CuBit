"""Native Devices scroll/hover regression; Nix and shared build lock required."""
from pathlib import Path
import hashlib
import json
import re
import shutil
import socket
import subprocess
import tempfile
import time

root=Path(__file__).resolve().parents[2]
d=Path(tempfile.mkdtemp(prefix='cubit-devices-hover-'))
print('ARTIFACTS:',d,flush=True)
def run(*args):
    return subprocess.run(args,cwd=root,check=True,capture_output=True,text=True)
def digest(p):
    with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
stage=root/'kernel/isodir/boot'
app=root/'userspace/apps/devices/build/devices.app'
profile=root/'tests/headless/init-devices.ccl'
services=('logstore.svc','clock.svc','config-storage.svc','tls.svc',
          'display.svc','metrics.svc','desktop.svc')
inputs=[stage/'cubit_kernel',stage/'initrd.img',app,profile]
service_paths=[stage/name for name in services]
inputs += service_paths
inputs += list((root/'userspace/apps/devices').glob('*.ad?'))
inputs += list((root/'userspace/lib/ui').glob('*.ad?'))
hashes={str(p):digest(p) for p in inputs}
(d/'inputs.json').write_text(json.dumps(hashes,indent=2))
run('python3','tools/build_development_disk.py',str(d/'base.img'),
    '--boot',*[str(p) for p in service_paths],str(app),
    '--file',f'init.ccl={profile}',
    '--file',f'tls/roots.der={root}/kernel/build/tls-roots.der')
base_hash=digest(d/'base.img')
shutil.copyfile(d/'base.img',d/'desktop.img')
iso=d/'iso';(iso/'boot/grub').mkdir(parents=True)
for name in ('cubit_kernel','initrd.img'):shutil.copyfile(stage/name,iso/'boot'/name)
(iso/'boot/grub/grub.cfg').write_text('''serial --speed=115200 --unit=0
terminal_output serial
set timeout=0
set default=0
menuentry "CuBit Devices hover regression" {
 multiboot /boot/cubit_kernel
 set gfxpayload=1024x768x32
 module /boot/initrd.img init.img
}
''')
run('grub-mkrescue','-o',str(d/'boot.iso'),str(iso))
serial=d/'serial.log';monitor=d/'monitor.sock'
args=['qemu-system-x86_64','-accel','tcg,thread=multi','-machine','q35',
      '-cpu','Broadwell','-smp','4','-m','2G','-cdrom',str(d/'boot.iso'),
      '-display','none','-serial',f'file:{serial}','-monitor',f'unix:{monitor},server,nowait',
      '-vga','none','-device','virtio-vga,xres=1024,yres=768',
      '-drive',f'file={d}/desktop.img,if=none,id=nvme0,format=raw',
      '-device','nvme,serial=cubitnvme,drive=nvme0',
      '-netdev','user,id=net0','-device','virtio-net-pci,netdev=net0',
      '-audiodev','none,id=snd0','-device','intel-hda',
      '-device','hda-output,audiodev=snd0','-no-reboot']
def text():return serial.read_text(errors='replace') if serial.exists() else ''
def wait_for(predicate,seconds=45):
    end=time.monotonic()+seconds
    while time.monotonic()<end:
        if vm.poll() is not None:raise RuntimeError('QEMU exited before check')
        if predicate():return
        time.sleep(.1)
    raise TimeoutError('normal session condition timed out')
def command(s):
    with socket.socket(socket.AF_UNIX,socket.SOCK_STREAM) as sock:
        sock.settimeout(3);sock.connect(str(monitor));sock.recv(65536)
        sock.sendall((s+'\n').encode());time.sleep(.1);sock.recv(65536)
def screenshot(name):
    p=d/(name+'.ppm');command('screendump '+str(p));wait_for(p.exists,5);return p

def pixels(p, left, top, right, bottom):
    data=p.read_bytes();m=re.match(rb'P6\s+(\d+)\s+(\d+)\s+255\s',data)
    assert m and (int(m[1]),int(m[2]))==(1024,768)
    payload=data[m.end():];assert len(payload)==1024*768*3
    return b''.join(payload[(y*1024+left)*3:(y*1024+right)*3] for y in range(top,bottom))

args += sum((['-device','virtio-rng-pci'] for _ in range(16)), [])

pointer=[80,80]
def move(x,y):
    while pointer!=[x,y]:
        dx=max(-60,min(60,x-pointer[0]));dy=max(-60,min(60,y-pointer[1]))
        command(f'mouse_move {dx} {dy}')
        pointer[0]+=dx;pointer[1]+=dy
        time.sleep(.05)
def click(x,y):
    move(x,y);command('mouse_button 1');time.sleep(.15);command('mouse_button 0')
def tree(p):return pixels(p,120,192,422,625)
with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
    try:
        wait_for(lambda:'devices: native window ready' in text(),60)
        time.sleep(3)
        initial=tree(screenshot('initial'))
        previous=initial
        for cycle in range(6):
            click(431,637 if cycle<3 else 200)
            time.sleep(.4)
            before=tree(screenshot(f'scroll-{cycle}'))
            assert before!=previous,'scroll did not update visible rows immediately'
            previous=before
            # Cross different rows, then leave: all hover styling is gone.
            # The entire tree must still equal the frame drawn on scrolling.
            move(300,253+cycle*24);time.sleep(.3)
            screenshot(f'hover-{cycle}')
            move(480,140);time.sleep(.4)
            after=tree(screenshot(f'leave-{cycle}'))
            assert after==before,f'row order/pixels changed after hover, cycle {cycle}'
        assert previous==initial,'three down/up steps did not restore the original rows'
        screenshot('final')
        assert not re.search(r'panic|assert|EXCEPTION:|TEST: FAIL|failed launch',text(),re.I),'guest fault'
        assert digest(d/'base.img')==base_hash,'base changed'
        assert all(digest(Path(p))==value for p,value in hashes.items()),'build inputs changed'
        (d/'result.json').write_text(json.dumps({'pass':True,'scroll_hover_cycles':6},indent=2))
        print('PASS: native Devices scroll immediately updates rows; six hover/leave cycles preserve all tree pixels',flush=True)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:vm.wait(timeout=5)
            except subprocess.TimeoutExpired:vm.kill();vm.wait()
