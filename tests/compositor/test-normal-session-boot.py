"""Boot the exact normal desktop startup profile using disposable artifacts.

Run in Nix under coordination/build.lock after make desktop-metrics.
Existing staged kernel/initrd and services are recorded as build inputs.
"""
from pathlib import Path
import argparse
import hashlib
import json
import re
import shutil
import socket
import subprocess
import tempfile
import time

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--backend', choices=('legacy','mesa'), default='legacy')
parser.add_argument('--mesa-fault', choices=('none','text'), default='none')
options=parser.parse_args()
if options.mesa_fault!='none' and options.backend!='mesa':
    parser.error('--mesa-fault requires --backend mesa')
root=Path(__file__).resolve().parents[2]
d=Path(tempfile.mkdtemp(prefix='cubit-normal-session-'))
print('ARTIFACTS:',d,flush=True)
def run(*args):
    return subprocess.run(args,cwd=root,check=True,capture_output=True,text=True)
def digest(p):
    with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
stage=root/'kernel/isodir/boot'
inputs=[root/'kernel/Makefile',root/'tests/headless/init-desktop-session.ccl',
        root/'tools/build_desktop_metrics.sh',stage/'desktop.svc',stage/'metrics.svc',
        stage/'cubit_kernel',stage/'initrd.img',stage/'clock.svc',stage/'logstore.svc',
        stage/'display.svc',stage/'tls.svc',stage/'config-storage.svc']
hashes={str(p):digest(p) for p in inputs}
variant='build-mesa-metrics' if options.backend=='mesa' else 'build-metrics'
expected=(root/'tests/compositor/build/desktop-mesa-text-metrics.svc'
          if options.mesa_fault=='text' else root/'userspace/services/desktop'/variant/'desktop.svc')
assert digest(stage/'desktop.svc')==digest(expected)
(d/'inputs.json').write_text(json.dumps(hashes,indent=2))
# Seed only the normal profile's base-disk dependencies, using the actual
# development-disk builder. The normal overlay supplies the other services.
run('python3','tools/build_development_disk.py',str(d/'base.img'),
    '--boot',str(stage/'clock.svc'),str(stage/'logstore.svc'),
    '--file',f'tls/roots.der={root}/kernel/build/tls-roots.der')
base_hash=digest(d/'base.img')
run('make','-C','kernel','prepare-desktop-disk',
    f'DESKTOP_BASE_DISK={d}/base.img',f'DESKTOP_SCRATCH_DISK={d}/desktop.img')
iso=d/'iso';(iso/'boot/grub').mkdir(parents=True)
for name in ('cubit_kernel','initrd.img'):shutil.copyfile(stage/name,iso/'boot'/name)
(iso/'boot/grub/grub.cfg').write_text('''serial --speed=115200 --unit=0
terminal_output serial
set timeout=0
set default=0
menuentry "CuBit normal desktop metrics" {
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

def region(p):
    data=p.read_bytes();m=re.match(rb'P6\s+(\d+)\s+(\d+)\s+255\s',data)
    assert m and (int(m[1]),int(m[2]))==(1024,768)
    pixels=data[m.end():];assert len(pixels)==1024*768*3
    return b''.join(pixels[(y*1024)*3:(y*1024+400)*3] for y in range(100,730))

with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
    try:
        wait_for(lambda:'desktop: asynchronous frame released' in text() and monitor.exists())
        time.sleep(1)
        baseline=region(screenshot('baseline'))
        for cycle in range(3):
            command('sendkey meta_l')
            # TCG software rendering is not a wall-clock performance oracle.
            # Require the actual pixel transition, within a bounded deadline.
            wait_for(lambda:sum(a!=b for a,b in zip(
                baseline,region(screenshot(f'open-{cycle}'))))>1000,30)
            command('sendkey esc')
            wait_for(lambda:region(screenshot(f'closed-{cycle}'))==baseline,30)
        time.sleep(3)
        contents=text()
        for marker in ('clock: registered','metricsvc: typed metrics ready',
                       'desktop: internal shell active','desktop: asynchronous frame released'):
            assert marker in contents,marker
        if options.mesa_fault=='text':
            assert contents.count('desktop: text batch failed; repainting scene in software')==1,'partial-write fault not exercised exactly once'
            assert 'desktop: retained software text active' in contents,'no retained software recovery'
            assert 'completion uncertain' not in contents and 'restarting' not in contents,'recovery restarted Desktop'
        elif options.backend=='mesa':
            assert 'desktop: Mesa retained-mask text active' in contents,'Mesa text path was not exercised'
            assert not re.search(r'software rendering|Mesa unavailable|CPU text fallback|text batch failed|retained software text',contents),'Mesa silently fell back'
        for service in ('logstore.svc','clock.svc','config-storage.svc','tls.svc','display.svc','metrics.svc','desktop.svc'):
            assert 'procmgr: init launched: '+service in contents,service
        assert re.search(r'desktop: stats .*key=[1-9]\d*',contents),'no input statistics'
        assert not re.search(r'panic|assert|EXCEPTION:|TEST: FAIL|desktop: metrics quarantined|failed launch',contents,re.I),'guest fault'
        assert digest(d/'base.img')==base_hash,'base changed'
        assert all(digest(Path(p))==value for p,value in hashes.items()),'build inputs changed'
        (d/'result.json').write_text(json.dumps({'pass':True,'backend':options.backend,'mesa_fault':options.mesa_fault,'menu_cycles':3,'restored_region_pixels':400*630,'startup_services':7},indent=2))
        print('PASS normal desktop startup: 7 services, 3 keyboard/menu cycles, exact wallpaper restoration, no faults',flush=True)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:vm.wait(timeout=5)
            except subprocess.TimeoutExpired:vm.kill();vm.wait()
