"""Native Observatory interaction test; Nix and shared build lock required."""
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

parser=argparse.ArgumentParser()
parser.add_argument('--fault',choices=('none','envelope','row','stall','full'),default='none')
parser.add_argument('--display',type=Path,help='private Display binary override')
parser.add_argument('--desktop',type=Path,help='private Desktop binary override; staged image unchanged')
parser.add_argument('--load',action='store_true',help='four same-priority CPU workers')
options=parser.parse_args()
if options.load and options.fault not in ('none','full'):parser.error('--load needs a normal collector')
normal=options.fault in ('none','full')
root=Path(__file__).resolve().parents[2]
d=Path(tempfile.mkdtemp(prefix='cubit-observatory-viewer-'))
print('ARTIFACTS:',d,flush=True)
def run(*args):
    return subprocess.run(args,cwd=root,check=True,capture_output=True,text=True)
def digest(p):
    with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
stage=root/'kernel/isodir/boot'
app=root/'userspace/apps/observatory/build/observatory.app'
profile=root/'tests/observatory-metrics/init-viewer.ccl'
services=('logstore.svc','clock.svc','config-storage.svc','tls.svc',
          'display.svc','metrics.svc','desktop.svc')
inputs=[stage/'cubit_kernel',stage/'initrd.img',app,profile]
service_paths=[(root/'tests/observatory-metrics/fault-service/build'/options.fault/'metrics.svc'
                if name=='metrics.svc' and options.fault!='none' else options.desktop.resolve() if name=='desktop.svc' and options.desktop else options.display.resolve() if name=='display.svc' and options.display else stage/name) for name in services]
if options.load:
    service_paths.append(root/'tests/observatory-metrics/load/build/observatory-load.app')
    inputs.append(root/'tests/observatory-metrics/load/main.adb')
    load_profile=d/'init-load.ccl'
    original=profile.read_text();end=original.rfind(')')
    load_profile.write_text(original[:end]+('  (start "observatory-load.app" (priority 3))\n'*4)+original[end:])
    profile=load_profile
    inputs.append(profile)
inputs += service_paths
inputs += list((root/'userspace/apps/observatory').glob('*.ad?'))
inputs += list((root/'userspace/lib/observatory').glob('*.ad?'))
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
menuentry "CuBit Observatory viewer" {
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
def region(p):return pixels(p,0,100,400,730)
def status(p):return pixels(p,114,610,846,642)
def visible_warning(data):
    return sum(r>g+30 and r>b+30 for r,g,b in zip(data[0::3],data[1::3],data[2::3]))>100
def visible_bars(data,color):
    # This disposable image uses the default Alloy palette. Only actual bar
    # color counts, never plot padding, borders, labels or background.
    return sum(pixel==color for pixel in zip(data[0::3],data[1::3],data[2::3]))>100

with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
    try:
        wait_for(lambda:'observatory: native window ready' in text(),60)
        def refreshes():
            return len(re.findall(r'observatory: page ready rows= *[1-9]',text()))
        wait_for(lambda:refreshes()>=3,45)
        if options.load:
            wait_for(lambda:text().count('TEST: viewer-load begin pid=')==4,30)
            assert 'TEST: viewer-load done' not in text(),'workers finished before input test'
        time.sleep(1)
        live=status(screenshot('live'))
        if normal:
            command('sendkey spc')
            wait_for(lambda:'observatory: paused=TRUE' in text())
            # Permit one query already in flight to finish; no new work while paused.
            time.sleep(2)
            paused_count=refreshes()
            paused=region(screenshot('paused'))
            paused_status=status(d/'paused.ppm')
            assert paused_status != live,'paused status was not visibly published'
            time.sleep(3)
            assert refreshes()==paused_count,'refresh continued while paused'
            assert region(screenshot('paused-stable'))==paused,'paused content changed'
            command('sendkey spc')
            wait_for(lambda:'observatory: paused=FALSE' in text())
            wait_for(lambda:refreshes()>=paused_count+2,30)
            for cycle in range(5 if options.load else 19):
                wait_for(lambda:status(screenshot(f'resumed-{cycle}'))==live,10)
                command('sendkey spc')
                wait_for(lambda:status(screenshot(f'pause-{cycle}'))==paused_status,10)
                time.sleep(.15 + cycle * .07)
                command('sendkey spc')
            wait_for(lambda:status(screenshot('resumed-final'))==live,10)
            command('sendkey n')
            before=refreshes()
            wait_for(lambda:refreshes()>before,30)
            command('sendkey r')
            before=refreshes()
            wait_for(lambda:refreshes()>before,30)
            # Keep querying long enough to exercise repeated grant and frame reuse.
            before=refreshes()
            wait_for(lambda:refreshes()>=before+10,45)
            running_image=screenshot('running')
            # Two non-background bar colors in native graph rectangles.
            graph=pixels(running_image,205,280,837,376)
            activity=pixels(running_image,205,445,837,541)
            assert visible_bars(graph,(46,113,128)),'no latency bars'
            assert visible_bars(activity,(57,121,90)),'no activity bars'
            command('sendkey spc')
            before_pause=refreshes()
            wait_for(lambda:status(screenshot('graphs-paused'))==paused_status,10)
            time.sleep(1)
            graph_region=pixels(screenshot('graphs'),110,250,848,570)
            command('sendkey t')
            wait_for(lambda:pixels(screenshot('table'),110,250,848,570)!=graph_region,10)
            command('sendkey t')
            wait_for(lambda:pixels(screenshot('graphs-restored'),110,250,848,570)==graph_region,10)
            command('sendkey esc')
            wait_for(lambda:'observatory: closed' in text())
            time.sleep(2)
            screenshot('closed')
        else:
            wait_for(lambda:'TEST: viewer fault injected '+options.fault in text(),30)
            wait_for(lambda:'observatory: collection unavailable' in text(),15)
            wait_for(lambda:status(screenshot('unavailable'))!=live,15)
            unavailable_status=status(d/'unavailable.ppm')
            assert visible_warning(unavailable_status),'unavailable status lacks warning color'
            retained=refreshes()
            # Input must still be handled while the query is failed/retiring.
            command('sendkey spc')
            wait_for(lambda:'observatory: paused=TRUE' in text(),10)
            command('sendkey r')
            wait_for(lambda:'TEST: viewer fault reply '+options.fault in text(),15)
            time.sleep(3)
            assert refreshes()==retained,'failed view revived on late data/input'
            assert status(screenshot('stale'))==unavailable_status,'stale status changed after late reply/input'
            command('sendkey esc')
            wait_for(lambda:'observatory: closed' in text(),10)
            screenshot('closed')
        if options.load:
            # Close must occur while all four workers are still active.
            assert 'TEST: viewer-load done' not in text(),'interaction outlasted worker overlap'
            wait_for(lambda:text().count('TEST: viewer-load done pid=')==4,180)
        contents=text()
        if options.load:
            begins=re.findall(r'TEST: viewer-load begin pid= *(\d+)',contents)
            ends=re.findall(r'TEST: viewer-load done pid= *(\d+) chunks= *(\d+)',contents)
            assert len(set(begins))==4 and sorted(begins)==sorted(pid for pid,_ in ends)
            assert all(int(chunks)>0 for _,chunks in ends)
            first_end=contents.index('TEST: viewer-load done')
            last_begin=contents.rindex('TEST: viewer-load begin')
            overlap=contents[last_begin:first_end]
            assert overlap.count('observatory: paused=TRUE')>=6
            assert overlap.count('observatory: page ready rows=')>=10
            assert 'observatory: closed' in overlap
        if options.fault=='full':
            assert 'TEST: full summary page rows=16' in contents
            assert 'observatory: page ready rows= 16' in contents
        assert not re.search(r'panic|assert|EXCEPTION:|TEST: FAIL|failed launch',contents,re.I),'guest fault'
        assert not normal or 'collection unavailable' not in contents
        assert digest(d/'base.img')==base_hash,'base changed'
        assert all(digest(Path(p))==value for p,value in hashes.items()),'build inputs changed'
        (d/'result.json').write_text(json.dumps({'pass':True,'load_workers':4 if options.load else 0,'fault':options.fault,'refreshes':refreshes(),
            'graphs_and_table':normal,'normal_interactions':normal,'visible_pause_cycles':(6 if options.load else 20) if normal else 0,'failure_input_and_close':not normal,'close':True},indent=2))
        print('PASS Observatory native viewer:',options.fault,'refreshes=',refreshes(),flush=True)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:vm.wait(timeout=5)
            except subprocess.TimeoutExpired:vm.kill();vm.wait()
