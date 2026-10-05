"""Run the existing sustained interaction oracle with staged native components.

Nix/build.lock required; builds a disposable disk/ISO, no shared service rebuild.
Set SERVO_BROWSER_FEATURES=1 for 20 tabs and four windows.
"""
from pathlib import Path
from performance_report import memory_report
import http.server
import hashlib
import os
import re
import shutil
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import time
import zlib

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix='penny-interaction-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)
import importlib.util
spec=importlib.util.spec_from_file_location('fixture',root/'tests/servo/http_server.py')
fixture=importlib.util.module_from_spec(spec);spec.loader.exec_module(fixture)
server=http.server.ThreadingHTTPServer(('127.0.0.1',18470),fixture.Handler)
server.daemon_threads=True
threading.Thread(target=server.serve_forever,daemon=True).start()
url='http://10.0.2.2:18470/servo-test.html'
(d / 'pages').write_text("data:text/html,<body style='background:%23fff'><h1>Native browser regression</h1></body>\n"+url+'\n'+url+'\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = root / 'kernel/isodir/boot'
app = root / 'userspace/rust/build/cubitshell.app'
(d/'inputs.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p)+'\n' for p in [app, *[stage/n for n in ('cubit_kernel','initrd.img','desktop.svc','display.svc')]]))
fonts = Path(os.environ['IBM_PLEX_SANS_FONT']).parent
extra = ['--file', f'servo/pages={d}/pages', '--file', f'Bookmarks/keep={d}/empty', '--file', f'servo/batch-test={d}/empty', '--file', f'servo/browser-check={d}/empty','--file',f'servo/perf-check={d}/empty']
for font in ('IBMPlexSans-Regular.ttf', 'IBMPlexSans-Bold.ttf', 'IBMPlexSerif-Regular.ttf', 'IBMPlexMono-Regular.ttf'):
    extra += ['--file', f'fonts/{font}={fonts/font}']
run('python3', 'tools/build_development_disk.py', str(d/'base.img'), '--boot',
    *[str(stage/name) for name in ('logstore.svc', 'clock.svc', 'display.svc', 'desktop.svc')], str(app),
    '--file', f'init.ccl={profile}', *extra)
shutil.copyfile(d/'base.img', d/'desktop.img')
iso = d/'iso'
(iso/'boot/grub').mkdir(parents=True)
for name in ('cubit_kernel', 'initrd.img'): shutil.copyfile(stage/name, iso/'boot'/name)
(iso/'boot/grub/grub.cfg').write_text('serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\nmenuentry "Browser tab rail" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n module /boot/initrd.img init.img\n}\n')
run('grub-mkrescue', '-o', str(d/'boot.iso'), str(iso))
serial = d/'serial.log'
monitor = d/'monitor.sock'
args = ['qemu-system-x86_64', '-accel', 'tcg,thread=multi', '-machine', 'q35', '-cpu', 'Broadwell',
        '-smp', '4', '-m', '2G', '-cdrom', str(d/'boot.iso'), '-display', 'none', '-serial', f'file:{serial}',
        '-monitor', f'unix:{monitor},server,nowait', '-vga', 'none', '-device', 'virtio-vga,xres=1024,yres=768',
        '-drive', f'file={d}/desktop.img,if=none,id=nvme0,format=raw', '-device', 'nvme,serial=cubitnvme,drive=nvme0',
        '-netdev', 'user,id=net0', '-device', 'virtio-net-pci,netdev=net0', '-no-reboot']
def text(): return serial.read_text(errors='replace') if serial.exists() else ''
def wait_for(predicate, seconds=45):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        if vm.poll() is not None: raise RuntimeError('QEMU exited')
        if predicate(): return
        time.sleep(.1)
    raise TimeoutError('Browser fixture timed out')
def command(s):
    def prompt(sock):
        data = b''
        while not data.endswith(b'(qemu) '): data += sock.recv(65536)
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
        sock.settimeout(5)
        sock.connect(str(monitor))
        prompt(sock)
        sock.sendall((s+'\n').encode())
        prompt(sock)
def screenshot(name):
    p = d/(name+'.ppm')
    command('screendump '+str(p))
    return p
def key(k):
    command('sendkey '+k)
    time.sleep(.5)
def assert_healthy():
    assert not re.search(r'USER-MEMORY-FAULT|CUBITSHELL: panic|EXCEPTION:|TEST: FAIL', text()), 'native browser fault'
def stop():
    if vm.poll() is None:
        vm.terminate()
        try: vm.wait(timeout=5)
        except subprocess.TimeoutExpired: vm.kill(); vm.wait()

from PIL import Image
with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
try:
    child=subprocess.run(['python3',str(root/'tests/servo/browser_input.py'),str(monitor),str(serial),str(d/'interaction.ppm')],cwd=root,timeout=600)
    assert_healthy()
    child.check_returncode()
    subprocess.run(['python3',str(root/'tests/servo/check_stability.py'),str(d/'interaction.timeline.jsonl')],check=True)
    if os.environ.get('SERVO_BROWSER_FEATURES')=='1':
        subprocess.run(['python3',str(root/'tests/servo/check_features.py'),str(d/'interaction.timeline.jsonl')],check=True)
finally:
    # Preserve diagnostics on assertion failures and subprocess timeouts too.
    # Artifact failures must not replace the original regression exception.
    stop()
    server.shutdown()
    failures = []
    for p in d.glob('*.ppm'):
        try:
            with Image.open(p) as frame:
                frame.save(p.with_suffix('.png'))
        except Exception as error:
            failures.append(f'{p.name}: {error}')
    try:
        memory_report(serial, d/'memory.json')
    except Exception as error:
        failures.append(f'memory report: {error}')
    if failures:
        (d/'artifact-errors.txt').write_text('\n'.join(failures) + '\n')
        print('ARTIFACT WARNINGS:', '; '.join(failures), flush=True)
        if sys.exc_info()[0] is None:
            raise RuntimeError('native artifact validation failed')
print('PASS native sustained interaction and features', flush=True)
