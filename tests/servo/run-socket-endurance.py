"""Native std/libc socket stress before Servo initialization. Nix/build.lock required.

Tests128 sequential and256 concurrent connection lifetimes,32 held channels,
excess refusal, idle reuse and recovery. Uses a loopback-only host echo peer.
"""
from pathlib import Path
import http.server
import hashlib
import os
import re
import shutil
import socket
import struct
import subprocess
import tempfile
import threading
import time
import zlib

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix='penny-sockets-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)
import socketserver
class Peer(socketserver.BaseRequestHandler):
    def handle(self):
        try:
            while data:=self.request.recv(8192):self.request.sendall(data)
        except (BrokenPipeError,ConnectionResetError):pass
class Server(socketserver.ThreadingTCPServer):
    daemon_threads=True
server=Server(('127.0.0.1',0),Peer)
threading.Thread(target=server.serve_forever,daemon=True).start()
(d/'socket-check').write_text(f'10.0.2.2:{server.server_address[1]}')
url='about:blank'
(d / 'pages').write_text(url + '\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = root / 'kernel/isodir/boot'
app = root / 'userspace/rust/build/cubitshell.app'
(d/'inputs.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p)+'\n' for p in [app, *[stage/n for n in ('cubit_kernel','initrd.img','desktop.svc','display.svc')]]))
fonts = Path(os.environ['IBM_PLEX_SANS_FONT']).parent
extra = ['--file', f'servo/pages={d}/pages', '--file', f'Bookmarks/keep={d}/empty','--file',f'servo/perf-check={d}/empty','--file',f'servo/socket-check={d}/socket-check']
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
        '-object',f'filter-dump,id=perf,netdev=net0,file={d}/network.pcap','-netdev', 'user,id=net0', '-device', 'virtio-net-pci,netdev=net0', '-no-reboot']
def text(): return serial.read_text(errors='replace') if serial.exists() else ''
def wait_for(predicate, seconds=45):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        if vm.poll() is not None: raise RuntimeError('QEMU exited')
        assert_healthy()
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
    wait_for(lambda:'CUBITSHELL-SOCKETS: PASS complete' in text() or 'CUBITSHELL-SOCKETS: FAIL' in text(),180)
    print('\n'.join(line for line in text().splitlines() if 'CUBITSHELL-SOCKETS:' in line or 'CUBITSHELL-MEMORY:' in line),flush=True)
    assert 'CUBITSHELL-MEMORY: PASS charge/release 2097152 bytes' in text()
    assert 'CUBITSHELL-SOCKETS: PASS complete' in text(), 'direct socket regression failed; see serial.log/network.pcap'
    assert_healthy()
finally:
    stop();server.shutdown()
