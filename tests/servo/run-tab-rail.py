"""Native tab-rail drag, limits, cancellation, Config reuse and screenshots. Nix/build.lock required."""
from pathlib import Path
import http.server
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
d = Path(tempfile.mkdtemp(prefix='cubit-browser-tab-rail-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)
class Page(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/favicon.png':
            raw = b''.join(b'\0' + b'\x20\x80\xc0\xff' * 16 for _ in range(16))
            def chunk(kind, data):
                return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data) & 0xffffffff)
            body = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', 16, 16, 8, 6, 0, 0, 0)) + chunk(b'IDAT', zlib.compress(raw)) + chunk(b'IEND', b'')
            mime = 'image/png'
        else:
            body = b'<title>Tab rail test</title><link rel="icon" href="/favicon.png"><h1>Resizable vertical tabs</h1>'
            mime = 'text/html'
        self.send_response(200)
        self.send_header('Content-Type', mime)
        self.end_headers()
        self.wfile.write(body)
    def log_message(self, *args): pass
server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Page)
threading.Thread(target=server.serve_forever, daemon=True).start()
url = f'http://10.0.2.2:{server.server_port}/'
(d / 'pages').write_text(url + '\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = root / 'kernel/isodir/boot'
app = root / 'userspace/rust/build/cubitshell.app'
fonts = Path(os.environ['IBM_PLEX_SANS_FONT']).parent
extra = ['--file', f'servo/pages={d}/pages', '--file', f'Bookmarks/keep={d}/empty']
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
pointer=[80,80]
def move_to(x,y):
    while pointer!=[x,y]:
        dx=max(-24,min(24,x-pointer[0]));dy=max(-24,min(24,y-pointer[1]))
        command(f'mouse_move {dx} {dy}')
        pointer[0]+=dx;pointer[1]+=dy;time.sleep(.08)
def click(x,y):
    move_to(x,y);command('mouse_button 1');time.sleep(.2);command('mouse_button 0');time.sleep(.8)
def drag(start,end,cancel=False):
    move_to(start,350);command('mouse_button 1');time.sleep(.3)
    move_to(end,350)
    if cancel:key('esc')
    command('mouse_button 0');time.sleep(1)
def check(width,name):
    assert_healthy()
    pixels=Image.open(screenshot(name)).convert('RGB')
    longest=run_length=0
    for x in range(1024):
        run_length=run_length+1 if pixels.getpixel((x,500))==(255,255,255) else 0
        longest=max(longest,run_length)
    assert longest==800-width,(name,longest,width)
with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
try:
    wait_for(lambda:'CUBITSHELL: loading page 0' in text(),90);time.sleep(8)
    key('ctrl-t');time.sleep(2);key('ctrl-t');time.sleep(2)
    key('alt-e');key('s');click(400,396);click(625,472)
    check(192,'rail-default')
    drag(291,355);check(256,'rail-wide')
    drag(355,435,True);check(256,'rail-cancel')
    drag(355,150);check(128,'rail-minimum')
    drag(227,800);check(400,'rail-maximum')
    drag(499,355);check(256,'rail-saved')
    key('ctrl-n');time.sleep(4);check(256,'rail-config-new-window')
    key('ctrl-shift-w');time.sleep(2)
    click(450,96);key('ctrl-shift-w')
    wait_for(lambda:'CUBITSHELL: closed' in text(),30);assert_healthy()
    print('PASS tab rail: drag, both limits, Escape cancel, Config width in new window, close/fault scan',flush=True)
finally:
    stop();server.shutdown()
