"""Private native browser bookmark fixture. Run in Nix with build.lock held."""
from pathlib import Path
import hashlib
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
d = Path(tempfile.mkdtemp(prefix='cubit-browser-bookmarks-'))
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
            body = b'<title>Bookmark Test</title><link rel="icon" href="/favicon.png"><h1>Bookmark persistence test</h1>'
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
(iso/'boot/grub/grub.cfg').write_text('serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\nmenuentry "Browser bookmarks" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n module /boot/initrd.img init.img\n}\n')
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
def snapshots():
    valid=[]
    for slot in (0,1):
        dest=d/f'snapshot-{slot}.dat'
        subprocess.run(['debugfs','-R',f'dump /Bookmarks/browser-{slot}.dat {dest}',str(d/'desktop.img')],capture_output=True)
        if dest.exists():
            data=dest.read_bytes()
            assert data[:4]==b'CBS1' and data[24:28]==b'CBM2'
            length=struct.unpack('<I',data[12:16])[0]
            assert len(data)==24+length
            hash_value=0xcbf29ce484222325
            for byte in data[24:]:hash_value=((hash_value^byte)*0x100000001b3)&0xffffffffffffffff
            assert hash_value==struct.unpack('<Q',data[16:24])[0]
            valid.append((struct.unpack('<Q',data[4:12])[0],data[24:]))
    assert valid,'no persisted bookmark snapshot'
    return max(valid)
def bookmark(data):
    at=4; found=[]
    for _ in range(64):
        kind,parent,n,u=struct.unpack('>BBHH',data[at:at+6]);at+=6
        icon=data[at:at+1024];at+=1024
        name=data[at:at+n].decode();at+=n
        address=data[at:at+u].decode();at+=u
        if kind==2:found.append((name,address,icon))
    assert at==len(data) and len(found)==1
    return found[0]
try:
    for boot in range(2):
        serial=d/f'serial-{boot}.log';monitor=d/f'monitor-{boot}.sock'
        launch=list(args)
        launch[launch.index('-serial')+1]=f'file:{serial}'
        launch[launch.index('-monitor')+1]=f'unix:{monitor},server,nowait'
        with (d/f'qemu-{boot}.log').open('w') as log:
            vm=subprocess.Popen(launch,cwd=root,stdout=log,stderr=log)
            try:
                wait_for(lambda:'CUBITSHELL: loading page 0' in text(),90)
                time.sleep(8);assert_healthy();screenshot(f'browser-{boot}')
                key('ctrl-d');time.sleep(2);screenshot(f'editor-{boot}')
                if boot==1:
                    key('ctrl-a')
                    for c in 'savedpage':key(c)
                key('ret');time.sleep(2);assert_healthy();screenshot(f'saved-{boot}')
                key('esc')
                key('alt-h');key('a');time.sleep(1);screenshot(f'penny-about-{boot}')
                key('esc');key('ctrl-shift-w')
                wait_for(lambda:'CUBITSHELL: closed' in text(),30);assert_healthy()
            finally:stop()
        generation,data=snapshots();name,address,icon=bookmark(data)
        assert generation==boot+1,(generation,boot)
        assert name==('Bookmark Test' if boot==0 else 'savedpage'),name
        assert address==url,address
        assert icon==bytes.fromhex('ff2080c0')*256,'favicon was not persisted'
    print('PASS: native Ctrl+D, save, cold reboot, edit/update, favicon bytes, close and fault scan',flush=True)
    (d/'result.txt').write_text('PASS two native boots; bookmark and favicon survived; update generation=2')
finally:
    server.shutdown()
