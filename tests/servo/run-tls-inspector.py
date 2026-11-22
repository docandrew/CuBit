"""Native Penny TLS inspector and optional public-site smoke tests. Nix/build.lock required.

PENNY_PUBLIC_SITES is a whitespace-separated URL list; these are observational
compatibility checks, not deterministic assertions about external content.
"""
from pathlib import Path
import http.server
import ssl
import json
import hashlib
from urllib.parse import urlsplit
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
d = Path(tempfile.mkdtemp(prefix='cubit-browser-tls-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    result = subprocess.run(args, cwd=root, capture_output=True, text=True)
    if result.returncode:
        print(result.stdout, result.stderr, flush=True)
        result.check_returncode()
    return result
class Page(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/favicon.png':
            raw = b''.join(b'\0' + b'\x20\x80\xc0\xff' * 16 for _ in range(16))
            def chunk(kind, data):
                return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data) & 0xffffffff)
            body = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', 16, 16, 8, 6, 0, 0, 0)) + chunk(b'IDAT', zlib.compress(raw)) + chunk(b'IEND', b'')
            mime = 'image/png'
        else:
            body = b'<title>Penny TLS fixture</title><link rel="icon" href="/favicon.png"><h1>TLS inspector fixture</h1>'
            mime = 'text/html'
        self.send_response(200)
        self.send_header('Content-Type', mime)
        self.end_headers()
        self.wfile.write(body)
    def log_message(self, *args): pass
servers = []
ports = {}
for kind in ('good', 'wronghost', 'expired', 'untrusted'):
    srv = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Page)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    pki = root / 'tests/tls/build/pki'
    context.load_cert_chain(pki/(kind+'.pem'), pki/(kind+'.key'))
    srv.socket = context.wrap_socket(srv.socket, server_side=True)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    ports[kind] = srv.server_port
    servers.append(srv)
server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Page)
threading.Thread(target=server.serve_forever, daemon=True).start()
servers.append(server)
url = os.environ.get('PENNY_ONLY_SITE') or f'https://tls-test.cubit.internal:{ports["good"]}/'
(d/'roots.der').write_bytes(b''.join(ssl.create_default_context().get_ca_certs(binary_form=True)) + (pki/'ca.der').read_bytes())
(d/'hosts').write_text('10.0.2.2 tls-test.cubit.internal\n')
(d / 'pages').write_text(url + '\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = root / 'kernel/isodir/boot'
app = root / 'userspace/rust/build/cubitshell.app'
fonts = Path(os.environ['IBM_PLEX_SANS_FONT']).parent
extra = ['--file', f'servo/pages={d}/pages', '--file', f'Bookmarks/keep={d}/empty',
         '--file', f'servo/hosts={d}/hosts', '--file', f'tls/roots.der={d}/roots.der',
         '--file', f'servo/tls-check={d}/empty']
for font in ('IBMPlexSans-Regular.ttf', 'IBMPlexSans-Bold.ttf', 'IBMPlexSerif-Regular.ttf', 'IBMPlexMono-Regular.ttf'):
    extra += ['--file', f'fonts/{font}={fonts/font}']
run('python3', 'tools/build_development_disk.py', str(d/'base.img'), '--boot',
    *[str(stage/name) for name in ('logstore.svc', 'clock.svc', 'display.svc', 'desktop.svc')], str(app),
    '--file', f'init.ccl={profile}', *extra)
shutil.copyfile(d/'base.img', d/'desktop.img')
iso = d/'iso'
(iso/'boot/grub').mkdir(parents=True)
for name in ('cubit_kernel', 'initrd.img'): shutil.copyfile(stage/name, iso/'boot'/name)
(iso/'boot/grub/grub.cfg').write_text('serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\nmenuentry "Penny TLS inspector" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n module /boot/initrd.img init.img\n}\n')
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
pointer=[80,80]
def move_to(x,y):
    while pointer!=[x,y]:
        dx=max(-24,min(24,x-pointer[0]));dy=max(-24,min(24,y-pointer[1]))
        command(f'mouse_move {dx} {dy}')
        pointer[0]+=dx;pointer[1]+=dy;time.sleep(.08)
def click(x,y):
    move_to(x,y);command('mouse_button 1');time.sleep(.2);command('mouse_button 0');time.sleep(.8)
def navigate(url):
    symbols = {':':'shift-semicolon', '/':'slash', '.':'dot', '-':'minus', '?':'shift-slash', '=':'equal', '&':'shift-7', '_':'shift-minus'}
    for attempt in range(2):
        offset=len(text())
        key('ctrl-l');time.sleep(.8)
        for c in url:
            command('sendkey ' + symbols.get(c, c) + ' 50')
            time.sleep(.18)
        key('ret')
        try:
            wait_for(lambda:'CUBITSHELL: navigate' in text()[offset:],8)
            return
        except TimeoutError:
            if attempt:raise AssertionError('Address input was not submitted: '+url)
            print('Retrying cancelled address input:',url,flush=True)
def inspect(name):
    key('alt-v');key('c');time.sleep(1)
    image = Image.open(screenshot(name)).convert('RGB')
    image.save(d/(name+'.png'))
    key('esc');assert_healthy()
def report_after(offset, url, timeout=65):
    wait_for(lambda: 'PENNY-TLS: '+url+'\n' in text()[offset:],timeout)
    # TLS notification can arrive after load completion; wait for it to settle.
    time.sleep(2)
    chunks = text()[offset:].split('PENNY-TLS: '+url+'\n')[1:]
    return chunks[-1].split('PENNY-TLS-END')[0]
def site_reports(offset, target):
    host=urlsplit(target).hostname.removeprefix('www.')
    records=[]
    for chunk in text()[offset:].split('PENNY-TLS: ')[1:]:
        location,_,body=chunk.partition('\n')
        if (urlsplit(location).hostname or '').removeprefix('www.')==host:
            records.append(body.split('PENNY-TLS-END')[0])
    return ''.join(records)
with (d/'qemu.log').open('w') as log:
    vm=subprocess.Popen(args,cwd=root,stdout=log,stderr=log)
try:
    wait_for(lambda:'CUBITSHELL: loading page 0' in text(),90)
    if os.environ.get('PENNY_ONLY_SITE'):
        try:
            wait_for(lambda:bool(site_reports(0,url)),90)
            time.sleep(15)
            status='verified TLS' if 'Main document connection (verified by rustls)' in site_reports(0,url) else 'no verified TLS'
        except TimeoutError:
            status='load timeout'
        Image.open(screenshot('fresh-site')).save(d/'fresh-site.png')
        inspect('fresh-site-inspector')
        report=site_reports(0,url)
        if 'Page load: complete' in report:status+='; load complete'
        (d/'public-results.json').write_text(json.dumps([{'url':url,'status':status}],indent=2))
        print('FRESH PUBLIC:',url,status,flush=True)
        key('ctrl-shift-w');wait_for(lambda:'CUBITSHELL: closed' in text(),30);assert_healthy()
        print('PASS fresh browser close/fault scan',flush=True)
    else:
        report = report_after(0,url)
        assert 'Main document connection (verified by rustls)' in report, report
        der = ssl.PEM_cert_to_DER_cert((pki/'good.pem').read_text())
        assert hashlib.sha256(der).hexdigest().upper() in report, report
        assert 'TLS 1.3' in report or 'TLS 1.2' in report, report
        assert 'Subject: tls-test.cubit.internal' in report, report
        inspect('tls-valid')
        print('PASS local TLS: actual certificate fingerprint/protocol/subject',flush=True)
        for kind in ('wronghost','expired','untrusted'):
            target=f'https://tls-test.cubit.internal:{ports[kind]}/'
            offset=len(text());navigate(target)
            report=report_after(offset,target)
            assert 'Main document connection (verified by rustls)' not in report, (kind,report)
            assert 'No verified TLS details' in report, (kind,report)
            inspect('tls-'+kind)
            print('PASS rejected certificate:',kind,flush=True)
        target=f'http://10.0.2.2:{server.server_port}/'
        offset=len(text());navigate(target);report=report_after(offset,target)
        assert 'not encrypted' in report, report
        inspect('tls-http')
        results=[]
        for index,target in enumerate(os.environ.get('PENNY_PUBLIC_SITES','').split()):
            key('ctrl-t');time.sleep(2)
            offset=len(text());navigate(target)
            try:
                # Redirects may change the final URL; record actual reports rather
                # than treating a redirect as a load failure.
                wait_for(lambda:bool(site_reports(offset,target)),90)
                time.sleep(12)
                reports=site_reports(offset,target)
                verified='Main document connection (verified by rustls)' in reports
                status=('verified TLS; '+('load complete' if 'Page load: complete' in reports else 'subresources still loading')) if verified else 'no verified TLS report'
            except TimeoutError:
                status='load timeout'
            screenshot('site-'+str(index))
            Image.open(d/('site-'+str(index)+'.ppm')).save(d/('site-'+str(index)+'.png'))
            inspect('site-'+str(index)+'-inspector')
            # Include completion updates that arrived while opening the inspector.
            if status != 'load timeout':
                reports=site_reports(offset,target)
                if 'Main document connection (verified by rustls)' in reports:
                    status='verified TLS; '+('load complete' if 'Page load: complete' in reports else 'subresources still loading')
            results.append({'url':target,'status':status})
            print('PUBLIC:',target,status,flush=True)
        (d/'public-results.json').write_text(json.dumps(results,indent=2))
        key('ctrl-shift-w');wait_for(lambda:'CUBITSHELL: closed' in text(),30)
        assert_healthy()
        print('PASS native TLS inspector, certificate rejection, HTTP clearing and clean close',flush=True)
finally:
    stop()
    for srv in servers:srv.shutdown()
