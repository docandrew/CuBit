"""Native persistent-HTTP connection endurance. Nix/build.lock required.

Uses staged native components, disposable disk, 40 local HTTP/1.1 origins,
80 timed fetches, response-content checks, screenshot and clean-close oracle.
Timings include TCG emulation; they are not physical hardware benchmarks.
"""
from pathlib import Path
from performance_report import memory_report
import http.server
import hashlib
import json
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
d = Path(tempfile.mkdtemp(prefix='penny-endurance-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)
origins = []
class Page(http.server.BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'
    def do_GET(self):
        if self.path == '/':
            body = ("""<title>Penny connection endurance</title><h1>Penny connection endurance</h1>
<pre id='status'>Running 80 requests across 40 origins...</pre><script>
(async()=>{let results=[];work:for(let round=0;round<2;round++)for(let url of URLS){
let start=performance.now(),ok=false,error='';let controller=new AbortController();
let timer=setTimeout(()=>controller.abort(),8000);
try {let r=await fetch(url,{signal:controller.signal});ok=(await r.text())==='pong';}catch(e){error=String(e);}
clearTimeout(timer);document.title='CuBitBrowserPerf:'+round+','+results.length+','+(ok?1:0)+','+Math.round(performance.now()-start);results.push({round,url,ok,error,ms:performance.now()-start});
document.getElementById('status').textContent=results.length+'/80 requests; failures '+results.filter(r=>!r.ok).length;
await new Promise(r=>setTimeout(r,1000));if(!ok)break work;}
document.title='CuBitBrowserPerfDone';})();</script>""".replace('URLS',json.dumps(origins))).encode()
        else: body = b'pong'
        self.send_response(200)
        self.send_header('Content-Type','text/html' if self.path=='/' else 'text/plain')
        self.send_header('Content-Length',str(len(body)))
        self.send_header('Access-Control-Allow-Origin','*')
        self.end_headers()
        try:self.wfile.write(body)
        except (BrokenPipeError,ConnectionResetError):pass
    def log_message(self,*args):pass
servers=[]
for _ in range(41):
    srv=http.server.ThreadingHTTPServer(('127.0.0.1',0),Page)
    srv.daemon_threads=True
    threading.Thread(target=srv.serve_forever,daemon=True).start()
    servers.append(srv)
origins=[f'http://10.0.2.2:{srv.server_port}/ping' for srv in servers[1:]]
(d/'origins.json').write_text(json.dumps(origins,indent=2))
url=f'http://10.0.2.2:{servers[0].server_port}/'
(d / 'pages').write_text(url + '\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = root / 'kernel/isodir/boot'
app = root / 'userspace/rust/build/cubitshell.app'
(d/'inputs.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p)+'\n' for p in [app, Path(os.environ.get('PENNY_INITRD',str(stage/'initrd.img'))), *[stage/n for n in ('cubit_kernel','desktop.svc','display.svc')]]))
fonts = Path(os.environ['IBM_PLEX_SANS_FONT']).parent
extra = ['--file', f'servo/pages={d}/pages', '--file', f'Bookmarks/keep={d}/empty','--file',f'servo/perf-check={d}/empty']
for font in ('IBMPlexSans-Regular.ttf', 'IBMPlexSans-Bold.ttf', 'IBMPlexSerif-Regular.ttf', 'IBMPlexMono-Regular.ttf'):
    extra += ['--file', f'fonts/{font}={fonts/font}']
run('python3', 'tools/build_development_disk.py', str(d/'base.img'), '--boot',
    *[str(stage/name) for name in ('logstore.svc', 'clock.svc', 'display.svc', 'desktop.svc')], str(app),
    '--file', f'init.ccl={profile}', *extra)
shutil.copyfile(d/'base.img', d/'desktop.img')
iso = d/'iso'
(iso/'boot/grub').mkdir(parents=True)
for name in ('cubit_kernel', 'initrd.img'):
    source=Path(os.environ['PENNY_INITRD']) if name=='initrd.img' and os.environ.get('PENNY_INITRD') else stage/name
    shutil.copyfile(source, iso/'boot'/name)
(iso/'boot/grub/grub.cfg').write_text('serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\nmenuentry "Browser tab rail" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n module /boot/initrd.img init.img\n}\n')
run('grub-mkrescue', '-o', str(d/'boot.iso'), str(iso))
serial = d/'serial.log'
monitor = d/'monitor.sock'
args = ['qemu-system-x86_64', '-accel', 'tcg,thread=multi', '-machine', 'q35', '-cpu', 'Broadwell',
        '-smp', '4', '-m', '2G', '-cdrom', str(d/'boot.iso'), '-display', 'none', '-serial', f'file:{serial}',
        '-monitor', f'unix:{monitor},server,nowait', '-vga', 'none', '-device', 'virtio-vga,xres=1024,yres=768',
        '-drive', f'file={d}/desktop.img,if=none,id=nvme0,format=raw', '-device', 'nvme,serial=cubitnvme,drive=nvme0',
        '-object', f'filter-dump,id=perf,netdev=net0,file={d}/network.pcap', '-netdev', 'user,id=net0', '-device', 'virtio-net-pci,netdev=net0', '-no-reboot']
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
started=time.monotonic()
try:
    wait_for(lambda:'CUBITSHELL-PERF: CuBitBrowserPerfDone' in text(),600)
    assert_healthy()
    rows=[{'round':int(a),'index':int(b),'ok':c=='1','ms':int(e)} for a,b,c,e in re.findall(r'CUBITSHELL-PERF: CuBitBrowserPerf:(\d+),(\d+),(\d+),(\d+)',text())]
    elapsed=time.monotonic()-started
    summary={'elapsed_seconds':elapsed,'requests':len(rows),'failures':sum(not r['ok'] for r in rows),'results':rows}
    (d/'results.json').write_text(json.dumps(summary,indent=2))
    Image.open(screenshot('endurance')).save(d/'endurance.png')
    key('ctrl-shift-w');wait_for(lambda:'CUBITSHELL: closed' in text(),30)
    assert_healthy()
    print(json.dumps({k:v for k,v in summary.items() if k!='results'}),flush=True)
    memory_report(serial,d/'memory.json')
    assert [r['index'] for r in rows]==list(range(len(rows))), 'missing or duplicate result callbacks'
    assert len(rows)==80 and all(r['ok'] for r in rows),'connection endurance failure; see results.json'
    print('PASS connection endurance and clean close',flush=True)
except BaseException:
    if vm.poll() is None:
        try:Image.open(screenshot('failure')).save(d/'failure.png')
        except Exception:pass
    raise
finally:
    stop()
    for srv in servers:srv.shutdown()
