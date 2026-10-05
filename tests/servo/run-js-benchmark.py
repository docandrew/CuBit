"""Measure fixed JavaScript workloads in native Penny under QEMU TCG.

Run in Nix with coordination/build.lock for shared staged binaries.
PENNY_STAGE and PENNY_APP may instead select a wholly private binary snapshot.
Uses a private disk. TCG numbers are diagnostic, never physical-platform comparisons.
"""
from pathlib import Path
from performance_report import memory_report
from js_benchmark_report import native_report
import functools
import http.server
import hashlib
import json
import os
import platform
import re
import shutil
import socket
import subprocess
import tempfile
import threading
import time

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix='penny-js-'))
print('ARTIFACTS:', d, flush=True)
def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)
class Page(http.server.SimpleHTTPRequestHandler):
    def log_message(self, *args): pass
server = http.server.ThreadingHTTPServer(('127.0.0.1', 0),
    functools.partial(Page, directory=str(Path(__file__).parent)))
server.daemon_threads = True
threading.Thread(target=server.serve_forever, daemon=True).start()
url = f'http://10.0.2.2:{server.server_port}/js-benchmark.html?autorun=1'
(d / 'pages').write_text(url + '\n')
(d / 'empty').write_text('')
profile = d / 'init.ccl'
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) (start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) (start "cubitshell.app" (priority 3) (network approve-declared)))')
stage = Path(os.environ.get('PENNY_STAGE', root / 'kernel/isodir/boot'))
app = Path(os.environ.get('PENNY_APP', root / 'userspace/rust/build/cubitshell.app'))
(d/'inputs.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p)+'\n' for p in [app, root/'userspace/servo/servo-cargo.sh', root/'userspace/servo/overlay/ports/cubitshell/Cargo.toml', Path(os.environ.get('PENNY_INITRD',str(stage/'initrd.img'))), *[stage/n for n in ('cubit_kernel','desktop.svc','display.svc','clock.svc','logstore.svc')]]))
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
(iso/'boot/grub/grub.cfg').write_text('serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\nmenuentry "Penny JavaScript benchmark" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n module /boot/initrd.img init.img\n}\n')
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
        while not data.endswith(b'(qemu) '):
            chunk = sock.recv(65536)
            if not chunk: raise EOFError('QEMU monitor closed')
            data += chunk
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
started = time.monotonic()
try:
    wait_for(lambda: any(marker in text() for marker in
        ('CuBitBrowserPerfJSDone', 'CuBitBrowserPerfJSFailed')), 600)
    report = native_report(text())
    report['environment'] = {
        'os': 'CuBit', 'browser': 'Penny / Servo / SpiderMonkey',
        'binary_source': str(app),
        'source_config_binding': 'config hashes describe current checkout; not an attestation that selected binary was built from it',
        'execution_mode': 'not runtime-attested; configured without JIT in servo-cargo.sh',
        'acceleration': 'QEMU TCG, multi-thread', 'guest_cpu': 'Broadwell',
        'guest_cpus': 4, 'guest_memory': '2 GiB',
        'host': platform.platform(), 'host_cpu_count': os.cpu_count(),
        'host_load': os.getloadavg(),
        'qemu': run('qemu-system-x86_64', '--version').stdout.splitlines()[0],
        'comparison_limit': 'Emulated timings; shared host not performance-isolated. Not Linux/Windows parity evidence.',
        'wall_seconds_including_startup': time.monotonic() - started,
    }
    report['fixture_sha256'] = {name: hashlib.sha256(
        (Path(__file__).parent / name).read_bytes()).hexdigest()
        for name in ('js-benchmark.js', 'js-benchmark.html', 'js_benchmark_report.py')}
    (d/'results.json').write_text(json.dumps(report, indent=2))
    Image.open(screenshot('javascript')).save(d/'javascript.png')
    key('ctrl-shift-w')
    wait_for(lambda: 'CUBITSHELL: closed' in text(), 30)
    assert_healthy()
    memory_report(serial, d/'memory.json')
    print(json.dumps(report, indent=2), flush=True)
    print('PASS native JavaScript workload checks and clean close; TCG timings only', flush=True)
except BaseException:
    if vm.poll() is None:
        try: Image.open(screenshot('failure')).save(d/'failure.png')
        except Exception: pass
    raise
finally:
    stop()
    server.shutdown()
