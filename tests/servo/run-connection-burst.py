"""A burst of parallel requests stays inside Penny's declared connections.

Penny's manifest declares 32 TCP connections, and netstack refuses any
beyond them ("the scope's declared connections are all in use"). A page
that starts many requests at once used to have hyper open one HTTP/1.1
connection per request, so everything past the declaration failed (WEB-001
compatibility survey: GitHub lost 58 of 102 resources). The page here
starts BURST_REQUESTS fetches at once across BURST_ORIGINS local origins;
each response is held for RESPONSE_DELAY_SECONDS so they overlap. Pass: every
fetch returns its body and netstack refused nothing.

Run inside the Nix shell under coordination/build.lock (native build
staged: make -C kernel servo). PENNY_ACCEL=kvm uses KVM (default TCG).
"""
from pathlib import Path
import http.server
import json
import os
import re
import shutil
import subprocess
import tempfile
import threading
import time

BURST_REQUESTS = 96
BURST_ORIGINS = 4
RESPONSE_DELAY_SECONDS = 0.3
BODY = b"pong"
FETCH_DEADLINE_MS = 30000
RUN_SECONDS = 600
REFUSAL = "netstack: open: the scope's declared connections are all in use"

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix="penny-burst-"))
print("ARTIFACTS:", d, flush=True)
urls = []


class Page(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        if self.path == "/":
            body = ("""<title>Penny connection burst</title><h1>Penny connection burst</h1>
<pre id='status'>Starting...</pre><script>
(async()=>{const start=performance.now();
const one=async(url)=>{const c=new AbortController();const t=setTimeout(()=>c.abort(),DEADLINE);
try{const r=await fetch(url,{signal:c.signal,cache:'no-store'});return (await r.text())==='pong';}
catch(e){return false;}finally{clearTimeout(t);}};
const results=await Promise.all(URLS.map(one));const ok=results.filter(x=>x).length;
document.getElementById('status').textContent=ok+'/'+results.length+' ok';
document.title='CuBitBrowserPerf:burst,'+ok+','+(results.length-ok)+','+Math.round(performance.now()-start);
await new Promise(r=>setTimeout(r,500));document.title='CuBitBrowserPerfDone';})();</script>"""
                    .replace("URLS", json.dumps(urls)).replace("DEADLINE", str(FETCH_DEADLINE_MS))).encode()
        else:
            time.sleep(RESPONSE_DELAY_SECONDS)
            body = BODY
        self.send_response(200)
        self.send_header("Content-Type", "text/html" if self.path == "/" else "text/plain")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        try:
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            pass

    def log_message(self, *args):
        pass


servers = []
for _ in range(BURST_ORIGINS + 1):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Page)
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    servers.append(server)
# QEMU user networking: the host is 10.0.2.2.
urls += [f"http://10.0.2.2:{servers[1 + i % BURST_ORIGINS].server_port}/r{i}" for i in range(BURST_REQUESTS)]
page = f"http://10.0.2.2:{servers[0].server_port}/"


def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)


(d / "pages").write_text(page + "\n")
(d / "empty").write_text("")
profile = d / "init.ccl"
profile.write_text('(startup v1 (start "logstore.svc") (start "clock.svc") '
                   '(start "display.svc") (start "desktop.svc") '
                   '(start "cubitshell.app" (network approve-declared)))')
stage = root / "kernel/isodir/boot"
app = root / "userspace/rust/build/cubitshell.app"
run("python3", "tools/build_development_disk.py", str(d / "desktop.img"), "--boot",
    *[str(stage / name) for name in ("logstore.svc", "clock.svc", "display.svc", "desktop.svc")], str(app),
    "--file", f"init.ccl={profile}", "--file", f"servo/pages={d}/pages",
    "--file", f"Bookmarks/keep={d}/empty", "--file", f"servo/perf-check={d}/empty")
iso = d / "iso"
(iso / "boot/grub").mkdir(parents=True)
for name in ("cubit_kernel", "initrd.img"):
    shutil.copyfile(stage / name, iso / "boot" / name)
(iso / "boot/grub/grub.cfg").write_text(
    'serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\n'
    'menuentry "Penny connection burst" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n'
    ' module /boot/initrd.img init.img\n}\n')
run("grub-mkrescue", "-o", str(d / "boot.iso"), str(iso))
serial = d / "serial.log"
accel = ["-accel", "kvm", "-cpu", "host"] if os.environ.get("PENNY_ACCEL") == "kvm" else \
    ["-accel", "tcg,thread=multi", "-cpu", "Broadwell"]
args = ["qemu-system-x86_64", *accel, "-machine", "q35", "-smp", "4", "-m", "2G",
        "-cdrom", str(d / "boot.iso"), "-display", "none", "-serial", f"file:{serial}",
        "-vga", "none", "-device", "virtio-vga,xres=1024,yres=768",
        "-drive", f"file={d}/desktop.img,if=none,id=nvme0,format=raw", "-device", "nvme,serial=cubitnvme,drive=nvme0",
        "-netdev", "user,id=net0", "-device", "virtio-net-pci,netdev=net0", "-no-reboot"]


def text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def healthy():
    assert not re.search(r"USER-MEMORY-FAULT|CUBITSHELL: panic|EXCEPTION:", text()), "native browser fault"


with (d / "qemu.log").open("w") as log:
    vm = subprocess.Popen(args, cwd=root, stdout=log, stderr=log)
try:
    deadline = time.monotonic() + RUN_SECONDS
    while "CUBITSHELL-PERF: CuBitBrowserPerfDone" not in text():
        if vm.poll() is not None:
            raise RuntimeError("QEMU exited")
        healthy()
        if time.monotonic() > deadline:
            raise TimeoutError("the burst page did not finish")
        time.sleep(0.2)
    healthy()
    match = re.search(r"CUBITSHELL-PERF: CuBitBrowserPerf:burst,(\d+),(\d+),(\d+)", text())
    assert match, "no burst result"
    ok, failed, ms = map(int, match.groups())
    refusals = text().count(REFUSAL)
    peaks = [int(p) for p in re.findall(r"PENNY-NET connections peak=(\d+)", text())]
    summary = {"requests": BURST_REQUESTS, "origins": BURST_ORIGINS, "ok": ok, "failed": failed,
               "burst_ms": ms, "netstack_refusals": refusals, "peak_connections": max(peaks, default=None)}
    (d / "results.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary), flush=True)
    assert ok == BURST_REQUESTS and failed == 0, f"{failed} of {BURST_REQUESTS} fetches failed"
    assert refusals == 0, f"netstack refused {refusals} connections"
    print("PASS connection burst within the declared connections", flush=True)
finally:
    if vm.poll() is None:
        vm.terminate()
        try:
            vm.wait(timeout=5)
        except subprocess.TimeoutExpired:
            vm.kill()
            vm.wait()
    for server in servers:
        server.shutdown()
