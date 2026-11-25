"""A connect waiting for Penny's whole connection share evicts an idle one.

Penny's HTTP client opens at most HTTP_CLIENT_CONNECTIONS (24) connections
(connection_limit.rs), and hyper keeps finished connections idle in its pool
for up to 5 s, each holding its share. A page that had reached 24 hosts
waited for those idle connections to time out before it could reach a 25th
(WEB-001 survey: waits up to 7.6 s on nasa.gov, 101 waits on calendly.com).
Here the page fetches once from FIRST_ORIGINS origins (with the page's own,
24 connections, then all idle), then at once from SECOND_ORIGINS new ones.
Pass: every fetch succeeds and the second round takes less than
SECOND_ROUND_LIMIT_MS, well under the pool's idle timeout.

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

FIRST_ORIGINS = 23
SECOND_ORIGINS = 6
POOL_IDLE_TIMEOUT_MS = 5000
SECOND_ROUND_LIMIT_MS = 2000
FETCH_DEADLINE_MS = 30000
RUN_SECONDS = 300
BODY = b"pong"

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix="penny-evict-"))
print("ARTIFACTS:", d, flush=True)
first, second = [], []


class Page(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        if self.path == "/":
            body = ("""<title>Penny idle eviction</title><h1>Penny idle eviction</h1><script>
(async()=>{const one=async(url)=>{const c=new AbortController();const t=setTimeout(()=>c.abort(),DEADLINE);
try{const r=await fetch(url,{signal:c.signal,cache:'no-store'});return (await r.text())==='pong';}
catch(e){return false;}finally{clearTimeout(t);}};
const a=await Promise.all(FIRST.map(one));await new Promise(r=>setTimeout(r,300));
const start=performance.now();const b=await Promise.all(SECOND.map(one));const ms=Math.round(performance.now()-start);
const ok=a.filter(x=>x).length+b.filter(x=>x).length;
document.title='CuBitBrowserPerf:evict,'+ok+','+(a.length+b.length-ok)+','+ms;
await new Promise(r=>setTimeout(r,500));document.title='CuBitBrowserPerfDone';})();</script>"""
                    .replace("FIRST", json.dumps(first)).replace("SECOND", json.dumps(second))
                    .replace("DEADLINE", str(FETCH_DEADLINE_MS))).encode()
        else:
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
for _ in range(1 + FIRST_ORIGINS + SECOND_ORIGINS):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Page)
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    servers.append(server)
# QEMU user networking: the host is 10.0.2.2.
first += [f"http://10.0.2.2:{s.server_port}/a" for s in servers[1:1 + FIRST_ORIGINS]]
second += [f"http://10.0.2.2:{s.server_port}/b" for s in servers[1 + FIRST_ORIGINS:]]
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
    'menuentry "Penny idle eviction" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n'
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
            raise TimeoutError("the eviction page did not finish")
        time.sleep(0.2)
    healthy()
    match = re.search(r"CUBITSHELL-PERF: CuBitBrowserPerf:evict,(\d+),(\d+),(\d+)", text())
    assert match, "no eviction result"
    ok, failed, ms = map(int, match.groups())
    waits = [int(w) for w in re.findall(r"PENNY-NET waited (\d+) ms", text())]
    evictions = re.findall(r"evictions=(\d+)", text())
    summary = {"first_origins": FIRST_ORIGINS, "second_origins": SECOND_ORIGINS, "ok": ok, "failed": failed,
               "second_round_ms": ms, "longest_wait_ms": max(waits, default=0),
               "evictions": int(evictions[-1]) if evictions else 0}
    (d / "results.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary), flush=True)
    assert failed == 0, f"{failed} fetches failed"
    assert ms < SECOND_ROUND_LIMIT_MS, f"the second round took {ms} ms (pool idle timeout {POOL_IDLE_TIMEOUT_MS} ms)"
    print("PASS a waiting connect evicts an idle connection", flush=True)
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
