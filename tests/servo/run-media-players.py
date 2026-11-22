"""Many media elements at once stay within Penny's threads (WEB-001).

Every media element that loads gets a GStreamer player with its own
threads, and CuBit gives a process MAX_THREADS_PER_PROCESS (128). slack.com's
28 autoplaying videos took Penny to the limit: the next thread Servo needed
could not start and the Constellation panicked (thread census:
PENNY-THREADS lines, overlay/ports/cubitshell/src/thread_census.rs). The page
here creates VIDEOS autoplaying videos whose server sends the first bytes of
a WebM file and then holds the response, so every player stays alive; then
it starts a dedicated worker (a new thread). Pass: the worker answers, no
thread was refused, Penny did not panic, and the bound on live players
(a commit on Penny's Servo fork) made the extra videos wait.

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

VIDEOS = 48
# The start of a WebM file (an EBML header ID); the rest never comes.
WEBM_START = bytes([0x1A, 0x45, 0xDF, 0xA3])
WEBM_DECLARED_BYTES = 1 << 20
SETTLE_MS = 8000
WORKER_DEADLINE_MS = 20000
RUN_SECONDS = 300
REFUSAL = "PENNY-THREADS refused="

root = Path(__file__).resolve().parents[2]
d = Path(tempfile.mkdtemp(prefix="penny-media-"))
print("ARTIFACTS:", d, flush=True)
stop = threading.Event()
video_base = []


class Page(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        if self.path == "/":
            body = ("""<title>Penny media players</title><h1>Penny media players</h1><div id='v'></div><script>
const box=document.getElementById('v');
for(let i=0;i<VIDEOS;i++){const v=document.createElement('video');v.muted=true;v.autoplay=true;v.loop=true;
v.width=32;v.height=18;v.src=BASE+'/v'+i+'.webm';box.appendChild(v);}
setTimeout(()=>{let done=false;const finish=(s)=>{if(done)return;done=true;
document.title='CuBitBrowserPerf:media,'+s;setTimeout(()=>{document.title='CuBitBrowserPerfDone';},500);};
try{const w=new Worker(URL.createObjectURL(new Blob(["postMessage('alive')"],{type:'text/javascript'})));
w.onmessage=(e)=>finish(String(e.data));w.onerror=()=>finish('worker-error');}catch(e){finish('worker-throw');}
setTimeout(()=>finish('worker-timeout'),WORKER_DEADLINE);},SETTLE);</script>"""
                    .replace("VIDEOS", str(VIDEOS)).replace("BASE", json.dumps(video_base[0]))
                    .replace("WORKER_DEADLINE", str(WORKER_DEADLINE_MS)).replace("SETTLE", str(SETTLE_MS))).encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return
        self.send_response(200)
        self.send_header("Content-Type", "video/webm")
        self.send_header("Content-Length", str(WEBM_DECLARED_BYTES))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        try:
            self.wfile.write(WEBM_START)
            self.wfile.flush()
            stop.wait()
        except (BrokenPipeError, ConnectionResetError):
            pass

    def log_message(self, *args):
        pass


servers = []
for _ in range(2):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Page)
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    servers.append(server)
# QEMU user networking: the host is 10.0.2.2.
video_base.append(f"http://10.0.2.2:{servers[1].server_port}")
page = f"http://10.0.2.2:{servers[0].server_port}/"


def run(*args):
    return subprocess.run(args, cwd=root, check=True, capture_output=True, text=True)


(d / "pages").write_text(page + "\n")
(d / "empty").write_text("")
profile = d / "init.ccl"
profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) '
                   '(start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) '
                   '(start "cubitshell.app" (priority 3) (network approve-declared)))')
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
    'menuentry "Penny media players" {\n multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n'
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
        assert "PENNY-ABORT" not in text(), "Penny aborted"
        if REFUSAL in text():
            raise AssertionError("a thread was refused: " + text()[text().index(REFUSAL):][:200])
        if time.monotonic() > deadline:
            raise TimeoutError("the media page did not finish")
        time.sleep(0.2)
    healthy()
    match = re.search(r"CUBITSHELL-PERF: CuBitBrowserPerf:media,(\S+)", text())
    assert match, "no media result"
    peaks = [int(p) for p in re.findall(r"PENNY-THREADS peak=(\d+)", text())]
    waiting = re.findall(r"PENNY-MEDIA players=(\d+) limit=(\d+) waiting=(\d+)", text())
    summary = {"videos": VIDEOS, "worker": match.group(1), "thread_peak": max(peaks, default=None),
               "refused": text().count(REFUSAL), "waiting_lines": len(waiting),
               "most_waiting": max((int(w[2]) for w in waiting), default=0)}
    (d / "results.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary), flush=True)
    assert match.group(1) == "alive", f"the worker did not answer: {match.group(1)}"
    assert summary["refused"] == 0, "a thread was refused"
    assert waiting, "no media element waited for a player"
    print("PASS media players within the process's threads", flush=True)
finally:
    stop.set()
    if vm.poll() is None:
        vm.terminate()
        try:
            vm.wait(timeout=5)
        except subprocess.TimeoutExpired:
            vm.kill()
            vm.wait()
    for server in servers:
        server.shutdown()
