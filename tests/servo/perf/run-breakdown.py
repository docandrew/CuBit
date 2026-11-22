#!/usr/bin/env python3
"""WEB-001 breakdown: the same Penny bench on CuBit (QEMU KVM) and hosted Linux.

Run in Nix on the measurement host, with nothing else heavy running:

    run-breakdown.py --out DIR --runs 5 [--pages article,css-stress,js-benchmark]
        [--platforms cubit,hosted] [--hosted-app PATH]

CuBit: a private development disk and ISO from the staged images
(PENNY_STAGE, default kernel/isodir/boot; PENNY_APP, default
userspace/rust/build/cubitshell.app), booted under KVM with -cpu host and
GUEST_CPUS vCPUs; Penny runs from init.ccl in the desktop with the bench
flags and logs to the serial port. Hosted: cubitshell built for
x86_64-unknown-linux-gnu (build-cubitshell-hosted.sh), pinned to GUEST_CPUS
host CPUs with taskset, at CuBit's viewport, logging to stderr. One page per
run; runs interleave platforms so host-load drift hits both. Every run keeps
its log and result.json; breakdown.py summarizes. The host serves the pages
over HTTP on loopback (the guest reaches it through QEMU's user network).
"""
import argparse
import functools
import hashlib
import http.server
import json
import os
import pathlib
import platform
import re
import shutil
import subprocess
import tempfile
import threading
import time

from breakdown import parse

ROOT = pathlib.Path(__file__).resolve().parents[3]
HERE = pathlib.Path(__file__).resolve().parent
GUEST_CPUS = 4
GUEST_MEMORY = "4G"
GUEST_DISPLAY = ("1024", "768")
RUN_DEADLINE_SECONDS = 600
POLL_SECONDS = 0.2
QEMU_GUEST_HOST = "10.0.2.2"
JS_DONE = "CuBitBrowserPerfJSDone"
PAGES = {
    "article": "article.html",
    "css-stress": "css-stress.html",
    "js-benchmark": "js-benchmark.html?autorun=1",
}
FAILURES = re.compile(r"USER-MEMORY-FAULT|CUBITSHELL: panic|PENNY-ABORT:|CUBITSHELL: FAIL|EXCEPTION:")
INIT = ('(startup v1 (start "logstore.svc" (priority 5)) (start "clock.svc" (priority 5)) '
        '(start "display.svc" (priority 5)) (start "desktop.svc" (priority 4)) '
        '(start "cubitshell.app" (priority 3) (network approve-declared)))')


FRAMES = ("top", "keys", "scrolled")
SAVE_FRAMES = False
# --alloc-timing: bench.rs times allocations of 1 MiB or more per input step
# (PENNY-ALLOC lines; alloc_report.py summarizes them).
ALLOC_TIMING = False


def flags(page):
    """The bench flag files, the same on both platforms."""
    values = {"perf-check": "", "profile-check": "", "batch-test": "", "bench": "",
              "log-level": "error"}
    if SAVE_FRAMES:
        values["bench-frames"] = ""
    if ALLOC_TIMING:
        values["alloc-timing"] = ""
    if page == "js-benchmark":
        values["bench-wait-title"] = JS_DONE
    return values


class Quiet(http.server.SimpleHTTPRequestHandler):
    def log_message(self, *args):
        pass


def serve(directory):
    server = http.server.ThreadingHTTPServer(
        ("127.0.0.1", 0), functools.partial(Quiet, directory=str(directory)))
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def sha256(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


class CuBit:
    def __init__(self, work, port, app=None):
        self.work = work
        self.port = port
        self.stage = pathlib.Path(os.environ.get("PENNY_STAGE", ROOT / "kernel/isodir/boot"))
        self.app = pathlib.Path(app or os.environ.get("PENNY_APP", ROOT / "userspace/rust/build/cubitshell.app"))
        iso = work / "iso"
        (iso / "boot/grub").mkdir(parents=True, exist_ok=True)
        for name in ("cubit_kernel", "initrd.img"):
            shutil.copyfile(self.stage / name, iso / "boot" / name)
        (iso / "boot/grub/grub.cfg").write_text(
            'serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\n'
            'menuentry "Penny breakdown" {\n multiboot /boot/cubit_kernel\n'
            f' set gfxpayload={GUEST_DISPLAY[0]}x{GUEST_DISPLAY[1]}x32\n'
            ' module /boot/initrd.img init.img\n}\n')
        self.iso = work / "boot.iso"
        subprocess.run(["grub-mkrescue", "-o", str(self.iso), str(iso)], check=True, capture_output=True)
        self.inputs = {str(p): sha256(p) for p in [self.app, *[self.stage / n for n in (
            "cubit_kernel", "initrd.img", "desktop.svc", "display.svc", "clock.svc", "logstore.svc")]]}

    def run(self, page, directory):
        url = f"http://{QEMU_GUEST_HOST}:{self.port}/{PAGES[page]}"
        (directory / "pages").write_text(url + "\n")
        (directory / "init.ccl").write_text(INIT)
        files = ["--file", f"init.ccl={directory / 'init.ccl'}",
                 "--file", f"servo/pages={directory / 'pages'}"]
        (directory / "empty").write_text("")
        files += ["--file", f"Bookmarks/keep={directory / 'empty'}"]
        for name, value in flags(page).items():
            (directory / ("flag-" + name)).write_text(value)
            files += ["--file", f"servo/{name}={directory / ('flag-' + name)}"]
        disk = directory / "desktop.img"
        subprocess.run(["python3", "tools/build_development_disk.py", str(disk), "--boot",
                        *[str(self.stage / n) for n in ("logstore.svc", "clock.svc", "display.svc", "desktop.svc")],
                        str(self.app), *files], cwd=ROOT, check=True, capture_output=True)
        serial = directory / "serial.log"
        argv = ["qemu-system-x86_64", "-accel", "kvm", "-cpu", "host", "-machine", "q35",
                "-smp", str(GUEST_CPUS), "-m", GUEST_MEMORY, "-cdrom", str(self.iso),
                "-display", "none", "-serial", f"file:{serial}", "-vga", "none",
                "-device", f"virtio-vga,xres={GUEST_DISPLAY[0]},yres={GUEST_DISPLAY[1]}",
                "-drive", f"file={disk},if=none,id=nvme0,format=raw",
                "-device", "nvme,serial=cubitnvme,drive=nvme0",
                "-netdev", "user,id=net0", "-device", "virtio-net-pci,netdev=net0", "-no-reboot"]
        (directory / "execution.json").write_text(json.dumps({"argv": argv, "inputs": self.inputs}, indent=1))
        started = time.monotonic()
        with (directory / "qemu.log").open("w") as log:
            vm = subprocess.Popen(argv, stdout=log, stderr=log)
        outcome = "TIMEOUT"
        try:
            while time.monotonic() - started < RUN_DEADLINE_SECONDS:
                text = serial.read_text(errors="replace") if serial.exists() else ""
                if "PENNY-BENCH: done" in text:
                    outcome = "DONE"
                    break
                if FAILURES.search(text):
                    outcome = "FAIL"
                    break
                if vm.poll() is not None:
                    outcome = "VM-EXIT"
                    break
                time.sleep(POLL_SECONDS)
        finally:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
            if SAVE_FRAMES:
                for name in FRAMES:
                    subprocess.run(["debugfs", "-R", f"dump /Bookmarks/penny-{name}.ppm "
                                    f"{directory / ('penny-' + name + '.ppm')}", str(disk)],
                                   capture_output=True)
            disk.unlink(missing_ok=True)
            # build_development_disk.py's symbol tree beside the disk (~11 MB a run).
            shutil.rmtree(directory / "debug", ignore_errors=True)
        return outcome, serial.read_text(errors="replace"), time.monotonic() - started


class Hosted:
    def __init__(self, app, port):
        self.app = pathlib.Path(app)
        self.port = port
        self.inputs = {str(self.app): sha256(self.app)}

    def run(self, page, directory, viewport):
        url = f"http://127.0.0.1:{self.port}/{PAGES[page]}"
        flag_dir = directory / "flags"
        flag_dir.mkdir()
        for name, value in flags(page).items():
            (flag_dir / name).write_text(value)
        if viewport:
            (flag_dir / "viewport").write_text(viewport)
        cpus = ",".join(str(cpu) for cpu in sorted(os.sched_getaffinity(0))[:GUEST_CPUS])
        argv = ["taskset", "-c", cpus, str(self.app), url]
        (directory / "execution.json").write_text(json.dumps({"argv": argv, "inputs": self.inputs}, indent=1))
        log = directory / "stderr.log"
        started = time.monotonic()
        environment = dict(os.environ, PENNY_FLAGS=str(flag_dir))
        try:
            with log.open("w") as stream:
                completed = subprocess.run(argv, env=environment, stdout=stream, stderr=subprocess.STDOUT,
                                           timeout=RUN_DEADLINE_SECONDS)
            text = log.read_text(errors="replace")
            outcome = "DONE" if "PENNY-BENCH: done" in text else f"EXIT-{completed.returncode}"
        except subprocess.TimeoutExpired:
            text = log.read_text(errors="replace")
            outcome = "TIMEOUT"
        if FAILURES.search(text):
            outcome = "FAIL"
        for name in FRAMES:
            frame = flag_dir / f"penny-{name}.ppm"
            if frame.exists():
                shutil.move(frame, directory / frame.name)
        return outcome, text, time.monotonic() - started


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=pathlib.Path, required=True)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--pages", default=",".join(PAGES))
    parser.add_argument("--platforms", default="cubit,hosted")
    parser.add_argument("--frames", action="store_true",
                        help="save top/keys/scrolled frames (PPM) per run, to compare rendering")
    parser.add_argument("--cubit-app", help="cubitshell.app to boot (default PENNY_APP or the staged build)")
    parser.add_argument("--hosted-app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                                    "x86_64-unknown-linux-gnu/release/cubitshell"))
    parser.add_argument("--alloc-timing", action="store_true",
                        help="time allocations of 1 MiB or more per input step (PENNY-ALLOC)")
    options = parser.parse_args()
    global SAVE_FRAMES, ALLOC_TIMING
    SAVE_FRAMES = options.frames
    ALLOC_TIMING = options.alloc_timing
    out = options.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    pages_dir = out / "pages"
    subprocess.run(["python3", str(HERE / "make_pages.py"), str(pages_dir)], check=True)
    server = serve(pages_dir)
    port = server.server_port
    platforms = options.platforms.split(",")
    work = pathlib.Path(tempfile.mkdtemp(prefix="penny-breakdown-", dir=out))
    runners = {}
    if "cubit" in platforms:
        runners["cubit"] = CuBit(work, port, options.cubit_app)
    if "hosted" in platforms:
        runners["hosted"] = Hosted(options.hosted_app, port)
    viewport = {}
    for run in range(options.runs):
        for page in options.pages.split(","):
            for name in platforms:
                directory = out / f"{page}-{name}-{run}"
                directory.mkdir()
                load_before = os.getloadavg()
                if name == "cubit":
                    outcome, text, seconds = runners[name].run(page, directory)
                else:
                    outcome, text, seconds = runners[name].run(page, directory, viewport.get(page))
                parsed = parse(text)
                if name == "cubit" and parsed["viewport"]:
                    viewport[page] = parsed["viewport"]
                record = {"page": page, "platform": name, "run": run, "outcome": outcome,
                          "wall_seconds": seconds, "host_load_before": load_before,
                          "host_load_after": os.getloadavg(), "host": platform.node(),
                          "parsed": parsed}
                (directory / "result.json").write_text(json.dumps(record, indent=1))
                print(f"{page} {name} run {run}: {outcome} {seconds:.1f}s viewport={parsed['viewport']}",
                      flush=True)
    server.shutdown()
    shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    main()
