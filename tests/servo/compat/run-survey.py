#!/usr/bin/env python3
"""WEB-001 compatibility survey: one command, every engine.

Inside the repository's Nix shell on the measurement host, with the survey
tools on PATH (tools.nix; run-survey.sh sets that up):

    run-survey.py --out DIR [--runs 3] [--engines servo,penny,firefox,chromium,wpe]
        [--sites id,id] [--groups priority,tranco] [--viewport WxH]

Engines, all with the same steps (plan.py) and the same in-page probe:
- servo: hosted cubitshell (build-cubitshell-hosted.sh), one process per
  site, the compat flag files in a private directory (PENNY_FLAGS).
- penny: cubitshell.app on CuBit under QEMU KVM, its network a virtio-net
  link to passt (L2, passt's DHCP and DNS, host sockets), not QEMU's
  user-mode stack. passt is the exact binary tools.nix pins (PASST), which
  the host's AppArmor profile lets sandbox itself. One VM works through the
  remaining sites; if Penny crashes or stalls, the VM is stopped, the site
  recorded, and a new VM carries on with the next site.
- firefox, chromium: drive_reference.py (Playwright).
- wpe: drive_wpe.py, stock WPE WebKit over WPEWebDriver, its web processes
  in WebKit's bubblewrap sandbox (checked every session).

Engines run at the same time, each on its own host CPUs (ENGINE_CPUS), runs
in sequence. Output: DIR/<engine>/run<N>/records.jsonl and shots/*.png;
analyze.py turns DIR into the table and gallery.
"""
import argparse
import json
import os
import pathlib
import re
import shutil
import socket as sockets
import subprocess
import sys
import threading
import time

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(HERE))
import plan  # noqa: E402

ENGINES = ("servo", "penny", "firefox", "chromium", "wpe")
# Host CPUs per engine: four each, as the guest gets four vCPUs; the guest's
# QEMU threads and passt get their own range.
ENGINE_CPUS = {"servo": "0-3", "firefox": "4-7", "chromium": "8-11", "wpe": "12-15",
               "penny": "16-23"}
GUEST_CPUS = 4
GUEST_MEMORY = "6G"
# A display this size gives Penny's window a 1278x726 viewport, the one
# every engine uses (DEFAULT_VIEWPORT).
GUEST_DISPLAY = ("1920", "1200")
DEFAULT_VIEWPORT = "1278x726"
# The development disk grows to this, room for every screenshot of a run.
GUEST_DISK_BYTES = 6 * 1024 ** 3
# Per site, beyond the plan's own waits: a hung Servo is stopped then.
SITE_MARGIN_SECONDS = 90
# A CuBit VM that prints no survey record for this long is stopped.
GUEST_STALL_SECONDS = 150
# Hosted Servo has this long to exit after its plan's last record.
SHUTDOWN_SECONDS = 10
GUEST_BOOT_SECONDS = 180
POLL_SECONDS = 0.5
RECORD = re.compile(r"PENNY-COMPAT (\S+) (\S+) (\S+) (\d+)/(\d+) ?(.*)")
PANIC = re.compile(r"CUBITSHELL: panic[^\n]*|USER-MEMORY-FAULT[^\n]*|PENNY-ABORT:[^\n]*|EXCEPTION:[^\n]*")
INIT = ('(startup v1 (start "logstore.svc") (start "clock.svc") '
        '(start "display.svc") (start "desktop.svc") '
        '(start "cubitshell.app" (network approve-declared)))')
# The development system's configured resolver (system.ccl net.dns.primary,
# QEMU user networking's forwarder); it takes precedence over the DHCP
# lease's, so passt answers that address with the host's resolver.
SYSTEM_DNS = "10.0.2.3"
STAGED_SERVICES = ("logstore.svc", "clock.svc", "display.svc", "desktop.svc")


def site_seconds(site):
    waits = sum(step.get("ms", 0) for step in plan.steps(site)) / 1000
    return plan.LOAD_DEADLINE_MS / 1000 + waits + SITE_MARGIN_SECONDS


def parse_log(text, engine, run):
    """cubitshell's PENNY-COMPAT lines as records (chunks joined)."""
    records, parts = [], {}
    for line in text.splitlines():
        match = RECORD.search(line)
        if not match:
            continue
        kind, site, name, index, count, chunk = match.groups()
        key = (kind, site, name)
        parts.setdefault(key, []).append(chunk.rstrip("\r"))
        if int(index) == int(count):
            records.append({"engine": engine, "run": run, "kind": kind, "site": site, "name": name,
                            "text": "".join(parts.pop(key))})
    return records


def ppm_to_png(ppm, png):
    from PIL import Image
    with Image.open(ppm) as image:
        image.save(png, optimize=False)


# --flag NAME=VALUE: more cubitshell flag files for both Servo engines
# (diagnostics, e.g. user-agent).
EXTRA_FLAGS = {}


def flag_files(directory, sites, viewport=None):
    files = {"compat": "", "log-level": "error", "compat-plan": plan.cubitshell_plan(sites),
             "compat-probe": plan.PROBE}
    if viewport:
        files["viewport"] = viewport
    files.update(EXTRA_FLAGS)
    for name, value in files.items():
        (directory / name).write_text(value)
    return files


class Servo:
    """Hosted cubitshell, one process per site."""

    def __init__(self, app, viewport):
        self.app = pathlib.Path(app)
        self.viewport = viewport

    @staticmethod
    def supervise(argv, flags, log, deadline):
        """The process's exit code, "timeout", or "shutdown-hang" when the
        plan finished but the process did not exit soon after."""
        with log.open("w") as handle:
            process = subprocess.Popen(argv, env=dict(os.environ, PENNY_FLAGS=str(flags)),
                                       stdout=handle, stderr=subprocess.STDOUT)
            started, finished_at = time.monotonic(), None
            while process.poll() is None:
                if finished_at is None and "PENNY-COMPAT done" in log.read_text(errors="replace"):
                    finished_at = time.monotonic()
                if finished_at is not None and time.monotonic() - finished_at > SHUTDOWN_SECONDS:
                    process.kill()
                    process.wait()
                    return "shutdown-hang"
                if time.monotonic() - started > deadline:
                    process.kill()
                    process.wait()
                    return "timeout"
                time.sleep(POLL_SECONDS)
            return process.returncode

    def run(self, sites, out, run):
        shots = out / "shots"
        shots.mkdir(parents=True, exist_ok=True)
        with (out / "records.jsonl").open("a") as stream:
            for site in sites:
                flags = out / "flags" / site["id"]
                flags.mkdir(parents=True, exist_ok=True)
                flag_files(flags, [site], self.viewport)
                argv = ["taskset", "-c", ENGINE_CPUS["servo"], str(self.app)]
                started = time.monotonic()
                log = flags / "stderr.log"
                outcome = self.supervise(argv, flags, log, site_seconds(site))
                text = log.read_text(errors="replace")
                records = parse_log(text, "servo", run)
                if outcome == "shutdown-hang":
                    # The plan finished; only Servo's shutdown hung.
                    records.append({"engine": "servo", "run": run, "kind": "shutdown-hang", "site": site["id"],
                                    "name": "-", "text": "no exit within the shutdown deadline after the plan"})
                elif outcome != 0 or not any(r["kind"] == "done" for r in records):
                    panic = PANIC.search(text)
                    records.append({"engine": "servo", "run": run, "kind": "crash", "site": site["id"],
                                    "name": "-", "text": f"exit {outcome}: " + (panic.group(0)[:300] if panic else "")})
                for ppm in flags.glob("compat-*.ppm"):
                    ppm_to_png(ppm, shots / (ppm.stem.removeprefix("compat-") + ".png"))
                    ppm.unlink()
                for record in records:
                    stream.write(json.dumps(record) + "\n")
                stream.flush()
                (flags / "compat-plan").unlink(missing_ok=True)
                print(f"servo run {run} {site['id']}: {outcome} {time.monotonic() - started:.1f}s", flush=True)


class Monitor:
    """QEMU's QMP socket: the run state, and human monitor commands."""

    def __init__(self, path):
        self.socket = sockets.socket(sockets.AF_UNIX, sockets.SOCK_STREAM)
        self.socket.connect(str(path))
        self.stream = self.socket.makefile("rw")
        self.events = []
        self.stream.readline()  # the greeting
        self.call("qmp_capabilities")

    def call(self, command, **arguments):
        self.stream.write(json.dumps({"execute": command, "arguments": arguments}) + "\n")
        self.stream.flush()
        while True:
            reply = json.loads(self.stream.readline())
            if "event" in reply:
                self.events.append(reply)
                continue
            return reply.get("return", reply.get("error"))

    def human(self, line, cpu=None):
        """One HMP command; each runs in a fresh monitor, so the vCPU it
        reads (registers, virtual addresses) is passed every time."""
        arguments = {"command-line": line}
        if cpu is not None:
            arguments["cpu-index"] = cpu
        return self.call("human-monitor-command", **arguments)

    def close(self):
        self.socket.close()


# The state saved when a guest stops on a reset (catch-reset mode).
RESET_DUMP_STACK_QWORDS = 128
RESET_DUMP_TABLE_QWORDS = 64
PAGE_TABLE_QWORDS = 512
# Kernel variables read (physically) from a stopped guest, by symbol:
# the console's output owner (a per-CPU GS base) and the panic flag.
KERNEL_IMAGE_BASE = 0xffffffff80000000
RESET_DUMP_SYMBOLS = ("textio__outputowner", "x86__panicked", "mem_mgr__kernelp4")
# Physical memory saved whole: the kernel image's reservation (its data,
# bss, IST stacks and the kernel log ring), for offline reading.
KERNEL_IMAGE_RESERVED_BYTES = 16 * 1024 ** 2


def kernel_symbols(kernel):
    """name -> address from the kernel image's symbol table."""
    listing = subprocess.run(["nm", str(kernel)], capture_output=True, text=True).stdout
    return {f[2]: int(f[0], 16) for f in (line.split() for line in listing.splitlines()) if len(f) == 3}


def dump_stopped_guest(monitor, directory, cpus, kernel=None):
    """Every vCPU's registers, the descriptor tables and the stack around
    RSP, written to directory/reset-state.txt."""
    out = [f"status: {monitor.call('query-status')}", f"events: {json.dumps(monitor.events)}",
           monitor.human("info cpus")]
    for cpu in range(cpus):
        registers = monitor.human("info registers", cpu)
        out += [f"===== vCPU {cpu}", registers]
        fields = dict(re.findall(r"\b(RSP|RIP|CR3)=([0-9a-f]+)", registers))
        tables = dict(re.findall(r"\b(GDT|IDT)=\s*([0-9a-f]+)", registers))
        if "RSP" in fields:
            rsp = int(fields["RSP"], 16)
            out += [f"--- stack at RSP-0x100 (vCPU {cpu})",
                    monitor.human(f"x/{RESET_DUMP_STACK_QWORDS}gx {rsp - 0x100:#x}", cpu)]
        if "RIP" in fields:
            out += [f"--- code at RIP (vCPU {cpu})", monitor.human(f"x/16i {int(fields['RIP'], 16):#x}", cpu)]
        if "CR3" in fields:
            # The top-level page table the vCPU ran on (physical).
            top = int(fields["CR3"], 16) & ~0xfff
            out += [f"--- PML4 at CR3 {top:#x} (vCPU {cpu})",
                    monitor.human(f"xp/{PAGE_TABLE_QWORDS}gx {top:#x}")]
        for name, base in tables.items():
            out += [f"--- {name} (vCPU {cpu})", monitor.human(f"x/{RESET_DUMP_TABLE_QWORDS}gx {int(base, 16):#x}", cpu)]
    symbols = kernel_symbols(kernel) if kernel else {}
    for name in RESET_DUMP_SYMBOLS:
        if name in symbols:
            physical = symbols[name] - KERNEL_IMAGE_BASE
            count = PAGE_TABLE_QWORDS if name == "mem_mgr__kernelp4" else 1
            out += [f"--- {name} (physical {physical:#x})", monitor.human(f"xp/{count}gx {physical:#x}")]
    monitor.call("pmemsave", val=0, size=KERNEL_IMAGE_RESERVED_BYTES,
                 filename=str(directory / "reset-kernel-image.bin"))
    out += [monitor.human("info pic"), "--- local APICs", *(monitor.human("info lapic", c) for c in range(cpus))]
    (directory / "reset-state.txt").write_text("\n".join(out) + "\n")


class Penny:
    """cubitshell.app on CuBit (QEMU KVM, passt networking)."""

    # catch-reset: the guest stops (not exits) on a reset, and its state is
    # saved before the VM is stopped (Penny's guest resets are a finding).
    catch_reset = False

    def __init__(self, app, work):
        self.stage = pathlib.Path(os.environ.get("PENNY_STAGE", ROOT / "kernel/isodir/boot"))
        self.app = pathlib.Path(app)
        self.work = work
        iso = work / "iso"
        (iso / "boot/grub").mkdir(parents=True, exist_ok=True)
        for name in ("cubit_kernel", "initrd.img"):
            shutil.copyfile(self.stage / name, iso / "boot" / name)
        (iso / "boot/grub/grub.cfg").write_text(
            'serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\nset default=0\n'
            'menuentry "Penny survey" {\n multiboot /boot/cubit_kernel\n'
            f' set gfxpayload={GUEST_DISPLAY[0]}x{GUEST_DISPLAY[1]}x32\n'
            ' module /boot/initrd.img init.img\n}\n')
        self.iso = work / "boot.iso"
        subprocess.run(["grub-mkrescue", "-o", str(self.iso), str(iso)], check=True, capture_output=True)

    def boot(self, sites, directory):
        """One VM over the given sites; returns its serial log."""
        directory.mkdir(parents=True, exist_ok=True)
        flags = directory / "flags"
        flags.mkdir(exist_ok=True)
        files = []
        for name in flag_files(flags, sites):
            files += ["--file", f"servo/{name}={flags / name}"]
        (directory / "init.ccl").write_text(INIT)
        (directory / "empty").write_text("")
        files += ["--file", f"init.ccl={directory / 'init.ccl'}",
                  "--file", f"Bookmarks/keep={directory / 'empty'}"]
        disk = directory / "desktop.img"
        subprocess.run(["python3", "tools/build_development_disk.py", str(disk), "--boot",
                        *[str(self.stage / n) for n in STAGED_SERVICES], str(self.app), *files],
                       cwd=ROOT, check=True, capture_output=True)
        shutil.rmtree(directory / "debug", ignore_errors=True)
        os.truncate(disk, GUEST_DISK_BYTES)
        subprocess.run(["resize2fs", "-f", str(disk)], check=True, capture_output=True)
        socket = directory / "passt.socket"
        passt = subprocess.Popen(["taskset", "-c", ENGINE_CPUS["penny"], os.environ["PASST"], "-f", "-q",
                                  "--dns-forward", SYSTEM_DNS, "-s", str(socket)],
                                 stdout=(directory / "passt.log").open("w"), stderr=subprocess.STDOUT)
        deadline = time.monotonic() + 10
        while not socket.exists() and time.monotonic() < deadline:
            time.sleep(0.05)
        serial = directory / "serial.log"
        argv = ["taskset", "-c", ENGINE_CPUS["penny"],
                "qemu-system-x86_64", "-accel", "kvm", "-cpu", "host", "-machine", "q35",
                "-smp", str(GUEST_CPUS), "-m", GUEST_MEMORY, "-cdrom", str(self.iso),
                "-display", "none", "-serial", f"file:{serial}", "-vga", "none",
                "-device", f"virtio-vga,xres={GUEST_DISPLAY[0]},yres={GUEST_DISPLAY[1]}",
                "-drive", f"file={disk},if=none,id=nvme0,format=raw",
                "-device", "nvme,serial=cubitnvme,drive=nvme0",
                "-netdev", f"stream,id=net0,server=off,addr.type=unix,addr.path={socket}",
                "-device", "virtio-net-pci,netdev=net0", "-no-reboot",
                "-d", "guest_errors", "-D", str(directory / "qemu-debug.log")]
        qmp = directory / "qmp.socket"
        if self.catch_reset:
            qmp.unlink(missing_ok=True)
            argv += ["-no-shutdown", "-qmp", f"unix:{qmp},server=on,wait=off"]
        (directory / "execution.json").write_text(json.dumps({"argv": argv}, indent=1))
        with (directory / "qemu.log").open("w") as log:
            vm = subprocess.Popen(argv, stdout=log, stderr=log)
        started = time.monotonic()
        last_size, last_change, outcome = 0, time.monotonic(), "stall"
        monitor = None
        if self.catch_reset:
            deadline = time.monotonic() + 10
            while not qmp.exists() and time.monotonic() < deadline:
                time.sleep(0.05)
            monitor = Monitor(qmp)
        try:
            while True:
                text = serial.read_text(errors="replace") if serial.exists() else ""
                records = text.count("PENNY-COMPAT ")
                if records != last_size:
                    last_size, last_change = records, time.monotonic()
                if "PENNY-COMPAT done" in text:
                    outcome = "done"
                    break
                if PANIC.search(text):
                    outcome = "crash"
                    time.sleep(2)
                    break
                if vm.poll() is not None:
                    outcome = "vm-exit"
                    break
                if monitor and monitor.call("query-status")["status"] in ("shutdown", "internal-error",
                                                                         "guest-panicked"):
                    outcome = "vm-exit"
                    dump_stopped_guest(monitor, directory, GUEST_CPUS, self.stage / "cubit_kernel")
                    break
                limit = GUEST_BOOT_SECONDS if records == 0 else GUEST_STALL_SECONDS
                if time.monotonic() - last_change > limit:
                    break
                time.sleep(POLL_SECONDS)
        finally:
            if monitor:
                monitor.close()
            vm.terminate()
            try:
                vm.wait(timeout=10)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
            passt.terminate()
            passt.wait()
        return outcome, serial.read_text(errors="replace"), disk, time.monotonic() - started

    def extract(self, disk, shots):
        listing = subprocess.run(["debugfs", "-R", "ls -p /Bookmarks", str(disk)],
                                 capture_output=True, text=True).stdout
        for line in listing.splitlines():
            # ls -p: /inode/mode/uid/gid/name/size/
            fields = line.strip().strip("/").split("/")
            if len(fields) < 5 or not fields[4].startswith("compat-"):
                continue
            name = fields[4]
            ppm = disk.parent / name
            subprocess.run(["debugfs", "-R", f"dump /Bookmarks/{name} {ppm}", str(disk)],
                           capture_output=True)
            if ppm.exists() and ppm.stat().st_size:
                try:
                    ppm_to_png(ppm, shots / (ppm.stem.removeprefix("compat-") + ".png"))
                except Exception as error:
                    print(f"penny: unreadable frame {ppm.name}: {error}", flush=True)
            ppm.unlink(missing_ok=True)

    def run(self, sites, out, run):
        shots = out / "shots"
        shots.mkdir(parents=True, exist_ok=True)
        remaining = list(sites)
        boot = 0
        with (out / "records.jsonl").open("a") as stream:
            while remaining:
                directory = out / f"boot{boot}"
                outcome, text, disk, seconds = self.boot(remaining, directory)
                records = parse_log(text, "penny", run)
                finished = {r["site"] for r in records if r["kind"] == "console" and r["name"] == "count"}
                if outcome != "done":
                    panic = PANIC.search(text)
                    # The site in flight: the first not finished, in plan order.
                    victim = next((s["id"] for s in remaining if s["id"] not in finished), None)
                    if victim is not None:
                        records.append({"engine": "penny", "run": run, "kind": "crash", "site": victim,
                                        "name": "-", "text": f"{outcome}: " + (panic.group(0)[:300] if panic else "")})
                        finished.add(victim)
                for record in records:
                    stream.write(json.dumps(record) + "\n")
                stream.flush()
                self.extract(disk, shots)
                disk.unlink(missing_ok=True)
                before = len(remaining)
                remaining = [s for s in remaining if s["id"] not in finished]
                if len(remaining) == before:
                    # Nothing finished (a boot failure): record it and skip one site.
                    remaining = remaining[1:]
                print(f"penny run {run} boot {boot}: {outcome} {seconds:.0f}s, "
                      f"{before - len(remaining)} sites, {len(remaining)} left", flush=True)
                boot += 1


class Reference:
    """A reference driver over the sites (drive_reference.py for Playwright's
    engines, drive_wpe.py for WPE). A driver can die (Playwright: an
    assertion in its Firefox WebSocket handling); it then restarts with the
    sites not yet finished, the interrupted one retried once. A WPE driver
    that finds a session unsandboxed stops the engine instead."""

    ATTEMPTS = 2
    DRIVERS = {"wpe": "drive_wpe.py"}

    def __init__(self, engine, viewport):
        self.engine = engine
        self.viewport = viewport
        self.driver = self.DRIVERS.get(engine, "drive_reference.py")

    def run(self, sites, out, run):
        remaining = [s["id"] for s in sites]
        attempts = {}
        records = out / "records.jsonl"
        while remaining:
            argv = ["taskset", "-c", ENGINE_CPUS[self.engine], sys.executable, "-I",
                    str(HERE / self.driver), "--engine", self.engine, "--run", str(run),
                    "--out", str(out), "--viewport", self.viewport, "--sites", ",".join(remaining),
                    "--sites-file", str(out.parent.parent / "sites.json")]
            with (out / "driver.log").open("a") as log:
                subprocess.run(argv, stdout=log, stderr=subprocess.STDOUT)
            finished = set()
            if records.exists():
                for line in records.read_text().splitlines():
                    record = json.loads(line)
                    if record["kind"] == "console" and record["name"] == "count":
                        finished.add(record["site"])
                    if record["kind"] == "sandbox-missing":
                        print(f"{self.engine}: stopped, not sandboxed: {record['text']}", flush=True)
                        return
            left = [s for s in remaining if s not in finished]
            if not left:
                break
            interrupted = left[0]
            attempts[interrupted] = attempts.get(interrupted, 0) + 1
            if attempts[interrupted] >= self.ATTEMPTS:
                with records.open("a") as stream:
                    stream.write(json.dumps({"engine": self.engine, "run": run, "kind": "driver-failed",
                                             "site": interrupted, "name": "-",
                                             "text": "the Playwright driver died on this site"}) + "\n")
                left = left[1:]
            remaining = left


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=pathlib.Path, required=True)
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--first-run", type=int, default=0,
                        help="number the runs from here (more runs of an engine in another directory)")
    parser.add_argument("--cpus", action="append", default=[], metavar="ENGINE=CPUS",
                        help="host CPUs for an engine instead of ENGINE_CPUS")
    parser.add_argument("--engines", default="servo,penny,firefox,chromium")
    parser.add_argument("--sites")
    parser.add_argument("--groups")
    parser.add_argument("--viewport", default=DEFAULT_VIEWPORT)
    parser.add_argument("--sites-file", default=str(HERE / "sites.json"))
    parser.add_argument("--hosted-app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                                    "x86_64-unknown-linux-gnu/release/cubitshell"))
    parser.add_argument("--cubit-app", default=str(ROOT / "userspace/rust/build/cubitshell.app"))
    parser.add_argument("--settle-ms", type=int, default=plan.SETTLE_MS,
                        help="the wait after load before the first record and screenshot")
    parser.add_argument("--flag", action="append", default=[], metavar="NAME=VALUE",
                        help="an extra cubitshell flag file for servo and penny (diagnostics)")
    parser.add_argument("--catch-reset", action="store_true",
                        help="penny: a guest reset stops the VM; its vCPU state goes to bootN/reset-state.txt")
    options = parser.parse_args()
    Penny.catch_reset = options.catch_reset
    plan.SETTLE_MS = options.settle_ms
    for choice in options.flag:
        name, value = choice.split("=", 1)
        EXTRA_FLAGS[name] = value
    out = options.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    data, sites = plan.load_sites(options.sites_file, only=set(options.sites.split(",")) if options.sites else None,
                                  groups=set(options.groups.split(",")) if options.groups else None)
    # The survey's own copy: the analysis reads the list that was run.
    if not (out / "sites.json").exists():
        (out / "sites.json").write_text(json.dumps(data, indent=1))
    for choice in options.cpus:
        engine, cpus = choice.split("=", 1)
        ENGINE_CPUS[engine] = cpus
    engines = options.engines.split(",")
    for engine in engines:
        if engine not in ENGINES:
            parser.error(f"unknown engine {engine}")
    # An engine added later to an existing survey (the same sites) joins it.
    previous = json.loads((out / "survey.json").read_text()) if (out / "survey.json").exists() else {}
    recorded = previous.get("engines", [])
    (out / "survey.json").write_text(json.dumps({
        "started": previous.get("started", time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())),
        "host": os.uname().nodename,
        "engines": recorded + [e for e in engines if e not in recorded], "runs": options.runs, "viewport": options.viewport,
        "sites": [s["id"] for s in sites], "load_deadline_ms": plan.LOAD_DEADLINE_MS,
        "settle_ms": plan.SETTLE_MS, "engine_cpus": ENGINE_CPUS,
        "hosted_app": options.hosted_app, "cubit_app": options.cubit_app}, indent=1))
    runners = {}
    for engine in engines:
        if engine == "servo":
            runners[engine] = Servo(options.hosted_app, options.viewport)
        elif engine == "penny":
            runners[engine] = Penny(options.cubit_app, out / "penny-work")
        else:
            runners[engine] = Reference(engine, options.viewport)

    def survey(engine):
        for run in range(options.first_run, options.first_run + options.runs):
            directory = out / engine / f"run{run}"
            directory.mkdir(parents=True, exist_ok=True)
            runners[engine].run(sites, directory, run)
            print(f"{engine} run {run} finished", flush=True)

    threads = [threading.Thread(target=survey, args=(engine,)) for engine in engines]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    print("SURVEY-DONE", flush=True)


if __name__ == "__main__":
    main()
