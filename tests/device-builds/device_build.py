#!/usr/bin/env python3
"""The device-build test's host side: the headless test (tests/headless/run.sh
device-build), the device-build live image (tests/usb-optical/run-live.py
--device-build) and its installed system (tests/install-reboot/run.sh).

Usage: device_build.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT BUILD_DIR
                       [--build-only] [--memory-report FILE] [--qemu-pid PID]

  --build-only      swap-probe's build 1 and hello's build only (pair, put,
                    build, SHA-256, download)
  --memory-report   while build 1 runs, sample (proc.list) on the device and
                    QEMU's resident memory; write the peaks as JSON to FILE
  --qemu-pid        QEMU's process, for its resident memory (VmRSS/VmHWM)

The guest's system volume (@system) holds the device-build payload
(tests/device-builds/build.sh): an installed root on QEMU's UFS disk
(make-disk.py), the live RAM root (@mem:0) or the installed root the
device-build live image's installer wrote: the CuBit toolchain at
/toolchain, the manifest step's tool at /tools/ccl-manifest.app, an empty
Storehouse at /storehouse (every step runs on CuBit), and the source subset
an app's build reads at /work/remote. Like a LAN host, this pairs with the
code the guest shows on its console and, with tools/cubit-remote:
  - puts swap-probe's sources (build 1) into the workspace;
  - builds it on CuBit (ccl-build.app, started by ccl-remote), following
    the job's output to its end;
  - stats the product and checks its SHA-256 against the Linux build of
    the same sources (BUILD_DIR/expected-build-1.sha256): reproducible;
  - downloads it and compares the bytes;
  - builds again, which must run no step (every step answered from the
    Storehouse);
  - puts build 2 (one source line changed) and checks that only the steps
    the change reaches run, and that the product is the Linux build 2's;
  - puts hello (userspace/apps/hello: an app, identity and manifest.ccl
    the device has never seen) and builds it, its manifest step too
    (ccl-manifest.app, on CuBit): the product, downloaded, is byte for byte
    the Linux build's (BUILD_DIR/expected-hello.*). With --build-only that
    is all.
Between and after the builds it swaps the running app (SELF-002 phase B,
cubit-remote app replace | rollback | status):
  - with the Desktop's "Allow app replacement" switch off (as at boot) a
    replace is refused (Switch_Off) and nothing starts;
  - the person turns the switch on: Super+N opens the status panel, and a
    pointer click (QMP's human monitor: PS/2 mouse moves and a button) on
    the row the Desktop says it drew; procmgr says staged launches are
    allowed;
  - replace with build 1: "swap-probe: build 1 running";
  - replace with build 2: "build 1 stopping", then "build 2 running";
    generation 2, previous generation 1 stopped in time;
  - rollback: "build 2 stopping", "build 1 running" again; status names
    generation 1 with build 1's SHA-256, previous generation 2;
  - replace hello with its build: generation 1 runs, and its hello line is
    in logstore ((logs.recent "com.cubit.hello")).
Then quits QEMU. Prints PASS/FAIL lines and the times; exits 1 on any failure.
"""
import argparse
import json
import os
import re
import socket
import struct
import sys
import tempfile
import threading
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "cubit-remote"))
import cubit_remote as cr  # noqa: E402

parser = argparse.ArgumentParser(description="the device-build test's host side")
parser.add_argument("serial")
parser.add_argument("qmp_socket")
parser.add_argument("timeout", type=float)
parser.add_argument("port", type=int)
parser.add_argument("build_dir", type=Path)
parser.add_argument("--build-only", action="store_true")
parser.add_argument("--memory-report", type=Path)
parser.add_argument("--qemu-pid", type=int)
options = parser.parse_args()
serial, qmp_socket, timeout, port, build_dir = (
    options.serial, options.qmp_socket, options.timeout, options.port, options.build_dir)
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
APP = "userspace/apps/swap-probe"
HELLO = "userspace/apps/hello"
HELLO_IDENTITY = "com.cubit.hello"
HELLO_LINE = "hello: hello from an app built on CuBit"
HELLO_LOG_SECONDS = 20
APP_SOURCES = ("component.ccl", "manifest.ccl", "main.adb")


def product_of(app):
    """Where the build exports an app's program, in the workspace."""
    return "out/%s/build/%s.app" % (app, app.rsplit("/", 1)[1])


PRODUCT = product_of(APP)
STATE_RUNNING, STATE_EXITED = 1, 2
STATUS_NOT_FOUND = 4
# Remote_Wire.App_Result, Instance_State, Stop_Kind.
APP_REPLACED, APP_ROLLED_BACK, APP_REPORTED = 0, 1, 2
INSTANCE_RUNNING = 1
STOP_STOPPED = 1
SESSION_SHOWN = b"desktop: remote session (process "
SWITCH_AT = re.compile(rb"desktop: status panel open at [0-9,]+ \(\d+x\d+\); "
                       rb"Allow app replacement switch at (\d+),(\d+)")
SWITCH_ALLOWED = b"procmgr: staged app launches allowed by the Desktop switch"
# PS/2 moves per monitor command, and enough of them to reach the corner.
MOUSE_STEP = 60
MOUSE_CORNER_STEPS = 60
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


def quit_qemu():
    try:
        with socket.socket(socket.AF_UNIX) as connection:
            connection.settimeout(3)
            connection.connect(qmp_socket)
            stream = connection.makefile("rwb")
            json.loads(stream.readline())
            for command in ("qmp_capabilities", "quit"):
                stream.write(json.dumps({"execute": command}).encode() + b"\n")
                stream.flush()
                stream.readline()
    except (OSError, ValueError) as error:
        print(f"device-build: could not quit QEMU: {error}", file=sys.stderr)


class Monitor:
    """QEMU's human monitor through QMP (sendkey, mouse_move, mouse_button)."""

    def __init__(self):
        self.connection = socket.socket(socket.AF_UNIX)
        self.connection.settimeout(5)
        self.connection.connect(qmp_socket)
        self.stream = self.connection.makefile("rwb")
        json.loads(self.stream.readline())
        self.command("qmp_capabilities")

    def command(self, name, arguments=None):
        item = {"execute": name}
        if arguments is not None:
            item["arguments"] = arguments
        self.stream.write(json.dumps(item).encode() + b"\n")
        self.stream.flush()
        while True:
            response = json.loads(self.stream.readline())
            if "error" in response:
                raise OSError(str(response["error"]))
            if "return" in response:
                return response["return"]

    def hmp(self, line):
        self.command("human-monitor-command", {"command-line": line})
        time.sleep(0.03)

    def click(self, x, y):
        """Pointer to the top left corner (the desktop clamps it), then to
        (x, y) in screen pixels, and a left click."""
        for _ in range(MOUSE_CORNER_STEPS):
            self.hmp("mouse_move %d %d" % (-MOUSE_STEP, -MOUSE_STEP))
        at = [0, 0]
        while at != [x, y]:
            dx, dy = [max(-MOUSE_STEP, min(MOUSE_STEP, b - a)) for a, b in zip(at, (x, y))]
            self.hmp("mouse_move %d %d" % (dx, dy))
            at = [at[0] + dx, at[1] + dy]
        time.sleep(0.2)
        self.hmp("mouse_button 1")
        time.sleep(0.1)
        self.hmp("mouse_button 0")

    def close(self):
        self.connection.close()


def serial_bytes():
    try:
        return Path(serial).read_bytes()
    except OSError:
        return b""


def pair():
    code = None
    while time.monotonic() < deadline:
        data = serial_bytes()
        found = CODE.search(data)
        if found and LISTENING in data:
            code = found.group(1).decode()
            break
        if b"CUBIT KERNEL PANIC" in data:
            break
        time.sleep(0.5)
    check(code is not None, "the guest showed a pairing code and listens")
    if code is None:
        return None
    host = cr.Host("guest", "127.0.0.1", port, cr.decode_token_text(code))
    for _ in range(20):
        try:
            bits, _, server = cr.hello(host)
            check(bits & 4 != 0, "hello: %s may build (%s)" % (server, cr.grant_names(bits)))
            return host
        except (OSError, cr.RemoteError):
            time.sleep(0.5)
    check(False, "hello with the pairing token")
    return None


def put_app(host, n, app=APP):
    """Put an app's sources (BUILD_DIR/app-N) into the workspace."""
    for name in APP_SOURCES:
        cr.put_file(host, "workspace", str(build_dir / ("app-%s" % n) / name), "%s/%s" % (app, name))


def build(host, label, app=APP):
    """Start a build of the app and follow it; (state, exit code, output, seconds)."""
    started = time.monotonic()
    job = struct.unpack(">I", host.call("build_start", "workspace",
                                        cr.name_field(app + "/component.ccl")))[0]
    offset, output, state, code = 0, b"", STATE_RUNNING, 0
    while time.monotonic() < deadline:
        state, code, total, first_kept, out = cr.build_poll(host, job, offset)
        if first_kept > offset:
            offset = first_kept
        output += out
        offset += len(out)
        if state != STATE_RUNNING and offset >= total:
            break
        if not out:
            time.sleep(0.5)
    seconds = time.monotonic() - started
    text = output.decode("utf-8", "replace")
    print("device-build: %s: job %d %s (exit code %d) in %.1f s; output:\n%s" % (
        label, job, cr.JOB_STATES.get(state, "?"), code, seconds, text), flush=True)
    return state, code, text, seconds


def wait_for_serial(marker, seconds=20, count=1):
    end = min(deadline, time.monotonic() + seconds)
    while serial_bytes().count(marker) < count and time.monotonic() < end:
        time.sleep(0.2)
    return serial_bytes().count(marker) >= count


def app(host, operation, name):
    """An app operation's report (dict), with 'refused' the App_Result's
    name when the device did not do it."""
    try:
        report = cr.app_call(host, operation, name)
        report["refused"] = None
    except cr.AppRefused as refused:
        report = refused.report
        report["refused"] = refused.name
    print("device-build: %s %s: %s" % (operation, name, report), flush=True)
    return report


def allow_app_replacement():
    """The person turns on Allow app replacement: Super+N opens the status
    panel; a click on the switch where the Desktop drew it."""
    check(wait_for_serial(SESSION_SHOWN, 60), "Desktop shows the remote session (its Remote access section)")
    monitor = Monitor()
    try:
        before = len(SWITCH_AT.findall(serial_bytes()))
        monitor.hmp("sendkey meta_l-n")
        end = time.monotonic() + 10
        found = []
        while time.monotonic() < end and len(found) <= before:
            found = SWITCH_AT.findall(serial_bytes())
            time.sleep(0.2)
        check(len(found) > before, "the status panel shows the Allow app replacement switch")
        if len(found) <= before:
            return
        x, y = int(found[-1][0]), int(found[-1][1])
        time.sleep(0.5)
        monitor.click(x, y)
        check(wait_for_serial(SWITCH_ALLOWED), "a click on the switch at %d,%d: procmgr allows staged launches" % (x, y))
        check(wait_for_serial(b"desktop: Allow app replacement on"), "the Desktop shows the switch on (procmgr confirmed)")
        monitor.hmp("sendkey esc")
    finally:
        monitor.close()


def ran_steps(text):
    return [line for line in text.splitlines() if line.startswith("ran ")]


# (proc.list)'s Process records (CCL.Interfaces.Processes): pid, name,
# identity, state, memory (bytes), launcher, age.
PROCESS = re.compile(r'\(Process (\d+) "([^"]*)" "[^"]*" Run_State\.\w+ (\d+) -?\d+ \d+\)')
MEMORY_SAMPLE_SECONDS = 1.0
KIB = 1024


def qemu_memory_kib(pid):
    """QEMU's resident set and its peak (KiB): the guest RAM it has touched."""
    found = {}
    try:
        for line in Path("/proc/%d/status" % pid).read_text().splitlines():
            key, _, value = line.partition(":")
            if key in ("VmRSS", "VmHWM"):
                found[key] = int(value.split()[0])
    except (OSError, ValueError):
        pass
    return found


class MemorySampler(threading.Thread):
    """Samples (proc.list) on the device while a build runs: the sum of
    every process's memory and the largest process, at their peaks."""

    def __init__(self, host):
        super().__init__(daemon=True)
        self.host = host
        self.stop = threading.Event()
        self.samples = 0
        self.peak_total = 0
        self.peak_process = (0, "")
        self.peak_processes = 0

    def run(self):
        while not self.stop.is_set():
            try:
                ok, text = cr.evaluate_whole(self.host, "(proc.list)")
            except (OSError, cr.RemoteError, SystemExit):
                ok, text = False, ""
            listed = PROCESS.findall(text) if ok else []
            if listed:
                self.samples += 1
                total = sum(int(memory) for _, _, memory in listed)
                if total > self.peak_total:
                    self.peak_total, self.peak_processes = total, len(listed)
                for _, name, memory in listed:
                    if int(memory) > self.peak_process[0]:
                        self.peak_process = (int(memory), name)
            self.stop.wait(MEMORY_SAMPLE_SECONDS)


def report_memory(sampler, before, seconds):
    after = qemu_memory_kib(options.qemu_pid) if options.qemu_pid else {}
    report = {
        "build_seconds": round(seconds, 1),
        "proc_list_samples": sampler.samples,
        "peak_sum_of_process_memory_bytes": sampler.peak_total,
        "processes_at_that_peak": sampler.peak_processes,
        "peak_single_process_bytes": sampler.peak_process[0],
        "peak_single_process": sampler.peak_process[1],
        "qemu_rss_kib_before_build": before.get("VmRSS"),
        "qemu_rss_kib_after_build": after.get("VmRSS"),
        "qemu_peak_rss_kib": after.get("VmHWM"),
    }
    options.memory_report.write_text(json.dumps(report, indent=2) + "\n")
    print("device-build: MEMORY %s" % json.dumps(report), flush=True)
    check(sampler.samples > 0, "memory: %d (proc.list) samples during build 1" % sampler.samples)


def main():
    host = pair()
    if host is None:
        return
    expected = {n: (build_dir / ("expected-build-%d.sha256" % n)).read_text().strip() for n in (1, 2)}

    put_app(host, 1)
    sampler = None
    if options.memory_report:
        before = qemu_memory_kib(options.qemu_pid) if options.qemu_pid else {}
        sampler = MemorySampler(host)
        sampler.start()
    state, code, text, seconds = build(host, "build 1")
    if sampler is not None:
        sampler.stop.set()
        sampler.join()
        report_memory(sampler, before, seconds)
    check(state == STATE_EXITED and code == 0, "build 1 on CuBit exits 0")
    check(len(ran_steps(text)) > 0, "build 1 ran %d steps on CuBit" % len(ran_steps(text)))
    check(any(manifest_step(APP) in s for s in ran_steps(text)),
          "build 1's manifest step ran on CuBit (no Storehouse seed)")
    check_product(host, APP, "build-1", "build 1")
    print("device-build: TIME build-1 %.1f s" % seconds, flush=True)
    if options.build_only:
        build_hello(host)
        return

    # Phase B: the app swapped for its build, gated by the Desktop's switch.
    report = app(host, "app_replace", PRODUCT)
    check(report["refused"] == "Switch_Off" and not report["current"],
          "with Allow app replacement off (as at boot) a replace is refused: %s" % report["refused"])
    check(b"swap-probe: build 1 running" not in serial_bytes(), "nothing started while the switch is off")
    check(wait_for_serial(b"ccl-remote: host app replace swap-probe refused: Allow app replacement is off"),
          "the refusal is an activity line on the device")
    allow_app_replacement()
    report = app(host, "app_replace", PRODUCT)
    check(report["refused"] is None and report["result"] == APP_REPLACED and report["current"] == 1
          and report["instance"] == INSTANCE_RUNNING and report["current_sha256"] == expected[1],
          "replace with build 1: generation 1 runs, its SHA-256 the Linux build's")
    check(wait_for_serial(b"swap-probe: build 1 running"), "the device shows \"swap-probe: build 1 running\"")

    state, code, text, seconds = build(host, "build 1 again")
    check(state == STATE_EXITED and code == 0 and not ran_steps(text),
          "a rebuild runs no step (all from the Storehouse)")
    print("device-build: TIME rebuild %.1f s" % seconds, flush=True)

    put_app(host, 2)
    state, code, text, seconds = build(host, "build 2")
    steps = ran_steps(text)
    check(state == STATE_EXITED and code == 0, "build 2 (main.adb changed) exits 0")
    check(steps and not any("manifest" in s for s in steps),
          "build 2 reruns only what the change reaches (%d steps, not the manifest)" % len(steps))
    size, digest = cr.remote_stat(host, "workspace", PRODUCT)
    check(digest.hex() == expected[2], "build 2's product is the Linux build 2's: sha256 %s" % digest.hex())
    print("device-build: TIME build-2 %.1f s" % seconds, flush=True)

    started = time.monotonic()
    report = app(host, "app_replace", PRODUCT)
    check(report["refused"] is None and report["result"] == APP_REPLACED and report["current"] == 2
          and report["previous"] == 1 and report["stop"] == STOP_STOPPED
          and report["current_sha256"] == expected[2] and report["previous_sha256"] == expected[1],
          "replace with build 2: generation 2 runs, generation 1 stopped in time")
    check(wait_for_serial(b"swap-probe: build 1 stopping") and wait_for_serial(b"swap-probe: build 2 running"),
          "the device shows build 1 stopping, then build 2 running")
    data = serial_bytes()
    check(0 <= data.find(b"swap-probe: build 1 stopping") < data.find(b"swap-probe: build 2 running"),
          "build 1 stopped before build 2 started")
    print("device-build: TIME replace %.1f s" % (time.monotonic() - started), flush=True)

    report = app(host, "app_rollback", "swap-probe")
    check(report["refused"] is None and report["result"] == APP_ROLLED_BACK and report["current"] == 1
          and report["previous"] == 2 and report["instance"] == INSTANCE_RUNNING,
          "rollback: generation 1 runs again, generation 2 previous")
    check(wait_for_serial(b"swap-probe: build 2 stopping") and
          wait_for_serial(b"swap-probe: build 1 running", count=2),
          "the device shows build 2 stopping and build 1 running again")
    report = app(host, "app_status", "swap-probe")
    check(report["result"] == APP_REPORTED and report["current"] == 1 and report["previous"] == 2
          and report["current_sha256"] == expected[1] and report["instance"] == INSTANCE_RUNNING,
          "status: the state names generation 1 (build 1's SHA-256), running")
    for op in (b"replace", b"rollback", b"status"):
        check(wait_for_serial(b"ccl-remote: host app " + op), "app %s is an activity line on the device" % op.decode())

    # A new app, its manifest built on the device, swapped in and heard from.
    build_hello(host)
    report = app(host, "app_replace", product_of(HELLO))
    expected_hello = (build_dir / "expected-hello.sha256").read_text().strip()
    check(report["refused"] is None and report["result"] == APP_REPLACED and report["current"] == 1
          and report["instance"] == INSTANCE_RUNNING and report["current_sha256"] == expected_hello,
          "replace hello with its build: generation 1 runs, its SHA-256 the Linux build's")
    check(wait_for_serial(HELLO_LINE.encode()), "the device shows \"%s\"" % HELLO_LINE)
    expression = '(logs.recent "%s")' % HELLO_IDENTITY
    end = min(deadline, time.monotonic() + HELLO_LOG_SECONDS)
    found, text = False, ""
    while not found and time.monotonic() < end:
        ok, text = cr.evaluate_whole(host, expression)
        found = ok and HELLO_LINE in text
        if not found:
            time.sleep(0.5)
    check(found, "logstore holds hello's line: %s -> %s" % (expression, text[:200]))


def manifest_step(app):
    """The label of an app's manifest step in ccl-build's "ran" lines."""
    return "manifest %s/manifest.ccl" % app


def check_product(host, app, expected_name, label):
    """The product's SHA-256 (stat on the device) and its downloaded bytes
    against the Linux build's (BUILD_DIR/expected-NAME.sha256, .app)."""
    product = product_of(app)
    expected = (build_dir / ("expected-%s.sha256" % expected_name)).read_text().strip()
    try:
        size, digest = cr.remote_stat(host, "workspace", product)
    except cr.RemoteError as e:
        size, digest = 0, b""
        check(False, "stat %s: %s" % (product, e))
    check(digest.hex() == expected,
          "%s's %s (%d bytes) is the Linux build's: sha256 %s" % (label, Path(product).name, size, digest.hex()))
    work = tempfile.mkdtemp(prefix="cubit-device-build-")
    local = os.path.join(work, Path(product).name)
    cr.get_file(host, "workspace", product, local)
    check(Path(local).read_bytes() == (build_dir / ("expected-%s.app" % expected_name)).read_bytes(),
          "%s: the downloaded product is byte for byte the Linux build" % label)
    os.remove(local)
    os.rmdir(work)


def build_hello(host):
    """hello, built entirely on the device: its manifest step runs there."""
    put_app(host, "hello", HELLO)
    state, code, text, seconds = build(host, "hello", HELLO)
    steps = ran_steps(text)
    check(state == STATE_EXITED and code == 0, "hello (a new app and manifest) builds on CuBit, exit 0")
    check(any(manifest_step(HELLO) in s for s in steps),
          "hello's manifest step ran on CuBit (ccl-manifest.app): %d steps" % len(steps))
    check_product(host, HELLO, "hello", "hello")
    print("device-build: TIME hello %.1f s" % seconds, flush=True)


try:
    main()
finally:
    if os.environ.get("DEVICE_BUILD_KEEP_RUNNING") != "1":
        quit_qemu()
print("DEVICE-BUILD: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
