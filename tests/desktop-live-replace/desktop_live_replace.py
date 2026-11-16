#!/usr/bin/env python3
"""The desktop-live-replace test's host side (tests/headless/run.sh --test
desktop-live-replace): desktop.svc replaced live through system
generations (LIVE-002), as the laptop's live and installed plans allow.

Usage: desktop_live_replace.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT DESKTOP_SVC TIMESYNC_SVC BOOT_DIR

The guest runs desktop.svc with (restart on-failure) (replace graceful)
(tests/desktop-live-replace/init.ccl). Like a LAN host this pairs with the
code on the guest's console and, with tools/cubit-remote:
  - puts programs made from the built desktop.svc into the workspace:
      desktop-good.svc   desktop.svc with 16 bytes appended (another build
                         stamp, so another content address)
      desktop-crash.svc  its entry instruction ud2: it faults at once
  - the person turns "Allow app replacement" on (a click);
  - no stop grace: refused (a graceful entry needs one);
  - desktop-good: Stop sent; Desktop hands its output leases and buffers
    back to Display and exits 0; procmgr starts desktop-good from the
    storehouse as the same entry; it registers and presents a frame again
    (the display comes back): Activated;
  - desktop-crash: it ends inside its window: Rolled_Back (Exited_Early);
    desktop-good runs again and presents again;
  - system rollback until the image's generation (0) is current; the
    image's desktop presents again.
  - timesync (replace transparent): timesync-good activated; the running
    timesync read Stop and exited 0 (procmgr did not have to end it); then
    rolled back to the image's program the same way.
Then, over the whole run: every Desktop that exited on its own exited 0,
every Stop was followed by the "display released" line, Display never
reported a lease taken from a live owner, and (proc.list) shows one
com.cubit.desktop. Prints PASS/FAIL lines; exits 1 on any failure.
"""
import hashlib
import json
import os
import re
import socket
import struct
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "cubit-remote"))
import cubit_remote as cr  # noqa: E402

serial, qmp_socket, timeout, port, desktop_svc, timesync_svc, boot_dir = (
    sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4]), Path(sys.argv[5]), Path(sys.argv[6]),
    Path(sys.argv[7]))
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
SESSION_SHOWN = b"desktop: remote session (process "
SWITCH_AT = re.compile(rb"desktop: status panel open at [0-9,]+ \(\d+x\d+\); "
                       rb"Allow app replacement switch at (\d+),(\d+)")
SWITCH_ALLOWED = b"procmgr: staged app launches allowed by the Desktop switch"
# One per Desktop instance: its first frame reached the screen.
PRESENTED = b"desktop: asynchronous frame released"
STOP_TAKEN = b"desktop: stop requested; releasing the display"
STOPPED = b"desktop: stopped; display released, exiting 0"
SERVICE = "desktop.svc"
# Explicit deadlines (milliseconds). The window covers Desktop's startup on
# QEMU (display info, bootstrap frame, renderer selection, registration);
# procmgr judges at its end.
HEALTH_MS = 20_000
STOP_GRACE_MS = 10_000
STOREHOUSE = "@system/storehouse/executables/"
MOUSE_STEP = 60
MOUSE_CORNER_STEPS = 60
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


def serial_bytes():
    try:
        return Path(serial).read_bytes()
    except OSError:
        return b""


def wait_for_serial(marker, seconds=20, count=1):
    end = min(deadline, time.monotonic() + seconds)
    while serial_bytes().count(marker) < count and time.monotonic() < end:
        time.sleep(0.2)
    return serial_bytes().count(marker) >= count


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


def quit_qemu():
    try:
        monitor = Monitor()
        try:
            monitor.command("quit")
        finally:
            monitor.close()
    except (OSError, ValueError) as error:
        print(f"desktop-live-replace: could not quit QEMU: {error}", file=sys.stderr)


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


def allow_app_replacement():
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
        check(wait_for_serial(SWITCH_ALLOWED), "a click on the switch: procmgr allows staged launches")
        monitor.hmp("sendkey esc")
    finally:
        monitor.close()


def entry_file_offset(image):
    """Where the ELF64 entry point's instruction is in the file."""
    entry, = struct.unpack_from("<Q", image, 24)
    phoff, = struct.unpack_from("<Q", image, 32)
    phentsize, phnum = struct.unpack_from("<HH", image, 54)
    for i in range(phnum):
        p_type, _, p_offset, p_vaddr, _, p_filesz = struct.unpack_from("<IIQQQQ", image, phoff + i * phentsize)
        if p_type == 1 and p_vaddr <= entry < p_vaddr + p_filesz:
            return p_offset + entry - p_vaddr
    raise SystemExit("desktop-live-replace: no loadable entry point")


def fixtures(work):
    desktop = desktop_svc.read_bytes()
    at = entry_file_offset(desktop)
    crash = bytearray(desktop)
    crash[at:at + 2] = b"\x0f\x0b"   # ud2: an invalid-opcode fault at once
    paths = {}
    for name, data in (("desktop-good.svc", desktop + b"desk-live-repl-1"),
                       ("desktop-crash.svc", bytes(crash))):
        path = os.path.join(work, name)
        Path(path).write_bytes(data)
        paths[name] = (path, hashlib.sha256(data).hexdigest())
    return paths


def activate(host, product, grace=STOP_GRACE_MS, service=SERVICE):
    try:
        report = cr.system_activate(host, service, product, HEALTH_MS, grace)
        report["refused"] = None
    except cr.GenerationRefused as refused:
        report = refused.report
        report["refused"] = refused.name
    print("desktop-live-replace: activate %s: %s (%s)" % (product, report, cr.generation_image(report)),
          flush=True)
    return report


APP_REPLACED, APP_START_FAILED = 0, 4
NOT_GRANTED = 5
METRICSVIEW_PRODUCT = "metricsview/metricsview.app"
METRICSVIEW_SHOWN = b"metricsview: first frame presented"
# What metricsview publishes through its com.cubit-os.stdmetric outlet each
# frame, read back from metrics.svc.
METRICSVIEW_SERIES = "metricsview.render_us"
METRICS_SECONDS = 15
# ccl-remote holds, DEVELOPMENT-ONLY, what the shipped apps ask for
# (its manifest), and names it in (hello) and at start.
DEVELOPMENT = "development-only authority for app replace: services "
# An attenuated launcher never passes on a render request (procmgr's
# launcherHolds counts services only): teapots' swap stays refused, and
# says why on the wire and in both logs.
TEAPOTS_PRODUCT = "refused/teapots.app"
TEAPOTS_REFUSAL = b'refused: it requests "render", which its launcher does not hold'
TEAPOTS_WHY = (b"ccl-remote: app teapots not started: teapots the new generation did not start "
               b"(procmgr: it needs authority its launcher does not hold, failure 5)")
# Every app the laptop images ship (images/laptop-install.ccl; cubitshell is
# not built), with the development-only services each needs ccl-remote to
# hold (its manifest's block). ccl-remote holds an optional service only
# while its provider runs at its start ("(absent)" in hello otherwise): an
# app starts when swapped in exactly when none it needs is absent; teapots
# (render) never. The observer roles the viewers need (log-observer,
# process-observer, log-control, series) ccl-remote holds by its plan entry
# and passes on, as procmgr passes any role a launcher holds.
OBSERVERS_PASSED = "observer roles passed on log-observer, process-observer, series, log-control;"
SHIPPED = {
    "agent": ["llm", "secrets"],
    "bible": [],
    "boot-logs": [],
    "calculator": [],
    "ccl-console": ["log-control", "lexicon", "installer", "wifi", "power", "llm", "secrets"],
    "ccl-workbench": ["log-control", "lexicon"],
    "config-inspector": [],
    "devices": ["device-manager"],
    "dictionary": ["lexicon"],
    "files": [],
    "gpu-viewer": ["gpu-probe", "device-manager"],
    "logs": ["log-control"],
    "metricsview": ["stdmetric"],
    "sameboy": ["mixer"],
    "text": [],
    "trace-viewer": [],
}


def absent_services(line):
    """The services hello names "(absent)"."""
    services = line[len(DEVELOPMENT):].split(";")[0]
    return {item.split(" (absent)")[0].strip() for item in services.split(",") if "(absent)" in item}


def app_replace(host, product, when):
    try:
        report = cr.app_call(host, "app_replace", product)
        report["refused"] = None
    except cr.AppRefused as refused:
        report = refused.report
        report["refused"] = refused.name
        print("desktop-live-replace: app replace %s (%s) refused: %s" % (product, when, refused), flush=True)
    print("desktop-live-replace: app replace %s (%s): %s" % (product, when, report), flush=True)
    return report


def series_published(host, name):
    end = time.monotonic() + METRICS_SECONDS
    text = ""
    while time.monotonic() < end:
        ok, text = cr.evaluate_whole(host, cr.series_expression(name, ""))
        if ok and name in text:
            return True, text
        time.sleep(0.5)
    return False, text


def replace_apps(host, when):
    """cubit-remote app replace, as on the laptop: the built metricsview
    starts, shows its window and publishes; teapots is refused, and the
    refusal says why on the host and on the device."""
    shown = serial_bytes().count(METRICSVIEW_SHOWN)
    report = app_replace(host, METRICSVIEW_PRODUCT, when)
    check(report["refused"] is None and report["result"] == APP_REPLACED and report["current"] > 0,
          "%s: app replace starts metricsview: generation %d (%s)" % (when, report["current"], report["refused"]))
    check(wait_for_serial(METRICSVIEW_SHOWN, 30, shown + 1), "%s: the swapped-in metricsview shows its window" % when)
    found, text = series_published(host, METRICSVIEW_SERIES)
    check(found, "%s: it publishes through its stdmetric outlet: %s in metrics.svc (%s)"
          % (when, METRICSVIEW_SERIES, text[:160]))
    refusals = serial_bytes().count(TEAPOTS_REFUSAL)
    said = serial_bytes().count(TEAPOTS_WHY)
    report = app_replace(host, TEAPOTS_PRODUCT, when)
    check(report["refused"] == "Start_Failed" and report["result"] == APP_START_FAILED
          and report["failure"] == NOT_GRANTED,
          "%s: teapots' replace is Start_Failed, procmgr's reason on the wire: Not_Granted (%s, failure %d)"
          % (when, report["refused"], report["failure"]))
    check(wait_for_serial(TEAPOTS_REFUSAL, 10, refusals + 1),
          "%s: procmgr's log line names the request ccl-remote cannot pass on" % when)
    check(wait_for_serial(TEAPOTS_WHY, 10, said + 1), "%s: ccl-remote's log line says why" % when)


def survey(host, absent):
    """Every shipped app swapped in from the workspace."""
    for app, needs in sorted(SHIPPED.items()):
        missing = sorted(set(needs) & absent)
        report = app_replace(host, "shipped/%s.app" % app, "survey")
        if not missing:
            check(report["refused"] is None and report["result"] == APP_REPLACED,
                  "survey: %s starts when swapped in (%s)" % (app, report["refused"]))
        else:
            check(report["refused"] == "Start_Failed" and report["failure"] == NOT_GRANTED,
                  "survey: %s needs %s, absent in this plan: refused, Not_Granted (%s)"
                  % (app, ", ".join(missing), report["refused"]))


def desktop_processes(host):
    ok, text = cr.evaluate_whole(host, "(proc.list)")
    return len(re.findall(r'\(Process \d+ "[^"]*" "com\.cubit\.desktop"', text)) if ok else -1


def main():
    host = pair()
    if host is None:
        return
    check(wait_for_serial(PRESENTED, 90), "the image's Desktop presented its first frame")
    work = tempfile.mkdtemp(prefix="cubit-desktop-live-replace-")
    paths = fixtures(work)
    timesync_good = os.path.join(work, "timesync-good.svc")
    Path(timesync_good).write_bytes(timesync_svc.read_bytes() + b"time-live-repl-1")
    paths["timesync-good.svc"] = (timesync_good, hashlib.sha256(Path(timesync_good).read_bytes()).hexdigest())
    for name, (path, _) in paths.items():
        cr.put_file(host, "workspace", path, "desktop/" + name)
    cr.put_file(host, "workspace", str(boot_dir / "metricsview.app"), METRICSVIEW_PRODUCT)
    cr.put_file(host, "workspace", str(boot_dir / "teapots.app"), TEAPOTS_PRODUCT)
    _, _, _, more = cr.hello_full(host)
    development = [line for line in more if line.startswith(DEVELOPMENT)]
    check(development and "stdmetric;" in development[0] and "lexicon," in development[0]
          and "@nvme:0/ (read write create)" in development[0] and OBSERVERS_PASSED in development[0],
          "hello names ccl-remote's development-only authority: %s" % more)
    absent = absent_services(development[0]) if development else set(SHIPPED)
    check(wait_for_serial(b"ccl-remote: " + DEVELOPMENT.encode(), 1),
          "ccl-remote's start names its development-only authority")
    good_hash = paths["desktop-good.svc"][1]
    good_program = (STOREHOUSE + good_hash).encode()
    presented = 1
    stops = 0

    allow_app_replacement()
    # An app swapped on a fresh boot (the image's Desktop).
    replace_apps(host, "fresh boot")

    report = activate(host, "desktop/desktop-good.svc", grace=0)
    check(report["refused"] == "Refused" and report["detail"] == 1 and report["current"] == 0,
          "desktop.svc is (replace graceful): an activation with no stop grace is refused")

    # Ready: Stop obeyed, the display handed back, exit 0; the new program
    # takes the display and presents.
    started = time.monotonic()
    report = activate(host, "desktop/desktop-good.svc")
    good_generation = report["current"]
    check(report["refused"] is None and report["result_name"] == "Activated" and good_generation > 0
          and report["pin_sha256"] == good_hash and report["running"] != 0,
          "desktop pinned to desktop-good: generation %d activated" % good_generation)
    print("desktop-live-replace: TIME switch %.1f s" % (time.monotonic() - started), flush=True)
    stops += 1
    presented += 1
    check(wait_for_serial(b"procmgr: switch desktop.svc: Stop sent to pid"),
          "procmgr sent the running desktop.svc Stop")
    check(wait_for_serial(STOP_TAKEN, 10, stops), "Desktop read the Stop")
    check(wait_for_serial(STOPPED, 10, stops), "Desktop handed the display back and exited 0")
    check(wait_for_serial(b"procmgr: switch desktop.svc switched; it runs " + good_program),
          "procmgr runs desktop from the storehouse by content address")
    check(wait_for_serial(PRESENTED, 30, presented), "the new Desktop presents: the display came back")
    # An app swapped once the Desktop itself was replaced live (procmgr
    # keeps the switch's state; the new Desktop reads it).
    replace_apps(host, "after the live Desktop replace")

    # A program that faults at once: rolled back to desktop-good.
    report = activate(host, "desktop/desktop-crash.svc")
    stops += 1
    presented += 1
    check(report["refused"] == "Rolled_Back" and report["detail"] == 2 and report["current"] == good_generation,
          "desktop-crash ended in its window: rolled back, generation %d stays" % good_generation)
    check(wait_for_serial(STOPPED, 10, stops), "the running Desktop exited 0 before the crash ran")
    check(wait_for_serial(b"procmgr: switch desktop.svc rolled back (the program ended inside its health window);"
                          b" it runs " + good_program), "procmgr runs desktop-good again")
    check(wait_for_serial(PRESENTED, 30, presented), "desktop-good presents again after the rollback")

    # Back to the image's program (generation 0), one rollback at a time.
    for _ in range(3):
        report = cr.system_rollback(host, HEALTH_MS, STOP_GRACE_MS)
        print("desktop-live-replace: rollback: %s (%s)" % (report, cr.generation_image(report)), flush=True)
        if report["result_name"] != "Activated":
            break
        stops += 1
        presented += 1
        if report["current"] == 0:
            break
    check(report["result_name"] == "Activated" and report["current"] == 0 and report["running"] != 0,
          "system rollback: the image's generation (0) is current again")
    check(wait_for_serial(b"procmgr: switch desktop.svc switched; it runs desktop.svc pid"),
          "procmgr runs desktop.svc's image program again")
    check(wait_for_serial(PRESENTED, 30, presented), "the image's Desktop presents again")
    time.sleep(2)

    # timesync (replace transparent): Stop read between polls, exit 0.
    timesync_exits = serial_bytes().count(b"timesync: stop requested; exiting")
    report = activate(host, "desktop/timesync-good.svc", service="timesync.svc")
    check(report["refused"] is None and report["result_name"] == "Activated"
          and report["pin_sha256"] == paths["timesync-good.svc"][1],
          "timesync pinned to timesync-good: activated")
    check(wait_for_serial(b"timesync: stop requested; exiting", 10, timesync_exits + 1),
          "the running timesync read Stop and exited")
    report = cr.system_rollback(host, HEALTH_MS, STOP_GRACE_MS)
    print("desktop-live-replace: timesync rollback: %s" % report, flush=True)
    check(report["result_name"] == "Activated", "timesync rolled back to the image's program")
    check(wait_for_serial(b"timesync: stop requested; exiting", 10, timesync_exits + 2),
          "timesync-good read Stop and exited too")
    time.sleep(2)

    # Every shipped app, swapped in from the workspace.
    for app in sorted(SHIPPED):
        cr.put_file(host, "workspace", str(boot_dir / (app + ".app")), "shipped/%s.app" % app)
    survey(host, absent)

    data = serial_bytes()
    check(data.count(STOPPED) == data.count(STOP_TAKEN) >= stops,
          "every Stop Desktop read (%d) ended with the display handed back" % data.count(STOP_TAKEN))
    check(not any(b"com.cubit.desktop pid" in line and b"exited code" in line and b"exited code 0" not in line
                  for line in data.splitlines()),
          "every desktop.svc that exited on its own exited 0")
    check(b"desktop: exiting (code" not in data, "no Desktop took a failure exit")
    check(not any(b"com.cubit.timesync pid" in line and b"exited code" in line and b"exited code 0" not in line
                  for line in data.splitlines()) and b"procmgr: switch timesync.svc: ending" not in data,
          "every timesync exited 0 on its own; procmgr ended none")
    check(b"procmgr: restarting desktop.svc" not in data,
          "Desktop never restarted on failure: the switches own the entry")
    check(desktop_processes(host) == 1, "(proc.list) shows exactly one com.cubit.desktop")
    for name, (path, _) in paths.items():
        os.remove(path)
    os.rmdir(work)


try:
    main()
finally:
    if os.environ.get("DESKTOP_LIVE_REPLACE_KEEP_RUNNING") != "1":
        quit_qemu()
print("DESKTOP-LIVE-REPLACE: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
