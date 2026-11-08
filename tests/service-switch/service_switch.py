#!/usr/bin/env python3
"""The service-switch test's host side (tests/headless/run.sh --test
service-switch): system generations switch running services and roll back.

Usage: service_switch.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT LEXICON_SVC CLOCK_SVC

The guest runs lexicon.svc with (replace transparent), clock.svc with
(replace graceful), and ccl-remote with builds granted. Like a LAN host
this pairs with the code on the guest's console and, with tools/cubit-remote's
system operations:
  - puts programs into the workspace, made here from the image's programs
    (no source change needed to get another content address):
      lexicon-good.svc   lexicon.svc with 16 bytes appended: another SHA-256
                         (another pin), the same manifest, runs alike
      lexicon-crash.svc  lexicon.svc whose entry instruction is ud2: the same
                         manifest, but it faults at once
      clock-good.svc     clock.svc with 16 bytes appended
      clock.svc          another service's program for lexicon: another manifest
  - with "Allow app replacement" off (as at boot): refused (Switch_Off);
    lexicon keeps running;
  - the person turns the switch on (Super+N, a click on it);
  - an entry whose restart contract is reboot (logstore.svc): Not_Replaceable;
  - lexicon pinned to clock.svc: Manifest_Changed;
  - lexicon pinned to lexicon-good: Activated; procmgr runs it from the
    storehouse (@system/storehouse/executables/<sha256>), it registered
    again (the health check), (proc.list) shows it;
  - lexicon pinned to lexicon-crash: Rolled_Back (it ended inside its health
    window), lexicon-good runs again, the current generation stays;
  - clock pinned to clock-good with no stop grace: refused (a graceful
    entry needs one); with a grace: Stop sent, clock does not quiesce, so
    the switch fails with clock still running (never ended to make way);
  - system rollback: the current generation's previous (the image, 0) is
    activated: lexicon's image program runs again;
  - system generations lists the records (typed CCL values) and current 0.
Then quits QEMU. Prints PASS/FAIL lines; exits 1 on any failure.
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

serial, qmp_socket, timeout, port, lexicon_svc, clock_svc = (
    sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4]), Path(sys.argv[5]), Path(sys.argv[6]))
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
SESSION_SHOWN = b"desktop: remote session (process "
SWITCH_AT = re.compile(rb"desktop: status panel open at [0-9,]+ \(\d+x\d+\); "
                       rb"Allow app replacement switch at (\d+),(\d+)")
SWITCH_ALLOWED = b"procmgr: staged app launches allowed by the Desktop switch"
READY = b"lexicon: registered"
SERVICE = "lexicon.svc"
# Explicit deadlines (milliseconds): lexicon reads no Stop, so it is ended
# at once (transparent); it registers at once, well inside the window.
HEALTH_MS = 5_000
STOP_GRACE_MS = 0
CLOCK_GRACE_MS = 2_000
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
        print(f"service-switch: could not quit QEMU: {error}", file=sys.stderr)


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
    raise SystemExit("service-switch: no loadable entry point")


def fixtures(work):
    lexicon = lexicon_svc.read_bytes()
    clock = clock_svc.read_bytes()
    crash = bytearray(lexicon)
    at = entry_file_offset(lexicon)
    crash[at:at + 2] = b"\x0f\x0b"   # ud2: an invalid-opcode fault at once
    paths = {}
    for name, data in (("lexicon-good.svc", lexicon + b"service-switch-1"), ("lexicon-crash.svc", bytes(crash)),
                       ("clock-good.svc", clock + b"service-switch-1"), ("clock.svc", clock)):
        path = os.path.join(work, name)
        Path(path).write_bytes(data)
        paths[name] = (path, hashlib.sha256(data).hexdigest())
    return paths


def activate(host, service, product, grace=STOP_GRACE_MS):
    try:
        report = cr.system_activate(host, service, product, HEALTH_MS, grace)
        report["refused"] = None
    except cr.GenerationRefused as refused:
        report = refused.report
        report["refused"] = refused.name
    print("service-switch: activate %s %s: %s (%s)" % (service, product, report, cr.generation_image(report)),
          flush=True)
    return report


def processes(host):
    ok, text = cr.evaluate_whole(host, "(proc.list)")
    return text if ok else ""


def main():
    host = pair()
    if host is None:
        return
    check(wait_for_serial(READY, 60), "lexicon.svc (the image's program) registered")
    work = tempfile.mkdtemp(prefix="cubit-service-switch-")
    paths = fixtures(work)
    for name, (path, _) in paths.items():
        cr.put_file(host, "workspace", path, "switch/" + name)
    good_hash, crash_hash = paths["lexicon-good.svc"][1], paths["lexicon-crash.svc"][1]

    # Off, as at boot: refused before anything stops.
    report = activate(host, SERVICE, "switch/lexicon-good.svc")
    check(report["refused"] == "Refused" and report["detail"] == 6 and report["current"] == 0,
          "with Allow app replacement off an activation is refused (Switch_Off); generation 0 stays")
    check(wait_for_serial(b"procmgr: switch refused: SWITCH_OFF"), "procmgr says why (SWITCH_OFF)")
    check(b"procmgr: switch lexicon.svc: ending" not in serial_bytes(), "lexicon.svc was not stopped")

    allow_app_replacement()

    report = activate(host, "logstore.svc", "switch/lexicon-good.svc")
    check(report["refused"] == "Refused" and report["detail"] == 4,
          "an entry whose restart contract is reboot is refused (Not_Replaceable)")
    report = activate(host, SERVICE, "switch/clock.svc")
    check(report["refused"] == "Refused" and report["detail"] == 11,
          "a program with another manifest is refused (Manifest_Changed)")
    check(wait_for_serial(b"procmgr: switch refused: MANIFEST_CHANGED"), "procmgr says MANIFEST_CHANGED")

    started = time.monotonic()
    report = activate(host, SERVICE, "switch/lexicon-good.svc")
    good_generation = report["current"]
    check(report["refused"] is None and report["result_name"] == "Activated" and good_generation > 0
          and report["pin_sha256"] == good_hash and report["running"] != 0,
          "lexicon pinned to lexicon-good: generation %d activated, its pin the product's SHA-256"
          % good_generation)
    print("service-switch: TIME switch %.1f s" % (time.monotonic() - started), flush=True)
    good_program = (STOREHOUSE + good_hash).encode()
    check(wait_for_serial(b"procmgr: switch lexicon.svc switched; it runs " + good_program),
          "procmgr runs lexicon from the storehouse by content address")
    check(wait_for_serial(READY, 10, count=2), "the new lexicon registered")
    # (proc.list) names carry 16 bytes (CuBit.Process_Observer): the
    # storehouse path's start; procmgr's log line above has it whole.
    listed = processes(host)
    check(re.search(r'\(Process \d+ "%s" "com\.cubit\.lexicon"' % re.escape(STOREHOUSE[:16]), listed)
          is not None, "(proc.list) shows com.cubit.lexicon running a storehouse program")

    report = activate(host, SERVICE, "switch/lexicon-crash.svc")
    check(report["refused"] == "Rolled_Back" and report["detail"] == 2 and report["current"] == good_generation
          and report["pin_sha256"] == crash_hash and report["running"] != 0,
          "lexicon pinned to lexicon-crash: it ended in its window; rolled back, generation %d stays"
          % good_generation)
    check(wait_for_serial(b"procmgr: switch lexicon.svc rolled back (the program ended inside its health window);"
                          b" it runs " + good_program),
          "procmgr rolled back to lexicon-good (the program before)")
    check(wait_for_serial(READY, 10, count=3), "lexicon-good registered again")

    # Graceful: clock must quiesce on Stop; it does not handle Stop.
    report = activate(host, "clock.svc", "switch/clock-good.svc", grace=0)
    check(report["refused"] == "Refused" and report["detail"] == 1,
          "a graceful entry with no stop grace is refused")
    report = activate(host, "clock.svc", "switch/clock-good.svc", grace=CLOCK_GRACE_MS)
    check(report["refused"] == "Stop_Failed" and report["current"] == good_generation,
          "clock (graceful) did not quiesce in its grace: not switched; generation %d stays" % good_generation)
    check(wait_for_serial(b"procmgr: switch clock.svc not switched: the running instance did not end;"
                          b" it runs clock.svc pid"),
          "clock keeps running its image program (never ended to make way)")
    check(b"procmgr: switch clock.svc: ending" not in serial_bytes(), "procmgr never ended clock")

    report = cr.system_rollback(host, HEALTH_MS, STOP_GRACE_MS)
    print("service-switch: rollback: %s (%s)" % (report, cr.generation_image(report)), flush=True)
    check(report["result_name"] == "Activated" and report["current"] == 0 and report["running"] != 0,
          "system rollback: the image's generation (0) is current again")
    check(wait_for_serial(b"procmgr: switch lexicon.svc switched; it runs lexicon.svc pid"),
          "procmgr runs lexicon's image program again")

    lines, current = cr.system_generations(host)
    print("service-switch: generations:\n" + "\n".join(lines) + "\ncurrent %s" % current, flush=True)
    check(current == 0 and any(good_hash in line for line in lines)
          and all(line.startswith("(System_Generation number => ") for line in lines),
          "system generations: typed records, lexicon-good's among them; current 0")
    for marker in (b"ccl-remote: host app system activate lexicon.svc: generation",
                   b"ccl-remote: host app system rollback"):
        check(wait_for_serial(marker), "the device shows the activity line %r" % marker.decode())
    for name, (path, _) in paths.items():
        os.remove(path)
    os.rmdir(work)


try:
    main()
finally:
    if os.environ.get("SERVICE_SWITCH_KEEP_RUNNING") != "1":
        quit_qemu()
print("SERVICE-SWITCH: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
