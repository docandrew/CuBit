#!/usr/bin/env python3
"""The acpi-live-replace test's host side (tests/headless/run.sh --test
acpi-live-replace): the one acpi.svc replaced live through system
generations, with its hardware grants retired and granted again.

Usage: acpi_live_replace.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT ACPI_SVC

The guest runs acpi.svc with (replace graceful) and the laptop plans'
approvals (tests/acpi-live-replace/init.ccl). Like a LAN host this pairs
with the code on the guest's console and, with tools/cubit-remote:
  - puts programs made from the built acpi.svc into the workspace (no
    source change is needed for another content address):
      acpi-good.svc   acpi.svc with 16 bytes appended (another build stamp)
      acpi-crash.svc  its entry instruction ud2: it faults at once
      acpi-hang.svc   its entry instruction jmp $: it runs, holding every
                      grant procmgr installed, and never registers or reads
                      Stop
      acpi-late.svc   acpi.svc with other 16 bytes appended
  - the person turns "Allow app replacement" on (a click);
  - no stop grace: refused (a graceful entry needs one);
  - acpi-good: Stop sent, acpi.svc stops serving (STOP_REQUESTED), retires
    (QUIESCENT) and exits; procmgr starts acpi-good from the storehouse as
    the same entry and it registers again: Activated;
  - acpi-crash: it ends inside its window: Rolled_Back (Exited_Early), and
    acpi-good runs (and registers) again;
  - acpi-hang: not registered at the end of its window: asked to Stop,
    ended after the grace, Rolled_Back (Not_Registered); acpi-good again;
  - a 1 ms stop grace (acpi-late): if the running instance misses it
    (still starting: it reads no Stop before it serves), procmgr aborts
    (Stop_Failed), ends nothing and keeps the generation; the Stop cannot
    be withdrawn, so the instance stops once it serves, and procmgr starts
    the kept generation's program again (Late_Stop). On q35 acpi.svc
    usually meets even 1 ms (Stop, retire, exit 0), and the switch
    completes; the run prints which BRANCH it took;
  - system rollback until the image's generation (0) is current.
Then, over the whole run: every instance that configured its hardware got
exactly the first instance's grant (memory windows, ports, pool and PCI
endpoints), mapped the same platform-pool member, and no grant was refused
as still held (devmgr's port and memory ledgers, the kernel's pool) or
withheld; every stop retired QUIESCENT; (proc.list) shows one acpi.svc.
Prints PASS/FAIL lines; exits 1 on any failure.
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

serial, qmp_socket, timeout, port, acpi_svc = (
    sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4]), Path(sys.argv[5]))
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
SESSION_SHOWN = b"desktop: remote session (process "
SWITCH_AT = re.compile(rb"desktop: status panel open at [0-9,]+ \(\d+x\d+\); "
                       rb"Allow app replacement switch at (\d+),(\d+)")
SWITCH_ALLOWED = b"procmgr: staged app launches allowed by the Desktop switch"
# Registration now advertises STARTING. Keep this gate at the former late
# checkpoint: selected LOCAL_RUNTIME, before the subsequent event-start checks.
RUNTIME_SELECTED = b"acpi: stage runtime-selection ok one-service mode=LOCAL_RUNTIME"
HARDWARE = re.compile(rb"acpi: stage hardware-config ok [^\r\n]*")
POOL_MAPPED = re.compile(rb"platform-pool: mapped phys=[^\r\n]*")
STOP_SERVED = b"acpi: stage serve ok STOP_REQUESTED"
RETIRED = b"acpi: stage retire ok QUIESCENT"
SERVICE = "acpi.svc"
# Explicit deadlines (milliseconds). The window covers acpi.svc's startup
# on q35 (AML load, _INI walk, registration); procmgr judges at its end.
HEALTH_MS = 20_000
STOP_GRACE_MS = 5_000
# The abort case: far shorter than any startup, so the Stop is not read.
ABORT_GRACE_MS = 1
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
        print(f"acpi-live-replace: could not quit QEMU: {error}", file=sys.stderr)


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
    raise SystemExit("acpi-live-replace: no loadable entry point")


def fixtures(work):
    acpi = acpi_svc.read_bytes()
    at = entry_file_offset(acpi)
    crash, hang = bytearray(acpi), bytearray(acpi)
    crash[at:at + 2] = b"\x0f\x0b"   # ud2: an invalid-opcode fault at once
    hang[at:at + 2] = b"\xeb\xfe"    # jmp $: runs, never registers
    paths = {}
    for name, data in (("acpi-good.svc", acpi + b"acpi-live-repl-1"), ("acpi-crash.svc", bytes(crash)),
                       ("acpi-hang.svc", bytes(hang)), ("acpi-late.svc", acpi + b"acpi-live-repl-2")):
        path = os.path.join(work, name)
        Path(path).write_bytes(data)
        paths[name] = (path, hashlib.sha256(data).hexdigest())
    return paths


def activate(host, product, grace=STOP_GRACE_MS):
    try:
        report = cr.system_activate(host, SERVICE, product, HEALTH_MS, grace)
        report["refused"] = None
    except cr.GenerationRefused as refused:
        report = refused.report
        report["refused"] = refused.name
    print("acpi-live-replace: activate %s: %s (%s)" % (product, report, cr.generation_image(report)), flush=True)
    return report


def acpi_processes(host):
    ok, text = cr.evaluate_whole(host, "(proc.list)")
    return len(re.findall(r'\(Process \d+ "[^"]*" "com\.cubit\.acpi"', text)) if ok else -1


def main():
    host = pair()
    if host is None:
        return
    check(wait_for_serial(RUNTIME_SELECTED, 90), "acpi.svc (the image's program) selected its local runtime")
    first = serial_bytes()
    hardware = HARDWARE.findall(first)
    check(len(hardware) == 1, "the first instance configured its hardware: %r" % hardware[:1])
    pool_per_instance = len(POOL_MAPPED.findall(first))
    check(pool_per_instance >= 1, "the first instance mapped %d platform-pool member(s)" % pool_per_instance)
    work = tempfile.mkdtemp(prefix="cubit-acpi-live-replace-")
    paths = fixtures(work)
    for name, (path, _) in paths.items():
        cr.put_file(host, "workspace", path, "acpi/" + name)
    good_hash = paths["acpi-good.svc"][1]
    good_program = (STOREHOUSE + good_hash).encode()
    selected_instances = 1
    stops = 0

    allow_app_replacement()

    report = activate(host, "acpi/acpi-good.svc", grace=0)
    check(report["refused"] == "Refused" and report["detail"] == 1 and report["current"] == 0,
          "acpi.svc is (replace graceful): an activation with no stop grace is refused")

    # Stop obeyed, retired, exited 0; the new program reaches local runtime.
    started = time.monotonic()
    report = activate(host, "acpi/acpi-good.svc")
    good_generation = report["current"]
    check(report["refused"] is None and report["result_name"] == "Activated" and good_generation > 0
          and report["pin_sha256"] == good_hash and report["running"] != 0,
          "acpi pinned to acpi-good: generation %d activated" % good_generation)
    print("acpi-live-replace: TIME switch %.1f s" % (time.monotonic() - started), flush=True)
    stops += 1
    selected_instances += 1
    check(wait_for_serial(b"procmgr: switch acpi.svc: Stop sent to pid"), "procmgr sent the running acpi.svc Stop")
    check(wait_for_serial(STOP_SERVED, 10, stops), "acpi.svc left serving on Stop (STOP_REQUESTED)")
    check(wait_for_serial(RETIRED, 10, stops), "acpi.svc retired its hardware (QUIESCENT) before exiting")
    check(wait_for_serial(b"procmgr: switch acpi.svc switched; it runs " + good_program),
          "procmgr runs acpi from the storehouse by content address")
    check(wait_for_serial(RUNTIME_SELECTED, 10, selected_instances), "the new acpi.svc ran startup again and selected its local runtime")

    # A program that faults at once: rolled back to acpi-good.
    report = activate(host, "acpi/acpi-crash.svc")
    stops += 1
    selected_instances += 1
    check(report["refused"] == "Rolled_Back" and report["detail"] == 2 and report["current"] == good_generation,
          "acpi-crash ended in its window: rolled back, generation %d stays" % good_generation)
    check(wait_for_serial(b"procmgr: switch acpi.svc rolled back (the program ended inside its health window);"
                          b" it runs " + good_program), "procmgr runs acpi-good again")
    check(wait_for_serial(RETIRED, 10, stops), "the running acpi.svc retired QUIESCENT before the crash ran")
    check(wait_for_serial(RUNTIME_SELECTED, 30, selected_instances), "acpi-good selected its local runtime again")

    # A program that holds its grants and never registers: stopped, ended.
    report = activate(host, "acpi/acpi-hang.svc")
    stops += 1
    selected_instances += 1
    check(report["refused"] == "Rolled_Back" and report["detail"] == 3 and report["current"] == good_generation,
          "acpi-hang did not register in its window: rolled back (Not_Registered), generation %d stays"
          % good_generation)
    check(wait_for_serial(b", which failed its health window"),
          "procmgr ended the hung instance after its grace (it read no Stop)")

    # Abort: a 1 ms stop grace. The restored acpi-good may still be
    # starting (it reads no Stop before it serves); then procmgr aborts
    # (Stop_Failed), ends nothing and keeps the generation, the queued Stop
    # is obeyed once serving starts, and procmgr starts the kept
    # generation's program again (Late_Stop). On q35 acpi.svc usually has
    # registered by the time the request arrives and stops in under 1 ms
    # (Stop, retire QUIESCENT, exit 0): then the switch completes, which is
    # also correct; the run says which branch it took.
    report = activate(host, "acpi/acpi-late.svc", grace=ABORT_GRACE_MS)
    if report["refused"] == "Stop_Failed":
        print("acpi-live-replace: BRANCH abort (the running instance missed a 1 ms grace)", flush=True)
        check(report["current"] == good_generation and report["running"] != 0,
              "aborted (Stop_Failed): generation %d stays" % good_generation)
        check(wait_for_serial(b"procmgr: switch acpi.svc not switched: the running instance did not end;"
                              b" it runs " + good_program), "procmgr kept acpi-good running (never ended)")
        # The one instance procmgr ever ended is the hung one (End_New above).
        check(serial_bytes().count(b"procmgr: switch acpi.svc: ending pid") == 1,
              "procmgr ended nothing for the aborted switch")
        check(wait_for_serial(b"procmgr: acpi.svc ended after its stop grace (cleanly; the switch was aborted):"
                              b" starting it again", 40),
              "after the queued Stop was obeyed, procmgr started the kept generation's program again")
        check(wait_for_serial(b"procmgr: acpi.svc runs again pid", 10), "it runs again")
        lines, current = cr.system_generations(host)
        check(current == good_generation, "system generations: generation %d is still current" % good_generation)
    else:
        print("acpi-live-replace: BRANCH switched (acpi.svc stopped within a 1 ms grace)", flush=True)
        check(report["refused"] is None and report["result_name"] == "Activated"
              and report["pin_sha256"] == paths["acpi-late.svc"][1],
              "a 1 ms stop grace: acpi.svc stopped in time; acpi-late activated")
    time.sleep(3)
    check(acpi_processes(host) == 1, "after the 1 ms grace one acpi.svc runs")

    # Back to the image's program (generation 0), one rollback at a time.
    for _ in range(3):
        report = cr.system_rollback(host, HEALTH_MS, STOP_GRACE_MS)
        print("acpi-live-replace: rollback: %s (%s)" % (report, cr.generation_image(report)), flush=True)
        if report["result_name"] != "Activated" or report["current"] == 0:
            break
    check(report["result_name"] == "Activated" and report["current"] == 0 and report["running"] != 0,
          "system rollback: the image's generation (0) is current again")
    check(wait_for_serial(b"procmgr: switch acpi.svc switched; it runs acpi.svc pid"),
          "procmgr runs acpi.svc's image program again")
    time.sleep(2)

    # The grants, over every instance that reached its hardware.
    data = serial_bytes()
    seen = HARDWARE.findall(data)
    runtime_selections = data.count(RUNTIME_SELECTED)
    check(len(seen) == runtime_selections and all(line == hardware[0] for line in seen),
          "all %d instances that configured hardware got the first one's grant exactly (%s); %d reached local runtime"
          % (len(seen), hardware[0].decode(), runtime_selections))
    check(len(POOL_MAPPED.findall(data)) == len(seen) * pool_per_instance,
          "each instance mapped the platform pool as the first did (%d)" % len(POOL_MAPPED.findall(data)))
    check(data.count(RETIRED) == data.count(STOP_SERVED) >= stops and b"acpi: stage retire fail" not in data,
          "every Stop served (%d) retired QUIESCENT" % data.count(STOP_SERVED))
    check(not any(b"com.cubit.acpi pid" in line and b"exited code" in line and b"exited code 0" not in line
                  for line in data.splitlines()),
          "every acpi.svc that exited on its own exited 0 (quiesced)")
    for refusal in (b"left out for acpi.svc", b"ACPI hardware refused", b"PCI_PREGRANT_REFUSED",
                    b"devmgr: platform pool refused", b"devmgr: platform pool copy refused",
                    b"devmgr: firmware data refused", b"platform-pool: refused claim",
                    b"STARTUP_REFUSED", b"Ada heap: cannot allocate"):
        check(refusal not in data, "never: %s" % refusal.decode())
    check(acpi_processes(host) == 1, "(proc.list) shows exactly one com.cubit.acpi")
    for name, (path, _) in paths.items():
        os.remove(path)
    os.rmdir(work)


try:
    main()
finally:
    if os.environ.get("ACPI_LIVE_REPLACE_KEEP_RUNNING") != "1":
        quit_qemu()
print("ACPI-LIVE-REPLACE: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
