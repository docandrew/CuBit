#!/usr/bin/env python3
"""The console-run-staged test's host side (tests/headless/run.sh --test
console-run-staged): an app built on CuBit runs from the CCL console as a
typed CCL call, and a rebuild is what the next call runs, with no console
restart.

Usage: console_run.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT BUILD_DIR

The guest is the device-build test's (tests/device-builds: the toolchain,
the Storehouse and the workspace on the UFS disk's root, @system) with the
CCL console beside ccl-remote. Its manifest's launch table names the
staging directory "@system/work/remote/out/" (where ccl-build exports what
it builds for ccl-remote) and holds a read scope over that directory only.
Like a LAN host this pairs with the code on the guest's console and, with
tools/cubit-remote and keys typed into the console over QMP:
  - puts hello (userspace/apps/hello, BUILD_DIR/app-hello) into the
    workspace and builds it on CuBit;
  - (programs.refresh) with "Allow app replacement" off (as at boot):
    hello is staged and Refused, with procmgr's reason;
  - the person turns the switch on (Super+N, a click on it);
  - (programs.refresh): hello is Ready; the console republishes its
    programs once the entry ends (hello.run checks from the next entry);
  - (hello.run (Hello_Parameters)): the run's greeting outlet holds hello's
    line, and it finished with exit code 0;
  - hello rebuilt on CuBit with another line; the next (hello.run ...)
    shows the new line: no refresh, no console restart;
  - hello rebuilt to raise an exception nobody handles after its line: its
    outcome is Failed (Run_Failure.Unhandled_Exception) and the runtime's
    report names the exception and its message (EXC-001);
  - the product removed: (programs.refresh) no longer lists it and
    hello.run no longer checks; put back with cubit-remote put (the Linux
    build of the first hello, BUILD_DIR/expected-hello.app): listed again,
    and its run shows the first line;
  - app replace stages hello as ccl-remote's generation 1
    (@system/work/generations/hello/1.app), where the console holds no
    scope and its launch table no directory: (programs.check ...) on it is
    Refused, named.
Then quits QEMU. Prints PASS/FAIL lines; exits 1 on any failure.
"""
import json
import re
import shutil
import socket
import struct
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "cubit-remote"))
import cubit_remote as cr  # noqa: E402

serial, qmp_socket, timeout, port, build_dir = (
    sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4]), Path(sys.argv[5]))
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
CONSOLE_READY = b"ccl-console: first frame presented"
SESSION_SHOWN = b"desktop: remote session (process "
SWITCH_AT = re.compile(rb"desktop: status panel open at [0-9,]+ \(\d+x\d+\); "
                       rb"Allow app replacement switch at (\d+),(\d+)")
SWITCH_ALLOWED = b"procmgr: staged app launches allowed by the Desktop switch"
CONSOLE_SPAWNED = b"procmgr: spawn: ccl-console.app"
REPL = re.compile(rb"ccl-console: REPL completed: ([^\n]*)")
HELLO = "userspace/apps/hello"
SOURCES = ("component.ccl", "manifest.ccl", "main.adb")
PRODUCT = "out/%s/build/hello.app" % HELLO
STAGED = "@system/work/remote/%s" % PRODUCT
GENERATION = "@system/work/generations/hello/1.app"
FIRST_LINE = "hello: hello from an app built on CuBit"
SECOND_LINE = "hello: rebuilt on CuBit, run again from the console"
RAISING_LINE = "hello: about to raise an exception nobody handles"
EXIT_STATEMENT = "Ignore := syscall (SYSCALL_EXIT, Exit_Success);"
RAISE_STATEMENT = 'raise Program_Error with "hello raised on purpose";'
UNHANDLED_REPORT = b"User-space exception PROGRAM_ERROR: hello raised on purpose"
STATE_RUNNING, STATE_EXITED = 1, 2
APP_REPLACED = 0
MOUSE_STEP = 60
MOUSE_CORNER_STEPS = 60
KEY_SECONDS = 0.03
REPL_SECONDS = 30
RUN_SECONDS = 20
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

    def hmp(self, line, pause=0.03):
        self.command("human-monitor-command", {"command-line": line})
        time.sleep(pause)

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


# US layout, as tests/ccl-console/send-text.py.
SHIFTED = {'(': '9', ')': '0', '"': 'apostrophe', '>': 'dot', '<': 'comma', '_': 'minus',
           ':': 'semicolon', '@': '2', '+': 'equal', '*': '8', '!': '1', '?': 'slash'}
PLAIN = {' ': 'spc', '.': 'dot', ',': 'comma', '-': 'minus', '=': 'equal', '/': 'slash',
         ';': 'semicolon', "'": 'apostrophe', '[': 'bracket_left', ']': 'bracket_right'}


def key(c):
    if c.isalpha():
        return ('shift-' + c.lower()) if c.isupper() else c
    if c.isdigit():
        return c
    if c in SHIFTED:
        return 'shift-' + SHIFTED[c]
    return PLAIN[c]


def console(text):
    """Type an entry into the console and Enter; its REPL result line (or
    "" when none came)."""
    before = len(REPL.findall(serial_bytes()))
    monitor = Monitor()
    try:
        for c in text:
            monitor.hmp("sendkey %s" % key(c), KEY_SECONDS)
        monitor.hmp("sendkey ret", 0.2)
    finally:
        monitor.close()
    end = min(deadline, time.monotonic() + REPL_SECONDS)
    while time.monotonic() < end:
        found = REPL.findall(serial_bytes())
        if len(found) > before:
            result = found[before].decode("utf-8", "replace")
            print("console-run-staged: %s -> %s" % (text, result[:400]), flush=True)
            return result
        time.sleep(0.2)
    print("console-run-staged: %s -> (no result)" % text, flush=True)
    return ""


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


def build_hello(host, sources, label):
    for name in SOURCES:
        cr.put_file(host, "workspace", str(sources / name), "%s/%s" % (HELLO, name))
    job = struct.unpack(">I", host.call("build_start", "workspace",
                                        cr.name_field(HELLO + "/component.ccl")))[0]
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
    print("console-run-staged: %s: job %d (exit code %d); output:\n%s" % (
        label, job, code, output.decode("utf-8", "replace")), flush=True)
    check(state == STATE_EXITED and code == 0, "%s builds on CuBit, exit 0" % label)


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


def entry_of(result, name, program):
    """The Program_Entry for (name, program) in a refresh's result: its
    state and why, or None."""
    found = re.search(r'\(Program_Entry "%s" "%s" (true|false) Program_State\.(\w+) "([^"]*)"\)'
                      % (re.escape(name), re.escape(program)), result)
    return (found.group(2), found.group(3), found.group(1) == "true") if found else None


def run_hello(name, line, label, raises=False):
    """Run hello from the console; check its greeting outlet and outcome:
    finished with exit code 0, or (raises) failed with an unhandled
    exception (EXC-001)."""
    console("(define %s (hello.run (Hello_Parameters)))" % name)
    end = min(deadline, time.monotonic() + RUN_SECONDS)
    outcome = ""
    while time.monotonic() < end:
        outcome = console("(hello.outcome %s)" % name)
        if "Done" in outcome:
            break
        time.sleep(1)
    if raises:
        check("Run_Outcome.Failed" in outcome and "Run_Failure.Unhandled_Exception" in outcome,
              "%s: the run failed with an unhandled exception: %s" % (label, outcome[:200]))
    else:
        check("Run_Outcome.Finished" in outcome and "(Unix_Exit 0)" in outcome,
              "%s: the run finished with exit code 0: %s" % (label, outcome[:200]))
    greeting = console("(window 64 (hello.com.cubit.hello.greeting %s))" % name)
    check('"%s"' % line in greeting, "%s: its greeting outlet holds \"%s\"" % (label, line))
    return greeting


def main():
    host = pair()
    if host is None:
        return
    check(wait_for_serial(CONSOLE_READY, 60), "the CCL console runs")
    time.sleep(1)

    build_hello(host, build_dir / "app-hello", "hello")
    result = console("(programs.refresh)")
    entry = entry_of(result, "hello", STAGED)
    check(entry is not None and entry[0] == "Refused" and entry[2] and "Allow app replacement" in entry[1],
          "with Allow app replacement off, the staged hello is Refused, named: %s" % (entry,))
    check(entry_of(result, "as", "as.app") is not None, "the launch table's own programs are listed too")

    allow_app_replacement()
    time.sleep(1)
    result = console("(programs.refresh)")
    entry = entry_of(result, "hello", STAGED)
    check(entry is not None and entry[0] == "Ready", "with the switch on, hello is Ready: %s" % (entry,))
    ok, text = cr.evaluate_whole(host, '(logs.recent "com.cubit.ccl-console")')
    check(ok and "ccl-console: programs republished" in text,
          "the console's log says it republished its programs")
    run_hello("r1", FIRST_LINE, "hello")
    check(wait_for_serial(b"procmgr: staged program " + STAGED.encode()),
          "procmgr started it as a staged program")

    # A rebuild: the next call runs it, with no refresh and no restart.
    changed = Path(tempfile.mkdtemp(prefix="cubit-console-run-"))
    try:
        for name in ("component.ccl", "manifest.ccl"):
            shutil.copy2(build_dir / "app-hello" / name, changed / name)
        text = (build_dir / "app-hello" / "main.adb").read_text()
        check(FIRST_LINE in text, "hello's source says the first line")
        (changed / "main.adb").write_text(text.replace(FIRST_LINE, SECOND_LINE))
        build_hello(host, changed, "hello, changed")
    finally:
        shutil.rmtree(changed)
    second = run_hello("r2", SECOND_LINE, "hello rebuilt")
    check(FIRST_LINE not in second, "the rebuilt hello's outlet does not hold the first line")
    check(serial_bytes().count(CONSOLE_SPAWNED) == 1, "the console never restarted")

    # EXC-001: a build that raises an exception nobody handles after its
    # greeting; the console's outcome is the typed failure, and the
    # runtime's report names the exception.
    raising = Path(tempfile.mkdtemp(prefix="cubit-console-run-"))
    try:
        for name in ("component.ccl", "manifest.ccl"):
            shutil.copy2(build_dir / "app-hello" / name, raising / name)
        text = (build_dir / "app-hello" / "main.adb").read_text()
        check(EXIT_STATEMENT in text, "hello's source ends with its exit")
        (raising / "main.adb").write_text(
            text.replace(FIRST_LINE, RAISING_LINE).replace(EXIT_STATEMENT, RAISE_STATEMENT))
        build_hello(host, raising, "hello, raising")
    finally:
        shutil.rmtree(raising)
    run_hello("r2x", RAISING_LINE, "hello raising", raises=True)
    check(wait_for_serial(UNHANDLED_REPORT),
          "the runtime reported the unhandled exception by name and message")

    # Gone, then staged again by a put.
    host.call("remove", "workspace", cr.name_field(PRODUCT))
    result = console("(programs.refresh)")
    check(result != "" and entry_of(result, "hello", STAGED) is None,
          "the product removed: a refresh no longer lists hello")
    time.sleep(1)
    gone = console("(hello.run (Hello_Parameters))")
    check(gone != "" and not gone.startswith("Run:"), "hello.run no longer checks: %s" % gone[:200])
    cr.put_file(host, "workspace", str(build_dir / "expected-hello.app"), PRODUCT)
    result = console("(programs.refresh)")
    entry = entry_of(result, "hello", STAGED)
    check(entry is not None and entry[0] == "Ready", "put back with cubit-remote put: Ready again")
    time.sleep(1)
    run_hello("r3", FIRST_LINE, "hello put back")

    # Staged where the console holds no scope: refused, named.
    report = cr.app_call(host, "app_replace", PRODUCT)
    check(report["result"] == APP_REPLACED and report["current"] == 1,
          "app replace stages hello as ccl-remote's generation 1")
    result = console('(programs.check (Input_File "%s"))' % GENERATION)
    entry = entry_of(result, "1", GENERATION)
    check(entry is not None and entry[0] == "Refused"
          and "neither named by this process's launch table nor beneath one of its staging directories"
          in entry[1],
          "a staged program the console has no scope over is Refused, named: %s" % (entry,))
    check(wait_for_serial(("ccl-launcher: %s refused: " % GENERATION).encode()),
          "the refusal is on the device's log")
    result = console('(programs.check (Input_File "%s"))' % STAGED)
    entry = entry_of(result, "hello", STAGED)
    check(entry is not None and entry[0] == "Ready", "the staged build checks Ready: %s" % (entry,))


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
        print("console-run-staged: could not quit QEMU: %s" % error, file=sys.stderr)


try:
    main()
except (OSError, cr.RemoteError) as error:
    check(False, "the driver stopped: %s" % error)
finally:
    quit_qemu()
print("CONSOLE-RUN-STAGED: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
