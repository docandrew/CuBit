#!/usr/bin/env python3
"""VMM-001 slice 1: screenshots of the guest's serial console in a CuBit window.

Waits on the serial log for the guest's prompt (after its uname -a and
/proc/cpuinfo lines), then takes QEMU's scanout as PNG into OUTDIR, with a
README naming what each shot shows. Then types into the window with QMP
send-key (the CuBit desktop's keyboard, routed to vmm's focused window and
on to the guest's serial port): `echo typed-$((6*7))` and Enter, which must
print typed-42; single keys, timed from QMP's send to vmm's "key to echo"
line on the serial log; and Ctrl+C on `sleep 100`, after which the shell
must take the next command at once.

usage: vm-desktop-shots.py SERIAL QMP_SOCKET TIMEOUT OUTDIR
"""
import json
from pathlib import Path
import socket
import statistics
import sys
import time

# QMP qcodes for the characters typed (US layout); shifted ones press shift.
QCODES = {" ": "spc", "-": "minus", "=": "equal", "\r": "ret"}
SHIFTED = {"$": "4", "(": "9", ")": "0", "*": "8", "+": "equal"}
HOLD_MS = 30          # each key's press-to-release, below the desktop's repeat delay
SAMPLES = 10          # timed keys
ECHO_LINE = b"vmm: key to echo "

serial, socket_path, timeout, outdir = sys.argv[1:]
log = Path(serial)
out = Path(outdir)
out.mkdir(parents=True, exist_ok=True)
deadline = time.monotonic() + float(timeout)


def text():
    try:
        return log.read_text(errors="replace")
    except FileNotFoundError:
        return ""


def wait_for(marker, within=None):
    limit = deadline if within is None else min(deadline, time.monotonic() + within)
    while marker not in text():
        if time.monotonic() > limit:
            raise SystemExit(f"vm-desktop-shots: no {marker!r} in time")
        time.sleep(0.2)


class EchoCounter:
    """vmm's "key to echo" lines so far, reading only what the log gained."""

    def __init__(self):
        self.offset = 0
        self.count = 0
        self.tail = b""

    def __call__(self):
        try:
            with open(serial, "rb") as f:
                f.seek(self.offset)
                data = f.read()
        except FileNotFoundError:
            return 0
        self.offset += len(data)
        chunk = self.tail + data
        self.count += chunk.count(ECHO_LINE)
        # Keep a partial marker for the next read, never a whole one.
        cut = chunk.rfind(ECHO_LINE)
        rest = chunk[cut + len(ECHO_LINE):] if cut >= 0 else chunk
        self.tail = rest[-(len(ECHO_LINE) - 1):]
        return self.count


echo_lines = EchoCounter()


while not Path(socket_path).exists():
    if time.monotonic() > deadline:
        raise SystemExit("vm-desktop-shots: no QMP socket")
    time.sleep(0.1)

with socket.socket(socket.AF_UNIX) as connection:
    connection.settimeout(5)
    connection.connect(socket_path)
    stream = connection.makefile("rwb")
    json.loads(stream.readline())

    def command(name, arguments=None):
        request = {"execute": name}
        if arguments is not None:
            request["arguments"] = arguments
        stream.write(json.dumps(request).encode() + b"\n")
        stream.flush()
        while True:
            reply = json.loads(stream.readline())
            if "error" in reply:
                raise SystemExit(f"vm-desktop-shots: {reply}")
            if "return" in reply:
                return reply["return"]

    command("qmp_capabilities")
    wait_for("] Linux version 6.12")
    time.sleep(1.0)
    command("screendump", {"filename": str(out / "01-linux-booting.png"), "format": "png"})
    wait_for("vmm guest: CuBit guest: shell ready")
    time.sleep(1.5)
    command("screendump", {"filename": str(out / "02-shell-prompt-uname.png"), "format": "png"})

    def key(*names):
        command("send-key", {"keys": [{"type": "qcode", "data": n} for n in names], "hold-time": HOLD_MS})

    def type_text(chars):
        for ch in chars:
            if ch in SHIFTED:
                key("shift", SHIFTED[ch])
            else:
                key(QCODES.get(ch, ch))
            time.sleep(HOLD_MS / 1000 * 2)

    type_text("echo typed-$((6*7))\r")
    wait_for("vmm guest: typed-42", within=20)
    time.sleep(0.5)
    command("screendump", {"filename": str(out / "03-typed.png"), "format": "png"})

    # Key to echo: QMP's send (the press) to vmm's line for the guest's echo.
    times = []
    for ch in "x" * SAMPLES + "\b" * SAMPLES:
        before = echo_lines()
        start = time.monotonic()
        key("backspace" if ch == "\b" else ch)
        while echo_lines() == before:
            if time.monotonic() - start > 5:
                raise SystemExit("vm-desktop-shots: a key was not echoed within 5 s")
            time.sleep(0.001)
        times.append((time.monotonic() - start) * 1000)
        time.sleep(HOLD_MS / 1000 * 2)
    summary = (f"vm-desktop-shots: key to echo (QMP send to vmm's echo line) over {len(times)} keys: "
               f"least {min(times):.1f} ms, median {statistics.median(times):.1f} ms, "
               f"greatest {max(times):.1f} ms")
    print(summary, flush=True)

    # Ctrl+C on a foreground sleep: the shell has the serial port as its
    # controlling terminal, so SIGINT ends the sleep.
    type_text("sleep 100\r")
    time.sleep(1.0)
    key("ctrl", "c")
    time.sleep(0.3)
    type_text("echo interrupted-$((2+3))\r")
    wait_for("vmm guest: interrupted-5", within=10)
    time.sleep(0.5)
    command("screendump", {"filename": str(out / "04-ctrl-c.png"), "format": "png"})
    (out / "README.md").write_text(
        "# VMM-001 slice 1: Linux in a CuBit VM, its serial console in a CuBit window\n\n"
        "Taken by tests/headless/vm-desktop-shots.py from QEMU's scanout of a CuBit\n"
        "desktop (headless --test vm-desktop, nested SVM under KVM). The window is\n"
        "the vmm app's view of the guest's serial port (COM1), as the monitor\n"
        "receives it: the guest's serial console, not a graphics console.\n\n"
        "- 01-linux-booting.png: Linux 6.12's boot log as it arrives.\n"
        "- 02-shell-prompt-uname.png: the guest (two vCPUs) ran `uname -a` and\n"
        "  `head -5 /proc/cpuinfo`, printed its online CPU count and started a\n"
        "  shell; its prompt is the last line.\n"
        "- 03-typed.png: `echo typed-$((6*7))` typed in the window (QMP send-key\n"
        "  into the CuBit desktop), the guest's answer typed-42, the cursor after\n"
        "  the prompt.\n"
        "- 04-ctrl-c.png: `sleep 100`, Ctrl+C (the guest's ^C), and the next\n"
        "  command answered at once.\n")
    print("vm-desktop-shots: done PASS", flush=True)
    with open(serial, "a") as f:
        f.write(summary + "\n")
        f.write("vm-desktop-shots: done PASS\n")
    command("quit")
