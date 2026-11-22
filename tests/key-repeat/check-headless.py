#!/usr/bin/env python3
"""Keyboard auto-repeat, end to end (headless test key-repeat).

Holds keys through QEMU's real PS/2 keyboard (QMP input-send-event: a press,
a wait, a release; QEMU itself never repeats) and reads clipboard-check's
copy markers, which report its text field's length:
  * hold: holding "a" for HOLD_S fills the field with about
    1 + (HOLD_S - delay) * rate characters (Desktop's repeats);
  * release stops: a second copy a second later reports the same length;
  * dedupe: a second press without a release (what PS/2 typematic and a VM
    host's auto-repeat send) adds nothing; a short tap adds one character.
Which keys repeat toward which destinations is covered by the hosted tests.
The delay and rate are the defaults (desktop.input.key-repeat unset).
Arguments: serial-log qmp-socket timeout-seconds.
Quits QEMU when done; prints KEY-REPEAT: PASS or raises."""
import json
import re
import socket
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def default(name):
    text = (ROOT / "userspace/ccl/src/ccl-interfaces-desktop_input.ads").read_text()
    m = re.search(name + r" : constant := ([\d_]+);", text)
    if m is None:
        raise RuntimeError(f"{name} not found")
    return int(m.group(1).replace("_", ""))


DELAY_S = default("DEFAULT_DELAY_MS") / 1000
RATE_HZ = default("DEFAULT_RATE_HZ")
HOLD_S = 1.5
EXPECTED = 1 + (HOLD_S - DELAY_S) * RATE_HZ      # 31 at 500 ms / 30 Hz
# QEMU scheduling and the PS/2 path blur the edges; the bounds still tell
# no repeat (1), PS/2-rate repeat (~11) and runaway apart.
LOW, HIGH = EXPECTED * 0.6, EXPECTED * 1.4


def require(condition, explanation):
    if not condition:
        raise RuntimeError(explanation)


serial_path, qmp_path, timeout = sys.argv[1:4]
deadline = time.monotonic() + float(timeout) - 5
serial = Path(serial_path)


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(marker, after=0):
    while True:
        text = log_text()
        index = text.find(marker, after)
        if index >= 0:
            return index + len(marker)
        require(time.monotonic() < deadline, f"timed out waiting for {marker!r}")
        time.sleep(0.1)


while not Path(qmp_path).exists():
    require(time.monotonic() < deadline, "QMP socket deadline")
    time.sleep(0.05)
qmp = socket.socket(socket.AF_UNIX)
qmp.settimeout(5)
qmp.connect(qmp_path)
stream = qmp.makefile("rwb")
require("QMP" in json.loads(stream.readline()), "QMP greeting")


def command(name, arguments=None):
    request = {"execute": name}
    if arguments is not None:
        request["arguments"] = arguments
    stream.write(json.dumps(request).encode() + b"\n")
    stream.flush()
    while True:
        response = json.loads(stream.readline())
        require("error" not in response, str(response))
        if "return" in response:
            return response["return"]


def key(code, down):
    command("input-send-event", {"events": [
        {"type": "key", "data": {"down": down, "key": {"type": "qcode", "data": code}}}]})


def tap(*codes):
    """A chord: press in order, release in reverse."""
    for code in codes:
        key(code, True)
        time.sleep(0.03)
    for code in reversed(codes):
        key(code, False)
        time.sleep(0.03)
    time.sleep(0.15)


def field_length():
    """Ctrl+C: clipboard-check reports its field's length (copied or not)."""
    after = len(log_text())
    tap("ctrl", "c")
    end = wait_for("clipboard-check: copy field=name outcome=", after)
    line = log_text()[end:].split("\n", 1)[0]
    m = re.search(r"length= (\d+)", line)
    require(m is not None, f"no length in {line!r}")
    return int(m.group(1))


command("qmp_capabilities")
try:
    wait_for("clipboard-check: ready")
    wait_for("clipboard-check: ungestured paste denied")
    time.sleep(0.5)
    require(field_length() == 0, "the field is not empty or not focused")

    key("a", True)
    time.sleep(HOLD_S)
    key("a", False)
    held = field_length()
    print(f"key-repeat: held {HOLD_S}s -> {held} characters (expected ~{EXPECTED:.0f})", flush=True)
    require(LOW <= held <= HIGH, f"held key gave {held} characters, expected {LOW:.0f}..{HIGH:.0f}")

    time.sleep(1.0)
    require(field_length() == held, "characters kept arriving after the release")

    tap("b")
    require(field_length() == held + 1, "a tap did not type exactly one character")
    # A device repeat (a second make, no break) is dropped; the key stays
    # held for less than the delay, so Desktop adds nothing either.
    key("c", True)
    time.sleep(0.1)
    key("c", True)
    time.sleep(0.1)
    key("c", False)
    time.sleep(0.2)
    require(field_length() == held + 2, "a device repeat was not dropped")

    tap("esc")
    wait_for("clipboard-check: closed")
    print("KEY-REPEAT: PASS", flush=True)
finally:
    try:
        command("quit")
    except Exception:
        pass
