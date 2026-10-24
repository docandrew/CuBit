#!/usr/bin/env python3
"""Host side of the net-link headless test (docs/net-link.md).

Usage: fixture.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS

- Serves the guest's TCP stream (127.0.0.1:18460, the guest's 10.0.2.2:18460):
  byte K is K mod 251, sent until the guest closes.
- Echoes UDP on 18461: PING -> PONG.
- Once the guest says it is transferring, pulls the virtual cable (QMP
  set_link net0 off) for FLAP_SECONDS, then plugs it back.
- Checks, from the serial log, that netstack and netmgr each logged the
  link going Down and coming Up once, that the guest's UDP send saw
  Unreachable while the link was down, that the static profile was applied
  again and IPv6 detection ran again after Up, and that the guest's TCP
  stream survived. Then quits QEMU. Exit status 0 only if every check holds.
"""
import re
import socket
import sys
import threading
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from qmp import QMP  # noqa: E402

STREAM_PORT = 18460
ECHO_PORT = 18461
PATTERN_PERIOD = 251
FLAP_SECONDS = 2.0
NETDEV = "net0"

serial_path = Path(sys.argv[1])
qmp_path = sys.argv[2]
deadline = time.monotonic() + float(sys.argv[3]) - 5
failures = []


def log():
    try:
        return serial_path.read_text(errors="replace")
    except OSError:
        return ""


def wait_for(pattern, after=0, what=None):
    """Wait for a regex in the log at or after offset 'after'; its offset."""
    regex = re.compile(pattern)
    while time.monotonic() < deadline:
        found = regex.search(log(), after)
        if found:
            return found.start()
        time.sleep(0.1)
    raise TimeoutError(f"guest never printed {what or pattern}")


def stream_server():
    block = bytes(i % PATTERN_PERIOD for i in range(PATTERN_PERIOD * 64))
    with socket.socket() as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", STREAM_PORT))
        listener.listen(1)
        listener.settimeout(120)
        connection, _ = listener.accept()
        with connection:
            # block is a whole number of periods: the pattern carries on.
            try:
                while True:
                    connection.sendall(block)
            except OSError:
                pass  # the guest closed


def echo_server():
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
        peer.bind(("127.0.0.1", ECHO_PORT))
        peer.settimeout(1)
        while time.monotonic() < deadline:
            try:
                data, guest = peer.recvfrom(64)
            except socket.timeout:
                continue
            if data == b"PING":
                peer.sendto(b"PONG", guest)


threading.Thread(target=stream_server, daemon=True).start()
threading.Thread(target=echo_server, daemon=True).start()

qmp = None
try:
    qmp = QMP(qmp_path)
    start = wait_for(r"net-link-check: transferring")
    qmp.set_link(NETDEV, False)
    pulled = time.monotonic()
    down = wait_for(r"netstack: (\S+) link Down reason=No_Carrier", start, "netstack's link Down")
    name = re.compile(r"netstack: (\S+) link Down").search(log(), start).group(1)
    time.sleep(max(0.0, FLAP_SECONDS - (time.monotonic() - pulled)))
    qmp.set_link(NETDEV, True)
    up = wait_for(rf"netstack: {re.escape(name)} link Up", down, "netstack's link Up")
    wait_for(r"TEST: (PASS|FAIL) net-link", start, "the guest's verdict")
    time.sleep(0.5)
    text = log()

    def count(pattern, start_at=0, end_at=None):
        return len(re.findall(pattern, text[start_at:end_at]))

    unreachable = text.find("net-link-check: udp send while down is unreachable PASS", start)
    if not down <= unreachable:
        failures.append("the guest's UDP send did not end Unreachable after the link went down")
    if count(rf"netstack: {re.escape(name)} link Down") != 1:
        failures.append("netstack did not log Down exactly once")
    if count(rf"netstack: {re.escape(name)} link Up", start) != 1:
        failures.append("netstack did not log Up exactly once after the flap")
    if count(rf"netmgr: {re.escape(name)} link Down", start) != 1:
        failures.append("netmgr did not log Down exactly once")
    if count(rf"netmgr: {re.escape(name)} link Up", start) != 1:
        failures.append("netmgr did not log Up exactly once after the flap")
    if count(rf"netstack: {re.escape(name)} configured: ", up) < 1:
        failures.append("the static settings were not applied again after Up")
    if count(r"netstack: IPv6 link-local ", up) < 1:
        failures.append("IPv6 duplicate address detection did not run again after Up")
    # One line per transition, readable from a screen photo
    # (coordination/release-stack.md): name, kind, MAC, old and new state,
    # generation, reason; and what netmgr applied when the link came back.
    line = r"netstack: {0} link {1} reason=\S+ was={2} kind=Ethernet mac=([0-9a-f]{{2}}:){{5}}[0-9a-f]{{2}} gen=\d+"
    if not re.search(line.format(re.escape(name), "Down", "Up"), text):
        failures.append("the Down line lacks the bring-up fields")
    if not re.search(line.format(re.escape(name), "Up", "Down"), text[up - 1:]):
        failures.append("the Up line lacks the bring-up fields")
    mline = r"netmgr: {0} link {1} reason=\S+ was={2} kind=Ethernet mac=([0-9a-f]{{2}}:){{5}}[0-9a-f]{{2}} gen=\d+"
    if not re.search(mline.format(re.escape(name), "Down", "Up"), text) or \
       not re.search(mline.format(re.escape(name), "Up", "Down"), text[up - 1:]):
        failures.append("netmgr's transition lines lack the bring-up fields")
    if count(rf"netmgr: {re.escape(name)} applied method=static address=\S+ gateway=\S+ metric=\d+ routes=", up) < 1:
        failures.append("netmgr did not log what it applied after Up")
    if "TEST: PASS net-link" not in text:
        failures.append("the guest's checks failed")
except Exception as error:  # reported below
    failures.append(str(error))
finally:
    if qmp is not None:
        qmp.quit()

for failure in failures:
    print(f"net-link fixture: FAIL: {failure}", flush=True)
if not failures:
    print("net-link fixture: PASS", flush=True)
sys.exit(1 if failures else 0)
