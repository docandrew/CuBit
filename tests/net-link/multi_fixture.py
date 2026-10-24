#!/usr/bin/env python3
"""Host side of the net-multi headless test (docs/net-link.md).

Usage: multi_fixture.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PCAP0 PCAP1

Two virtio-net devices: net0 (10.0.2.0/24, configured statically by the
system profile, metric 100) and net1 (10.0.3.0/24, no setting: netmgr's
default, DHCP, metric 200).

- Serves the guest's TCP stream on 127.0.0.1:18471 (the guest's
  10.0.3.2:18471 over net1): byte K is K mod 251.
- Checks both interfaces attach and are configured independently.
- Pulls net0 for 3 s: the stream over net1 must keep flowing, and the
  guest's datagrams to 192.0.2.1 (reached only by a default route) must
  leave by net1 while net0 is down and by net0 (lower metric) before and
  after (read from the two packet captures).
- Pulls net1 for 2 s: netmgr must re-validate its DHCP lease with
  INIT-REBOOT and be bound again, and the stream must survive.
- Ends the stream; the guest reports its own checks. Quits QEMU. Exit 0 only
  if every check holds.
"""
import re
import socket
import struct
import sys
import threading
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from qmp import QMP  # noqa: E402

STREAM_PORT = 18471
PATTERN_PERIOD = 251
PROBE_ADDRESS = bytes([192, 0, 2, 1])
ETHERTYPE_IPV4 = 0x0800
UDP = 17
SETTLE = 0.6          # seconds after a cable change before judging routes
FIRST_FLAP = 3.0
SECOND_FLAP = 2.0
MINIMUM_FLOW = 65_536  # bytes the stream must carry while net0 is down
STREAM_PACE = 0.05     # seconds between the stream's 16 KiB sends

serial_path = Path(sys.argv[1])
qmp_path = sys.argv[2]
deadline = time.monotonic() + float(sys.argv[3]) - 5
pcaps = [Path(sys.argv[4]), Path(sys.argv[5])]
failures = []
sent_log = []          # (time, total bytes sent) samples
stop_stream = threading.Event()


def log():
    try:
        return serial_path.read_text(errors="replace")
    except OSError:
        return ""


def wait_for(pattern, after=0, what=None):
    regex = re.compile(pattern)
    while time.monotonic() < deadline:
        found = regex.search(log(), after)
        if found:
            return found
        time.sleep(0.1)
    raise TimeoutError(f"guest never printed {what or pattern}")


def stream_server():
    block = bytes(i % PATTERN_PERIOD for i in range(PATTERN_PERIOD * 64))
    total = 0
    with socket.socket() as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", STREAM_PORT))
        listener.listen(1)
        listener.settimeout(150)
        connection, _ = listener.accept()
        connection.settimeout(1)
        with connection:
            while not stop_stream.is_set():
                try:
                    total += connection.send(block)
                    # About 300 KB/s: enough to show the stream flowing,
                    # little enough to keep net1's capture small.
                    time.sleep(STREAM_PACE)
                except socket.timeout:
                    pass
                except OSError:
                    return
                sent_log.append((time.time(), total))
            connection.shutdown(socket.SHUT_WR)
            time.sleep(1)


def sent_by(moment):
    best = 0
    for when, total in sent_log:
        if when <= moment:
            best = total
    return best


class ProbeCounter:
    """Counts datagrams to the probe address in a growing pcap file, reading
    only what was appended since the last look (QEMU's capture clock is not
    the fixture's, so the fixture compares counts taken at its own times)."""

    def __init__(self, path):
        self.path = path
        self.offset = 24
        self.order = None
        self.count = 0

    def update(self):
        try:
            with self.path.open("rb") as capture:
                if self.order is None:
                    magic = capture.read(4)
                    if len(magic) < 4:
                        return self.count
                    self.order = "<" if magic in (b"\xd4\xc3\xb2\xa1", b"\x4d\x3c\xb2\xa1") else ">"
                capture.seek(self.offset)
                while True:
                    header = capture.read(16)
                    if len(header) < 16:
                        break
                    _, _, captured, _ = struct.unpack(self.order + "IIII", header)
                    frame = capture.read(captured)
                    if len(frame) < captured:
                        break
                    self.offset += 16 + captured
                    if len(frame) >= 34 and struct.unpack(">H", frame[12:14])[0] == ETHERTYPE_IPV4 \
                       and frame[23] == UDP and frame[30:34] == PROBE_ADDRESS:
                        self.count += 1
        except OSError:
            pass
        return self.count


threading.Thread(target=stream_server, daemon=True).start()
counters = [ProbeCounter(pcaps[0]), ProbeCounter(pcaps[1])]


def counts():
    return counters[0].update(), counters[1].update()


qmp = None
try:
    qmp = QMP(qmp_path)
    configured = wait_for(r"netstack: (\S+) configured: 10\.0\.2\.15/24", 0, "net0's static address")
    name0 = configured.group(1)
    dhcp = wait_for(r"netstack: (\S+) configured: 10\.0\.3\.\d+/24", 0, "net1's DHCP address")
    name1 = dhcp.group(1)
    if name0 == name1:
        failures.append("the two interfaces have one name")
    wait_for(rf"netmgr: {re.escape(name1)} DHCP bound ", 0, "net1's DHCP lease")
    running = wait_for(r"net-multi-check: running", 0, "the guest's stream").start()

    # Default route failover: pull net0. Probe counts on each interface,
    # taken at the fixture's own times: before, during and after.
    start0, start1 = counts()
    time.sleep(1.5)
    pre0, pre1 = counts()
    qmp.set_link("net0", False)
    pulled = time.time()
    wait_for(rf"netstack: {re.escape(name0)} link Down", running, "net0's link Down")
    time.sleep(SETTLE)
    down0, down1 = counts()
    time.sleep(FIRST_FLAP)
    plugged = time.time()
    late0, late1 = counts()
    qmp.set_link("net0", True)
    wait_for(rf"netstack: {re.escape(name0)} link Up", running, "net0's link Up")
    time.sleep(SETTLE)
    up0, up1 = counts()
    time.sleep(2.0)
    end0, end1 = counts()
    if sent_by(plugged) - sent_by(pulled + SETTLE) < MINIMUM_FLOW:
        failures.append("the stream over net1 did not keep flowing while net0 was down")

    # DHCP re-validation: pull net1.
    mark = len(log())
    qmp.set_link("net1", False)
    wait_for(rf"netstack: {re.escape(name1)} link Down", mark, "net1's link Down")
    time.sleep(SECOND_FLAP)
    qmp.set_link("net1", True)
    wait_for(rf"netmgr: {re.escape(name1)} DHCP INIT-REBOOT", mark, "netmgr's INIT-REBOOT")
    wait_for(rf"netmgr: {re.escape(name1)} DHCP bound ", mark, "the re-validated lease")
    time.sleep(2.0)
    flowed = sent_by(time.time())
    time.sleep(2.0)
    if sent_by(time.time()) <= flowed:
        failures.append("the stream did not survive net1's flap")
    stop_stream.set()
    wait_for(r"TEST: (PASS|FAIL) net-multi", running, "the guest's verdict")
    time.sleep(0.5)

    if pre0 - start0 < 5:
        failures.append("before the flap, probes did not leave by net0")
    if pre1 - start1 > 0:
        failures.append("before the flap, probes left by net1 (net0 has the lower metric)")
    if late1 - down1 < 10:
        failures.append("while net0 was down, probes did not fail over to net1")
    if late0 - down0 > 0:
        failures.append("while net0 was down, probes still went to net0")
    if end0 - up0 < 5 or end1 - up1 > 0:
        failures.append("after net0 came back, probes did not return to it")
    text = log()
    if "TEST: PASS net-multi" not in text:
        failures.append("the guest's checks failed")
except Exception as error:  # reported below
    failures.append(str(error))
finally:
    stop_stream.set()
    if qmp is not None:
        qmp.quit()

for failure in failures:
    print(f"net-multi fixture: FAIL: {failure}", flush=True)
if not failures:
    print("net-multi fixture: PASS", flush=True)
sys.exit(1 if failures else 0)
