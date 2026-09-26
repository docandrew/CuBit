"""Loopback-only SNTP fixture for the timesync headless regression.

QEMU user networking maps guest traffic for 10.0.2.2:PORT to the host's
127.0.0.1:PORT. Three servers answer on 18123-18125:

* 18123 and 18124 are honest: host UTC plus OFFSET seconds.
* 18125 is a falseticker: host UTC plus OFFSET plus one hour.

The guest must adopt the honest majority. The fixture checks every request
(a well-formed SNTPv4 client packet with a fresh, nonzero transmit nonce),
then compares the UTC timesync-check reports against host time + OFFSET.
Unprivileged processes cannot bind port 123, which is why the test runs the
timesync-test.svc variant whose only difference is this fixture scope.
"""
import socket
import struct
import sys
import threading
import time
from pathlib import Path

OFFSET = 300  # within clock.svc's 15-minute unauthenticated step bound
FALSETICKER = OFFSET + 3600
NTP_UNIX = 2_208_988_800
TOLERANCE = 5

errors = []
nonces = set()
requests = {18123: 0, 18124: 0, 18125: 0}
lock = threading.Lock()


def ntp_stamp(unix_seconds):
    seconds = int(unix_seconds) + NTP_UNIX
    fraction = int((unix_seconds % 1) * (1 << 32))
    return (seconds << 32) | fraction


def serve(port, offset, stop):
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as server:
        server.bind(("127.0.0.1", port))
        server.settimeout(0.2)
        while not stop.is_set():
            try:
                data, peer = server.recvfrom(2048)
            except socket.timeout:
                continue
            received = time.time() + offset
            if len(data) != 48 or data[0] != 0x23:
                with lock:
                    errors.append(f"port {port}: malformed request {data[:4].hex()}")
                continue
            nonce = struct.unpack("!Q", data[40:48])[0]
            if any(data[1:40]):
                with lock:
                    errors.append(f"port {port}: nonzero request header fields")
            with lock:
                if nonce == 0 or nonce in nonces:
                    errors.append(f"port {port}: zero or reused nonce {nonce:#x}")
                nonces.add(nonce)
                requests[port] += 1
            reply = bytearray(48)
            reply[0] = 0x24  # LI 0, VN 4, mode 4 (server)
            reply[1] = 2  # stratum
            reply[2] = 6  # poll
            reply[3] = 0xEC  # precision ~ 2^-20 s
            struct.pack_into("!I", reply, 4, 0x0000_0010)  # root delay
            struct.pack_into("!I", reply, 8, 0x0000_0010)  # root dispersion
            reply[12:16] = b"TEST"
            struct.pack_into("!Q", reply, 16, ntp_stamp(received))
            struct.pack_into("!Q", reply, 24, nonce)  # origin = client nonce
            struct.pack_into("!Q", reply, 32, ntp_stamp(received))
            struct.pack_into("!Q", reply, 40, ntp_stamp(time.time() + offset))
            server.sendto(bytes(reply), peer)


def main():
    serial_log = Path(sys.argv[1])
    stop = threading.Event()
    threads = [
        threading.Thread(target=serve, args=(18123, OFFSET, stop), daemon=True),
        threading.Thread(target=serve, args=(18124, OFFSET, stop), daemon=True),
        threading.Thread(target=serve, args=(18125, FALSETICKER, stop), daemon=True),
    ]
    for thread in threads:
        thread.start()
    deadline = time.monotonic() + 150
    reported = None
    while time.monotonic() < deadline:
        text = serial_log.read_text(errors="replace") if serial_log.exists() else ""
        for line in text.splitlines():
            if line.startswith("timesync-check: utc="):
                reported = int(line.split("=", 1)[1].strip())
        if reported is not None:
            break
        time.sleep(0.05)
    expected = time.time() + OFFSET
    stop.set()
    for thread in threads:
        thread.join(timeout=2)
    if reported is None:
        errors.append("guest never reported synchronized UTC")
    elif abs(reported - expected) > TOLERANCE:
        errors.append(f"guest UTC {reported} differs from fixture time "
                      f"{expected:.0f} by {reported - expected:+.0f} s")
    if min(requests.values()) < 1:
        errors.append(f"not every server was queried: {requests}")
    if errors:
        for error in errors:
            print(f"timesync fixture: {error}", flush=True)
        return 1
    print(f"timesync fixture: PASS (majority adopted, falseticker outvoted, "
          f"guest UTC within {TOLERANCE} s of host+{OFFSET} s, {len(nonces)} unique nonces)",
          flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
