"""Loopback-only peer for the real CuBit outbound TCP policy regression."""
import socket
import sys
import time
from pathlib import Path

with socket.socket() as listener:
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    listener.bind(("127.0.0.1", 18443))
    listener.listen(1)
    listener.settimeout(120)
    # More cycles than both bounded tables: stale channel handles must not
    # resolve after reuse, and TCP reservations/grants must return capacity.
    for cycle in range(12):
        with listener.accept()[0] as connection:
            connection.settimeout(15)
            request = bytearray()
            while len(request) < 4:
                chunk = connection.recv(4 - len(request))
                if not chunk:
                    raise RuntimeError("guest closed before completing PING")
                request.extend(chunk)
            if request != b"PING":
                raise RuntimeError("unexpected guest payload")
            connection.sendall(b"PONG")
            connection.shutdown(socket.SHUT_WR)
            # Exercise guest FIN + host ACK rather than racing destruction of
            # the host socket against the next test connection.
            if connection.recv(1) != b"":
                raise RuntimeError("unexpected bytes after PING")
    print("network peer: PASS (12 channel lifetimes)", flush=True)

# QEMU's host forward is a loopback-only test fixture, not an application relay.
# Wait for guest markers rather than probing a closed port with real sessions.
serial_log = Path(sys.argv[1])
def wait_marker(marker):
    deadline = time.monotonic() + 90
    while marker not in serial_log.read_text(errors="replace"):
        if time.monotonic() >= deadline:
            raise TimeoutError(f"guest did not reach {marker}")
        time.sleep(0.05)


wait_marker("network-check: backlog-expiry ready")
abandoned = []
try:
    started = time.monotonic()
    for _ in range(2):
        connection = socket.create_connection(("127.0.0.1", 18444), timeout=5)
        abandoned.append(connection)
        connection.settimeout(6)
        connection.sendall(b"never accepted")
    for connection in abandoned:
        try:
            if connection.recv(1) != b"":
                raise RuntimeError("unaccepted connection received application data")
        except ConnectionResetError:
            pass
    if time.monotonic() - started >= 6.5:
        raise RuntimeError("backlog did not expire before listener closure")
finally:
    for connection in abandoned:
        connection.close()
print("network peer: PASS (unaccepted backlog deadline)", flush=True)

for cycle in range(1, 5):
    wait_marker(f"network-check: inbound ready {cycle}")
    with socket.create_connection(("127.0.0.1", 18444), timeout=15) as connection:
        connection.settimeout(15)
        connection.sendall(b"CuB")
        time.sleep(0.05)
        connection.sendall(b"itIPC")
        connection.shutdown(socket.SHUT_WR)
        response = bytearray()
        while True:
            chunk = connection.recv(32)
            if not chunk:
                break
            response.extend(chunk)
            if len(response) > 8:
                raise RuntimeError("oversized inbound reply")
        if response != b"ACCEPTED":
            raise RuntimeError(f"unexpected inbound reply: {response!r}")
print("network peer: PASS (4 native inbound accepts, fragmentation and half-close)", flush=True)
