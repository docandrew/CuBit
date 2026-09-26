"""Loopback-only TLS fixture for the tls-probe headless regression.

QEMU user networking maps guest 10.0.2.2:PORT to host 127.0.0.1:PORT. Each
port presents one certificate from tests/tls/build/pki (make-pki.sh):

  18460  good       valid chain for tls-test.cubit.internal
  18461  wronghost  valid chain for other.cubit.internal
  18462  untrusted  tls-test.cubit.internal from a root the guest lacks
  18463  expired    tls-test.cubit.internal, expired in 2025

On 18460 the guest must complete the handshake and send PING; the server
answers PONG. On the others the guest must abort the handshake. The server
is Python's ssl module (OpenSSL), not SPARKTLS, so this also checks
interoperability with an independent implementation.
"""
import socket
import ssl
import sys
import threading
import time
from pathlib import Path

PKI = Path(__file__).resolve().parent / "build" / "pki"
CASES = {18460: "good", 18461: "wronghost", 18462: "untrusted", 18463: "expired"}
results = {}
lock = threading.Lock()


def serve(port, name, stop):
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain(PKI / f"{name}.pem", PKI / f"{name}.key")
    with socket.socket() as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", port))
        listener.listen(1)
        listener.settimeout(0.5)
        while not stop.is_set():
            try:
                raw, _ = listener.accept()
            except socket.timeout:
                continue
            raw.settimeout(30)
            try:
                with context.wrap_socket(raw, server_side=True) as tls:
                    data = tls.recv(4)
                    if data == b"PING":
                        tls.sendall(b"PONG")
                        outcome = f"exchanged ({tls.version()}, {tls.cipher()[0]})"
                    else:
                        outcome = f"handshake completed, unexpected data {data!r}"
                    try:
                        tls.unwrap()
                    except (ssl.SSLError, OSError):
                        pass
            except (ssl.SSLError, OSError) as error:
                outcome = f"handshake aborted ({getattr(error, 'reason', None) or error})"
            with lock:
                results[port] = outcome
            print(f"tls fixture: {port} {name}: {outcome}", flush=True)


def main():
    serial_log = Path(sys.argv[1])
    test = sys.argv[2] if len(sys.argv) > 2 else "tls-probe"
    guest = "the guest (tls.svc)" if test == "tls-service" else "the guest"
    stop = threading.Event()
    threads = [threading.Thread(target=serve, args=(p, n, stop), daemon=True)
               for p, n in CASES.items()]
    for thread in threads:
        thread.start()
    deadline = time.monotonic() + 240
    while time.monotonic() < deadline:
        text = serial_log.read_text(errors="replace") if serial_log.exists() else ""
        if f"TEST: PASS {test}" in text or f"TEST: FAIL {test}" in text:
            break
        time.sleep(0.1)
    time.sleep(1)
    stop.set()
    for thread in threads:
        thread.join(timeout=2)
    errors = []
    if not results.get(18460, "").startswith("exchanged"):
        errors.append(f"valid case: {results.get(18460, 'no connection')}")
    for port in (18461, 18462, 18463):
        if not results.get(port, "").startswith("handshake aborted"):
            errors.append(f"{CASES[port]} case: {results.get(port, 'no connection')}")
    if errors:
        for error in errors:
            print(f"tls fixture: FAIL {error}", flush=True)
        return 1
    print("tls fixture: PASS (verified exchange with OpenSSL server; wrong host, "
          f"untrusted root and expired certificate all aborted by {guest})", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
