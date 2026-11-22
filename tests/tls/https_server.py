"""Loopback-only HTTPS fixture for the netsurf-https headless regression.

Serves one small HTML page over TLS 1.2/1.3 on 127.0.0.1:18460 (the guest's
10.0.2.2:18460) with the test certificate for tls-test.cubit.internal from
tests/tls/build/pki. NetSurf, built with this URL as its homepage, must fetch
it through tls.svc: the browser holds no network scope for this port and no
TLS code. Passes when a GET for / arrives over a verified TLS session with
the right Host header and the page is served.
"""
import http.server
import ssl
import sys
import threading
import time
from pathlib import Path

PKI = Path(__file__).resolve().parent / "build" / "pki"
PAGE = (b"<!DOCTYPE html><html><head><title>CuBit HTTPS</title></head>"
        b"<body><h1>Served over TLS to NetSurf via tls.svc</h1></body></html>")
seen = {"requests": [], "tls": None, "served": False}


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        seen["requests"].append((self.path, self.headers.get("Host"),
                                 self.headers.get("User-Agent", "")))
        if self.path != "/":
            self.send_error(404)
            return
        seen["tls"] = self.connection.version()
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(PAGE)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(PAGE)
        seen["served"] = True

    def log_message(self, fmt, *args):
        print("https fixture: " + fmt % args, flush=True)


def main():
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain(PKI / "good.pem", PKI / "good.key")
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 18460), Handler)
    server.socket = context.wrap_socket(server.socket, server_side=True)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    deadline = time.monotonic() + float(sys.argv[2] if len(sys.argv) > 2 else 200)
    while time.monotonic() < deadline and not seen["served"]:
        time.sleep(0.1)
    time.sleep(1)
    server.shutdown()
    pages = [r for r in seen["requests"] if r[0] == "/"]
    if not seen["served"] or not pages:
        print("https fixture: FAIL (NetSurf never fetched the page over TLS)", flush=True)
        return 1
    path, host, agent = pages[0]
    if host != "tls-test.cubit.internal:18460":
        print(f"https fixture: FAIL (Host header {host!r}; the port is required)", flush=True)
        return 1
    print(f"https fixture: PASS (NetSurf fetched / over {seen['tls']} with Host {host}; "
          f"User-Agent {agent!r})", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
