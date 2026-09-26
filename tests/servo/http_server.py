#!/usr/bin/env python3
"""Plain-HTTP page fixture for the headless `servo` case.

Serves one test page on 127.0.0.1:18470, which the guest reaches as
10.0.2.2:18470 through QEMU's user network. Exits after the timeout.

    http_server.py <timeout-seconds>
"""
import http.server
import sys
import threading

PAGE = b"""<!doctype html>
<html><head><title>Servo over netstack</title></head>
<body style="background:#fff;margin:24px">
<h1 style="color:#135">Fetched over CuBit netstack</h1>
<p>This page came from the host through the CuBit libc's TCP sockets.</p>
<div style="width:300px;height:80px;background:#2a7"></div>
</body></html>
"""


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path != "/servo-test.html":
            self.send_error(404)
            return
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(PAGE)))
        self.end_headers()
        self.wfile.write(PAGE)
        print(f"servo-http: served {self.path}", flush=True)

    def log_message(self, *args):
        pass


def main():
    timeout = float(sys.argv[1]) if len(sys.argv) > 1 else 120
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 18470), Handler)
    threading.Timer(timeout, server.shutdown).start()
    server.serve_forever()


if __name__ == "__main__":
    main()
