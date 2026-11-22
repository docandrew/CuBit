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

BROWSER_A = b"""<!doctype html><meta charset="utf-8">
<title>CuBitBrowserA</title>
<body style="background:white;margin:24px;font:18px sans-serif"
 onkeydown="if(event.key==='Escape')document.title='CuBitBrowserRestoredA';if(event.key==='F2')document.title='CuBitBrowserRetained:'+document.getElementById('entry').value">
<h1>Servo browser input</h1>
<p>The native address field navigated to this page.</p>
<input id="entry" aria-label="Browser input test"
 style="position:absolute;left:24px;top:136px;width:200px;height:24px;box-sizing:border-box"
 onclick="document.title='CuBitBrowserPointerFocus'"
 oninput="document.title='CuBitBrowserTyped:'+this.value">
<p><a href="/browser-b">Visit the second page</a></p>
<div style="width:300px;height:80px;background:#2a7"></div>
"""
BROWSER_B = b"""<!doctype html><meta charset="utf-8">
<title>CuBitBrowserB</title>
<body style="background:white;margin:24px;font:18px sans-serif;min-height:1600px"
 onkeydown="if(event.key==='Escape')document.title='CuBitBrowserRestoredB'">
<h1>History and reload</h1><p>Second page in browser navigation regression.</p>
<div style="width:300px;height:80px;background:#37c"></div>
<script>
onwheel=event=>{document.title='CuBitBrowserWheel:'+Math.sign(event.deltaY);};
onscroll=()=>{if(scrollY>0)document.title='CuBitBrowserScrolled';};
onresize=()=>{document.title='CuBitBrowserResize:'+innerWidth+'x'+innerHeight;};
</script>
"""


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        page = {"/servo-test.html": PAGE, "/browser-a": BROWSER_A, "/browser-b": BROWSER_B}.get(self.path)
        if page is None:
            self.send_error(404)
            return
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(page)))
        self.end_headers()
        self.wfile.write(page)
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
