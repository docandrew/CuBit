#!/usr/bin/env python3
"""Navigation and Resource Timing carry the real network phases (WEB-001).

    run-network-timing.py [--app HOSTED_CUBITSHELL]

Hosted cubitshell's compat mode loads a local page from one server and a
script from a second one (a second connection); both servers wait
FIRST_BYTE_DELAY_SECONDS before their response headers. Servo used to set
its connect marks as placeholders before connecting, never set
domainLookupEnd or responseStart, and left performance.timing's network
attributes at 0 (performance.measure with 'fetchStart' threw). Pass: in the
navigation entry, the resource entry and performance.timing, the time from
requestStart to responseStart covers the server's delay, the lookup and
connect marks are set and in order, and performance.measure accepts
'fetchStart'. The fix is a commit on Penny's Servo fork (userspace/servo/FORKS).
"""
import argparse
import http.server
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import threading
import time

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEADLINE_SECONDS = 120
FIRST_BYTE_DELAY_SECONDS = 0.4
# Timer coarsening and rounding: the measured wait may read this much short.
DELAY_TOLERANCE_MS = 20
DELAY_MS = FIRST_BYTE_DELAY_SECONDS * 1000 - DELAY_TOLERANCE_MS

PROBE = r"""(() => {
  const pick = (e, names) => Object.fromEntries(names.map((n) => [n, e ? e[n] : null]));
  const phases = ["fetchStart", "domainLookupStart", "domainLookupEnd", "connectStart",
                  "connectEnd", "secureConnectionStart", "requestStart", "responseStart", "responseEnd"];
  const navigation = performance.getEntriesByType("navigation")[0];
  const resource = performance.getEntriesByType("resource").find((e) => e.name.includes("/slow.js"));
  let measure = "ok";
  try { performance.measure("penny-fetch", "fetchStart", "responseEnd"); } catch (e) { measure = String(e); }
  return JSON.stringify({navigation: pick(navigation, phases), resource: pick(resource, phases),
                         timing: pick(performance.timing, ["navigationStart", ...phases]), measure});
})()"""


def handler(body_for):
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            time.sleep(FIRST_BYTE_DELAY_SECONDS)
            body, kind = body_for(self.path)
            self.send_response(200)
            self.send_header("Content-Type", kind)
            self.send_header("Content-Length", str(len(body)))
            self.send_header("Timing-Allow-Origin", "*")
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, *args):
            pass
    return Handler


def serve(body_for):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler(body_for))
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def ordered(entry, label, failures):
    """The lookup, connect and request marks are set and in order."""
    names = ["fetchStart", "domainLookupStart", "domainLookupEnd", "connectStart",
             "connectEnd", "requestStart", "responseStart", "responseEnd"]
    values = [entry.get(n) for n in names]
    # fetchStart may be 0: the navigation's time origin is its fetch's start.
    if values[0] is None or any(v is None or v <= 0 for v in values[1:]):
        failures.append(f"{label}: unset marks {dict(zip(names, values))}")
        return
    for (a, x), (b, y) in zip(zip(names, values), zip(names[1:], values[1:])):
        if y < x:
            failures.append(f"{label}: {b} {y} before {a} {x}")
    waited = entry["responseStart"] - entry["requestStart"]
    if waited < DELAY_MS:
        failures.append(f"{label}: requestStart to responseStart {waited} ms, server waited "
                        f"{FIRST_BYTE_DELAY_SECONDS * 1000:.0f} ms")


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    options = parser.parse_args()
    script_server = serve(lambda path: (b"window.slowLoaded = true;", "text/javascript"))
    page = (f"<!doctype html><title>timing</title><p>Network timing."
            f"<script src='http://localhost:{script_server.server_port}/slow.js'></script>").encode()
    page_server = serve(lambda path: (page, "text/html"))
    with tempfile.TemporaryDirectory(prefix="penny-timing-") as directory:
        flags = pathlib.Path(directory)
        (flags / "compat").write_text("")
        (flags / "log-level").write_text("error")
        (flags / "compat-probe").write_text("window.__probe = true;")
        (flags / "compat-plan").write_text(
            f"load\ttiming\t20000\thttp://localhost:{page_server.server_port}/\nwait\t500\n"
            f"js\tphases\t{PROBE.replace(chr(10), ' ')}\n")
        completed = subprocess.run([options.app], env=dict(os.environ, PENNY_FLAGS=str(flags)),
                                   capture_output=True, text=True, timeout=DEADLINE_SECONDS)
    page_server.shutdown()
    script_server.shutdown()
    log = completed.stdout + completed.stderr
    chunks = re.findall(r"PENNY-COMPAT js-ok timing phases \d+/\d+ (.*)", log)
    if not chunks:
        print("\n".join(line for line in log.splitlines() if "PENNY-COMPAT" in line or "panic" in line)[-2000:])
        print("NETWORK-TIMING: FAIL (no result)")
        return 1
    result = json.loads("".join(chunks))
    print(json.dumps(result, indent=1))
    failures = []
    ordered(result["navigation"], "navigation entry", failures)
    ordered(result["resource"], "resource entry", failures)
    timing = result["timing"]
    if not timing["fetchStart"] or not timing["navigationStart"]:
        failures.append(f"performance.timing: fetchStart {timing['fetchStart']}")
    elif timing["responseStart"] - timing["requestStart"] < DELAY_MS:
        failures.append(f"performance.timing: requestStart {timing['requestStart']} "
                        f"responseStart {timing['responseStart']}")
    if result["measure"] != "ok":
        failures.append(f"performance.measure('fetchStart'): {result['measure']}")
    for failure in failures:
        print("FAIL", failure)
    print("NETWORK-TIMING:", "FAIL" if failures else "PASS")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
