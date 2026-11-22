#!/usr/bin/env python3
"""Penny's User-Agent presentation: the header and the navigator agree.

    run-user-agent.py [--app HOSTED_CUBITSHELL]

Hosted cubitshell's compat mode loads local pages under each presentation
(the hosted harness's flag files `user-agent` and `site-agents`; on CuBit the
same choice comes from the typed settings browser.servo.user-agent and
browser.servo.site-agent.<host>). The server records the User-Agent header
of each document and of its image; the page reports navigator.userAgent,
navigator.platform and a dedicated worker's navigator.userAgent. Pass:
- with no choice, everything is Penny's own CuBit User-Agent and platform;
- for each presentation (penny, linux, windows, mac), the header of the
  document and of its image, navigator.userAgent and the worker's are the
  expected string, and navigator.platform the expected platform;
- per host: with every site Windows and localhost Mac, a page from
  localhost is Mac and one from 127.0.0.1 is Windows, header and navigator;
- a flag that is not a presentation leaves Penny's own in force.
The expected strings are written out here, not taken from the code under
test (only Servo's version is read, from the pinned checkout).
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
import tomllib

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEADLINE_SECONDS = 120
LOAD_DEADLINE_MS = 20000
SETTLE_MS = 1500
FIREFOX = "153.0"
PLATFORMS = {"penny": "CuBit", "linux": "Linux x86_64", "windows": "Win32", "mac": "MacIntel"}


def expected_agents(servo_version):
    gecko = f"rv:{FIREFOX}) Gecko/20100101 Firefox/{FIREFOX}"
    return {
        "penny": f"Mozilla/5.0 (CuBit; x86_64; rv:{FIREFOX}) Servo/{servo_version} Firefox/{FIREFOX}",
        "linux": f"Mozilla/5.0 (X11; Linux x86_64; {gecko}",
        "windows": f"Mozilla/5.0 (Windows NT 10.0; Win64; x64; {gecko}",
        "mac": f"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; {gecko}",
    }


REPORT = ("JSON.stringify({ua: navigator.userAgent, platform: navigator.platform, "
          "worker: window.__workerAgent || null})")
WORKER = b"postMessage(navigator.userAgent);"


class Server:
    """Pages, images and the worker script; each request's User-Agent, by path."""

    def __init__(self):
        self.seen = {}
        seen = self.seen

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_GET(self):
                seen[self.path] = self.headers.get("User-Agent")
                if self.path.startswith("/page-"):
                    tag = self.path.removeprefix("/page-")
                    body = (f"<!doctype html><title>{tag}</title><p>User-Agent check {tag}."
                            f"<img src='/image-{tag}.png'>"
                            "<script>const w = new Worker('/worker.js');"
                            "w.onmessage = (e) => { window.__workerAgent = e.data; };</script>").encode()
                    kind = "text/html"
                elif self.path == "/worker.js":
                    body, kind = WORKER, "text/javascript"
                else:
                    body, kind = b"", "image/png"
                self.send_response(200)
                self.send_header("Content-Type", kind)
                self.send_header("Content-Length", str(len(body)))
                self.send_header("Cache-Control", "no-store")
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, *args):
                pass

        self.http = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.http.daemon_threads = True
        threading.Thread(target=self.http.serve_forever, daemon=True).start()
        self.port = self.http.server_port


def run(app, flags, loads):
    """One cubitshell process over loads [(tag, url)]: tag -> report."""
    with tempfile.TemporaryDirectory(prefix="penny-agent-") as directory:
        folder = pathlib.Path(directory)
        (folder / "compat").write_text("")
        (folder / "log-level").write_text("error")
        (folder / "compat-probe").write_text("window.__probe = true;")
        for name, value in flags.items():
            (folder / name).write_text(value)
        plan = "".join(f"load\t{tag}\t{LOAD_DEADLINE_MS}\t{url}\nwait\t{SETTLE_MS}\njs\treport\t{REPORT}\n"
                       for tag, url in loads)
        (folder / "compat-plan").write_text(plan)
        completed = subprocess.run([app], env=dict(os.environ, PENNY_FLAGS=str(folder)),
                                   capture_output=True, text=True, timeout=DEADLINE_SECONDS)
    log = completed.stdout + completed.stderr
    reports = {}
    for tag, _ in loads:
        chunks = re.findall(rf"PENNY-COMPAT js-ok {re.escape(tag)} report \d+/\d+ (.*)", log)
        reports[tag] = json.loads("".join(chunks)) if chunks else None
    return reports, log


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    parser.add_argument("--servo", default=str(ROOT / "userspace/rust/build/servo-work/servo"))
    options = parser.parse_args()
    servo_version = tomllib.loads((pathlib.Path(options.servo) / "Cargo.toml").read_text())["workspace"]["package"]["version"]
    agents = expected_agents(servo_version)
    server = Server()
    failures = []

    def check(label, report, tag, presentation):
        want_agent, want_platform = agents[presentation], PLATFORMS[presentation]
        got = {"document header": server.seen.get(f"/page-{tag}"),
               "image header": server.seen.get(f"/image-{tag}.png"),
               "navigator.userAgent": (report or {}).get("ua"),
               "worker navigator.userAgent": (report or {}).get("worker")}
        for what, value in got.items():
            if value != want_agent:
                failures.append(f"{label}: {what} {value!r}, want {want_agent!r}")
        platform = (report or {}).get("platform")
        if platform != want_platform:
            failures.append(f"{label}: navigator.platform {platform!r}, want {want_platform!r}")
        print(f"{label}: {got['document header']!r} platform {platform!r}")

    local = f"http://127.0.0.1:{server.port}"
    named = f"http://localhost:{server.port}"
    cases = [("default", {}, "penny")] + [(name, {"user-agent": name}, name) for name in PLATFORMS] + \
        [("not-a-presentation", {"user-agent": "Mozilla/5.0 (free text)"}, "penny")]
    for label, flags, presentation in cases:
        tag = label.replace("-", "")
        reports, log = run(options.app, flags, [(tag, f"{local}/page-{tag}")])
        if reports[tag] is None:
            print("\n".join(line for line in log.splitlines() if "PENNY-COMPAT" in line or "panic" in line)[-1500:])
        check(label, reports[tag], tag, presentation)
    # Per host: every site Windows, localhost (and its subdomains) Mac.
    reports, log = run(options.app, {"user-agent": "windows", "site-agents": "localhost mac\n"},
                       [("sitenamed", f"{named}/page-sitenamed"), ("siteaddress", f"{local}/page-siteaddress")])
    check("per-site localhost", reports["sitenamed"], "sitenamed", "mac")
    check("per-site 127.0.0.1", reports["siteaddress"], "siteaddress", "windows")
    server.http.shutdown()
    for failure in failures:
        print("FAIL", failure)
    print("USER-AGENT:", "FAIL" if failures else "PASS")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
