#!/usr/bin/env python3
"""Embedder scripts on a document that cannot run script (WEB-001).

    run-embedder-script-sandbox.py [--app HOSTED_CUBITSHELL]

A local page served with `Content-Security-Policy: sandbox` (scripting
disabled) is loaded by hosted cubitshell's compat mode with a user script
installed, then the embedder evaluates a script on it. Before the fix
(userspace/servo/embedder_scripts.py) the script thread asserted
can_run_script and the content process panicked; now the user script is not
run there and the evaluation reports an error. A second, ordinary page
checks that evaluation still works.
"""
import argparse
import functools
import http.server
import os
import pathlib
import subprocess
import sys
import tempfile
import threading

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEADLINE_SECONDS = 120
PAGE = b"<!doctype html><title>sandboxed</title><p>No script runs here.</p>"


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        if self.path.startswith("/sandboxed"):
            self.send_header("Content-Security-Policy", "sandbox")
        self.send_header("Content-Length", str(len(PAGE)))
        self.end_headers()
        self.wfile.write(PAGE)

    def log_message(self, *args):
        pass


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    options = parser.parse_args()
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    base = f"http://127.0.0.1:{server.server_port}"
    with tempfile.TemporaryDirectory(prefix="penny-sandbox-") as directory:
        flags = pathlib.Path(directory)
        (flags / "compat").write_text("")
        (flags / "log-level").write_text("error")
        (flags / "compat-probe").write_text("window.__probe = true;")
        (flags / "compat-plan").write_text(
            f"load\tsandboxed\t10000\t{base}/sandboxed\nwait\t300\njs\tcheck\t'ran'\n"
            f"load\tordinary\t10000\t{base}/ordinary\nwait\t300\njs\tcheck\tString(window.__probe)\n")
        completed = subprocess.run([options.app], env=dict(os.environ, PENNY_FLAGS=str(flags)),
                                   capture_output=True, text=True, timeout=DEADLINE_SECONDS)
    server.shutdown()
    log = completed.stdout + completed.stderr
    checks = {
        "no panic": "panicked" not in log,
        "sandboxed evaluation refused": "PENNY-COMPAT js-error sandboxed check" in log,
        "ordinary evaluation with the user script": "PENNY-COMPAT js-ok ordinary check 1/1 true" in log,
        "plan finished": "PENNY-COMPAT done" in log,
    }
    for name, ok in checks.items():
        print(f"{'ok  ' if ok else 'FAIL'} {name}")
    if all(checks.values()):
        print("EMBEDDER-SCRIPT-SANDBOX: PASS")
        return 0
    print("\n".join(line for line in log.splitlines() if "PENNY-COMPAT" in line or "panic" in line)[-2000:])
    print("EMBEDDER-SCRIPT-SANDBOX: FAIL")
    return 1


if __name__ == "__main__":
    sys.exit(main())
