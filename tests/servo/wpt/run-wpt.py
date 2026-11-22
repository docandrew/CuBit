#!/usr/bin/env python3
"""A subset of web-platform-tests in hosted cubitshell (testharness.js tests).

    run-wpt.py --app HOSTED_CUBITSHELL --out RESULTS.json TEST_DIR_OR_FILE...
    run-wpt.py --compare BEFORE.json AFTER.json

Serves the Servo fork's tests/wpt/tests (the WPT revision Servo pins) from a
local server (127.0.0.1: a secure context, as WPT's https tests need) and
loads each test page in cubitshell's compat mode, one process per test file.
.any.js and .window.js files get the wrapper wptserve would generate (their
META scripts included); .html files load as they are. A testharnessreport.js
replacement keeps the harness's results in the page, and the plan reads them
after the test's completion or its time limit (TIMEOUT_MS, long: LONG_MS).
Reftests, workers-only variants and wptserve's other handlers are not run.

RESULTS.json maps each test file to its harness status and subtests
({name: status}). --compare prints the subtest pass counts of both and
every subtest that passes in one and not the other.
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

ROOT = pathlib.Path(__file__).resolve().parents[3]
WPT = pathlib.Path(os.environ.get("SERVO_DIR", ROOT / "userspace/rust/build/servo-work/servo")) / "tests/wpt/tests"
TIMEOUT_MS = 20000
LONG_MS = 90000
LOAD_DEADLINE_MS = 30000
# testharness.js's results, kept for the plan's script to read.
REPORT = b"""
window.__wpt = null;
add_completion_callback(function (tests, status) {
  window.__wpt = JSON.stringify({status: status.status, message: status.message,
    tests: tests.map(function (t) { return {name: t.name, status: t.status, message: t.message}; })});
});
"""
STATUS = {0: "PASS", 1: "FAIL", 2: "TIMEOUT", 3: "NOTRUN", 4: "PRECONDITION_FAILED"}
HARNESS = {0: "OK", 1: "ERROR", 2: "TIMEOUT", 3: "PRECONDITION_FAILED"}


def wrapper(path):
    """The page wptserve generates for X.any.js / X.window.js (window scope)."""
    source = path.read_text(errors="replace")
    metas = re.findall(r"^// META: (\w+)=(.*)$", source, re.M)
    scripts = "".join(f'<script src="{v.strip()}"></script>\n' for k, v in metas if k == "script")
    title = next((v for k, v in metas if k == "title"), path.name)
    return (f"<!doctype html><meta charset=utf-8><title>{title}</title>\n"
            "<script src=/resources/testharness.js></script>\n"
            "<script src=/resources/testharnessreport.js></script>\n"
            f"{scripts}<script src=\"{path.name}\"></script>\n").encode()


def is_long(path):
    return "timeout=long" in path.read_text(errors="replace") or 'name="timeout" content="long"' in path.read_text(errors="replace")


def serve():
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            route = self.path.split("?")[0]
            if route == "/resources/testharnessreport.js":
                body, kind = REPORT, "text/javascript"
            else:
                wrapped = re.match(r"^(.*)\.(any|window)\.html$", route)
                target = WPT / route.lstrip("/")
                if wrapped:
                    source = WPT / (wrapped.group(1).lstrip("/") + f".{wrapped.group(2)}.js")
                    if not source.is_file():
                        self.send_error(404)
                        return
                    body, kind = wrapper(source), "text/html; charset=utf-8"
                elif target.is_file() and WPT in target.resolve().parents:
                    body = target.read_bytes()
                    kind = {".js": "text/javascript", ".html": "text/html; charset=utf-8",
                            ".json": "application/json", ".css": "text/css"}.get(target.suffix, "application/octet-stream")
                else:
                    self.send_error(404)
                    return
            self.send_response(200)
            self.send_header("Content-Type", kind)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, *args):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def test_files(selection):
    out = []
    for item in selection:
        path = WPT / item
        candidates = sorted(path.rglob("*")) if path.is_dir() else [path]
        for file in candidates:
            name = file.name
            if name.endswith((".any.js", ".window.js")) or (name.endswith(".html") and "-ref" not in name
                                                             and "testharness" in file.read_text(errors="replace")):
                out.append(file)
    return out


def url_of(file):
    relative = "/" + str(file.relative_to(WPT))
    for kind in ("any", "window"):
        if relative.endswith(f".{kind}.js"):
            return relative[:-3] + ".html"
    return relative


def run_one(app, port, file):
    limit = LONG_MS if is_long(file) else TIMEOUT_MS
    # The compat plan has no conditions: wait the test's whole time limit,
    # then read the harness's results (null: it had not completed).
    plan = (f"load\twpt\t{LOAD_DEADLINE_MS}\thttp://127.0.0.1:{port}{url_of(file)}\n"
            f"wait\t{limit}\njs\tresult\t(window.__wpt || 'null')\n")
    with tempfile.TemporaryDirectory(prefix="penny-wpt-") as directory:
        flags = pathlib.Path(directory)
        for name, value in {"compat": "", "log-level": "error", "compat-probe": "window.__probe = true;",
                            "compat-plan": plan}.items():
            (flags / name).write_text(value)
        try:
            completed = subprocess.run([app], env=dict(os.environ, PENNY_FLAGS=str(flags)), capture_output=True,
                                       text=True, timeout=(LOAD_DEADLINE_MS + limit) / 1000 + 120)
            log = completed.stdout + completed.stderr
        except subprocess.TimeoutExpired as expired:
            log = (expired.stdout or b"").decode(errors="replace") if isinstance(expired.stdout, bytes) else (expired.stdout or "")
    chunks = re.findall(r"PENNY-COMPAT js-ok wpt result \d+/\d+ (.*)", log)
    raw = "".join(chunks)
    if not raw or raw == "null":
        return {"status": "TIMEOUT" if chunks else "CRASH", "subtests": {}}
    result = json.loads(raw)
    return {"status": HARNESS.get(result["status"], str(result["status"])),
            "subtests": {t["name"]: STATUS.get(t["status"], str(t["status"])) for t in result["tests"]}}


def compare(before_path, after_path):
    before, after = (json.loads(pathlib.Path(p).read_text()) for p in (before_path, after_path))
    def passes(results):
        return {(f, n) for f, r in results.items() for n, s in r["subtests"].items() if s == "PASS"}
    def total(results):
        return sum(len(r["subtests"]) for r in results.values())
    b, a = passes(before), passes(after)
    print(f"before: {len(b)}/{total(before)} subtests pass; after: {len(a)}/{total(after)}")
    for file, name in sorted(b - a):
        print(f"REGRESSED {file}: {name} ({after.get(file, {}).get('subtests', {}).get(name, 'missing')})")
    for file, name in sorted(a - b):
        print(f"FIXED {file}: {name}")
    return 1 if b - a else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    parser.add_argument("--out", type=pathlib.Path)
    parser.add_argument("--compare", nargs=2, metavar=("BEFORE", "AFTER"))
    parser.add_argument("tests", nargs="*")
    options = parser.parse_args()
    if options.compare:
        return compare(*options.compare)
    server = serve()
    results = {}
    for file in test_files(options.tests):
        name = str(file.relative_to(WPT))
        results[name] = run_one(options.app, server.server_port, file)
        passed = sum(s == "PASS" for s in results[name]["subtests"].values())
        print(f"{name}: {results[name]['status']} {passed}/{len(results[name]['subtests'])}", flush=True)
        if options.out:
            options.out.write_text(json.dumps(results, indent=1))
    server.shutdown()
    total = sum(len(r["subtests"]) for r in results.values())
    print(f"WPT: {sum(s == 'PASS' for r in results.values() for s in r['subtests'].values())}/{total} subtests pass "
          f"in {len(results)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
