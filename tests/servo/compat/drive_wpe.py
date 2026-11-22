#!/usr/bin/env python3
"""WEB-001 compatibility survey: stock WPE WebKit, in its sandbox.

    drive_wpe.py --run N --out DIR --viewport WxH [--sites a,b] [--groups g]
        [--sites-file F]

WPE WebKit from tools.nix (wpe.nix: the WPE port of the release nixpkgs
packages as WebKitGTK, not Playwright's patched WebKit), driven over
WebDriver (WPEWebDriver, plain HTTP; the standard library is the client).
Each site gets a fresh session: a new headless MiniBrowser whose website
data is ephemeral (cold cache, no cookies), with the survey probe as a
document-start user script (MiniBrowser --user-script, wpe.nix's patch, as
Playwright's addInitScript), then the steps in plan.py. Records go to
DIR/records.jsonl in drive_reference.py's shape.

WebKit's web and network processes run in its bubblewrap sandbox. The
driver refuses to start when a sandbox-disabling variable is set, and checks
every session: the web process must run under the bwrap binary WebKit was
built with (WPE_BWRAP, the exact store path the host's AppArmor profile
names) in a user namespace of its own; otherwise it records
sandbox-missing and stops the run. Environment (run-survey.sh sets it):
WPE_ROOT (the wpe output), WPE_BWRAP, and the EGL variables for a host that
is not NixOS.
"""
import argparse
import base64
import json
import os
import pathlib
import socket as sockets
import subprocess
import sys
import time
import urllib.error
import urllib.request

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import plan  # noqa: E402

ENGINE = "wpe"
MINIBROWSER = "libexec/wpe-webkit-2.0/MiniBrowser"
WEBDRIVER = "bin/WPEWebDriver"
# Any of these turns WebKit's sandbox off; the survey never runs so.
SANDBOX_OFF_VARIABLES = ("WEBKIT_DISABLE_SANDBOX_THIS_IS_DANGEROUS", "WEBKIT_DISABLE_SANDBOX")
# A step that has not finished by then is abandoned (a hung page script).
STEP_DEADLINE_SECONDS = 15
# Beyond a command's own deadline, for the HTTP round trip.
HTTP_MARGIN_SECONDS = 30
DRIVER_START_SECONDS = 20
# The web process appears this long after the session at most.
SANDBOX_CHECK_SECONDS = 10
POLL_SECONDS = 0.1
WHEEL_PAUSE_MS = 50
CLICK_PAUSE_MS = 50
KEY_PAUSE_MS = 30


class SessionGone(Exception):
    """The browser behind the session went away (a crash)."""


class WebDriver:
    def __init__(self, port):
        self.base = f"http://127.0.0.1:{port}"

    def call(self, method, path, body=None, seconds=STEP_DEADLINE_SECONDS):
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(self.base + path, data=data, method=method,
                                         headers={"Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(request, timeout=seconds + HTTP_MARGIN_SECONDS) as response:
                return json.loads(response.read() or b"{}").get("value")
        except urllib.error.HTTPError as failure:
            try:
                value = json.loads(failure.read()).get("value", {})
            except json.JSONDecodeError:
                value = {}
            error = value.get("error", f"HTTP {failure.code}")
            if error in ("invalid session id", "no such window"):
                raise SessionGone(error) from None
            raise RuntimeError(f"{error}: {value.get('message', '')}"[:300]) from None


def free_port():
    with sockets.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        return probe.getsockname()[1]


def descendants(root):
    """pid -> (parent, exe) of every process under root (root included)."""
    table = {}
    for entry in pathlib.Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        try:
            fields = (entry / "stat").read_text().rsplit(")", 1)[1].split()
            table[int(entry.name)] = int(fields[1])
        except (OSError, IndexError, ValueError):
            continue
    found, frontier = {root}, [root]
    while frontier:
        parent = frontier.pop()
        for pid, ppid in table.items():
            if ppid == parent and pid not in found:
                found.add(pid)
                frontier.append(pid)
    out = {}
    for pid in found:
        try:
            out[pid] = (table.get(pid), os.readlink(f"/proc/{pid}/exe"))
        except OSError:
            continue
    return out


def sandbox_state(driver_pid, bwrap):
    """How the session's web processes run: 'sandboxed: ...' when every
    WPEWebProcess descends from the expected bwrap and sits in another
    user namespace than this driver; otherwise why not (None while no web
    process exists yet)."""
    processes = descendants(driver_pid)
    own_namespace = os.readlink("/proc/self/ns/user")
    web = [pid for pid, (_, exe) in processes.items() if exe.endswith("/WPEWebProcess")]
    if not web:
        return None
    for pid in web:
        chain, cursor = [], processes[pid][0]
        while cursor in processes:
            chain.append(processes[cursor][1])
            cursor = processes[cursor][0]
        if bwrap not in chain:
            return f"web process {pid} not under {bwrap} (ancestors: {chain[:4]})"
        try:
            if os.readlink(f"/proc/{pid}/ns/user") == own_namespace:
                return f"web process {pid} shares the driver's user namespace"
        except OSError as error:
            return f"web process {pid}: {error}"
    others = sorted({pathlib.Path(exe).name for _, exe in processes.values()
                     if exe.endswith(("/WPENetworkProcess", "/WPEGPUProcess", "/xdg-dbus-proxy"))})
    return f"sandboxed: {len(web)} web process(es) under {bwrap}" + (f", with {', '.join(others)}" if others else "")


class Recorder:
    def __init__(self, path, run):
        self.stream = path.open("a")
        self.run = run

    def __call__(self, kind, site, name, text):
        self.stream.write(json.dumps({"engine": ENGINE, "run": self.run, "kind": kind,
                                      "site": site, "name": name, "text": text}) + "\n")
        self.stream.flush()


def capabilities(root, probe, viewport):
    return {"capabilities": {"alwaysMatch": {
        "pageLoadStrategy": "normal",
        "timeouts": {"pageLoad": plan.LOAD_DEADLINE_MS, "script": STEP_DEADLINE_SECONDS * 1000,
                     "implicit": 0},
        "wpe:browserOptions": {
            "binary": str(root / MINIBROWSER),
            # --automation: MiniBrowser accepts WebDriver's session only so.
            "args": ["--automation", "--headless", f"--user-script={probe}",
                     f"--size={viewport['width']}x{viewport['height']}"]}}}}


def survey_site(driver, session, site, viewport, shots, record):
    """The site's steps; returns the console.error calls the probe saw
    (WebDriver exposes no console log; the other references count the
    console's error messages)."""
    sid = site["id"]
    console_errors = 0
    prefix = f"/session/{session}"
    for step in plan.steps(site):
        kind = step["do"]
        try:
            if kind == "load":
                started = time.monotonic()
                loaded, error = True, None
                try:
                    driver.call("POST", prefix + "/url", {"url": step["url"]},
                                seconds=step["deadline_ms"] / 1000)
                except RuntimeError as failure:
                    loaded, error = False, str(failure)
                elapsed = round((time.monotonic() - started) * 1000)
                record("load", sid, "-", json.dumps({
                    "loaded": loaded, "load_ms": elapsed if loaded else None, "first_frame_ms": None,
                    "crashed": False, "error": error}))
            elif kind == "wait":
                time.sleep(step["ms"] / 1000)
            elif kind == "js":
                try:
                    value = driver.call("POST", prefix + "/execute/sync",
                                        {"script": f"return ({step['script']});", "args": []})
                    text = value if isinstance(value, str) else json.dumps(value)
                    record("js-ok", sid, step["name"], text)
                    if step["name"] == "collect":
                        try:
                            console_errors = int(json.loads(text).get("consoleErrors") or 0)
                        except (ValueError, AttributeError):
                            pass
                except RuntimeError as failure:
                    record("js-error", sid, step["name"], str(failure).splitlines()[0][:300])
            elif kind == "click":
                click(driver, prefix, step, sid, record)
            elif kind == "wheel":
                x, y = viewport["width"] // 2, viewport["height"] // 2
                scrolls = []
                for _ in range(step["notches"]):
                    scrolls += [{"type": "scroll", "x": x, "y": y, "deltaX": 0,
                                 "deltaY": plan.WHEEL_NOTCH_PIXELS, "duration": 0, "origin": "viewport"},
                                {"type": "pause", "duration": WHEEL_PAUSE_MS}]
                perform(driver, prefix, [{"type": "wheel", "id": "wheel", "actions": scrolls}])
            elif kind == "keys":
                keys = []
                for character in step["text"]:
                    keys += [{"type": "keyDown", "value": character}, {"type": "keyUp", "value": character},
                             {"type": "pause", "duration": KEY_PAUSE_MS}]
                perform(driver, prefix, [{"type": "key", "id": "keyboard", "actions": keys}])
            elif kind == "shot":
                png = base64.b64decode(driver.call("GET", prefix + "/screenshot"))
                (shots / f"{sid}-{step['name']}.png").write_bytes(png)
                record("shot", sid, step["name"], f"{viewport['width']}x{viewport['height']}")
        except SessionGone:
            raise
        except Exception as error:  # the step's own failure, recorded
            record(f"{kind}-failed", sid, step.get("name", "-"), str(error).splitlines()[0][:300])
    return console_errors


def perform(driver, prefix, sources):
    try:
        driver.call("POST", prefix + "/actions", {"actions": sources})
    finally:
        driver.call("DELETE", prefix + "/actions")


def click(driver, prefix, step, sid, record):
    try:
        answer = driver.call("POST", prefix + "/execute/sync", {"script": f"return ({step['script']});", "args": []})
    except RuntimeError as error:
        record("click-missed", sid, step["name"], str(error).splitlines()[0][:200])
        return
    try:
        x, y = (float(v) for v in str(answer).split(","))
    except ValueError:
        record("click-missed", sid, step["name"], str(answer)[:200])
        return
    perform(driver, prefix, [{"type": "pointer", "id": "mouse", "parameters": {"pointerType": "mouse"},
                              "actions": [{"type": "pointerMove", "x": round(x), "y": round(y),
                                           "origin": "viewport", "duration": 0},
                                          {"type": "pause", "duration": CLICK_PAUSE_MS},
                                          {"type": "pointerDown", "button": 0},
                                          {"type": "pointerUp", "button": 0}]}])
    record("click", sid, step["name"], f"{x:g},{y:g}")


class Service:
    """WPEWebDriver on a free local port, refusing a sandbox-off environment."""

    def __init__(self, log):
        for name in SANDBOX_OFF_VARIABLES:
            if name in os.environ:
                sys.exit(f"wpe: {name} is set; the survey runs WebKit sandboxed only")
        self.root = pathlib.Path(os.environ["WPE_ROOT"])
        self.bwrap = os.environ["WPE_BWRAP"]
        port = free_port()
        # Its own process group: stop() ends the browsers it started too.
        self.process = subprocess.Popen([str(self.root / WEBDRIVER), f"--port={port}"],
                                        stdout=log.open("a"), stderr=subprocess.STDOUT,
                                        start_new_session=True)
        self.driver = WebDriver(port)
        deadline = time.monotonic() + DRIVER_START_SECONDS
        while True:
            try:
                self.driver.call("GET", "/status")
                return
            except (OSError, RuntimeError):
                if time.monotonic() > deadline or self.process.poll() is not None:
                    self.stop()
                    sys.exit("wpe: WPEWebDriver did not start")
                time.sleep(POLL_SECONDS)

    def session(self, probe, viewport):
        """A new session: (its id, its capabilities, its sandbox state)."""
        created = self.driver.call("POST", "/session", capabilities(self.root, probe, viewport),
                                   seconds=DRIVER_START_SECONDS)
        # The session starts on about:blank: its web process exists.
        deadline = time.monotonic() + SANDBOX_CHECK_SECONDS
        state = sandbox_state(self.process.pid, self.bwrap)
        while state is None and time.monotonic() < deadline:
            time.sleep(POLL_SECONDS)
            state = sandbox_state(self.process.pid, self.bwrap)
        return created["sessionId"], created.get("capabilities", {}), state or "no web process"

    def close(self, session):
        try:
            self.driver.call("DELETE", f"/session/{session}")
        except Exception:
            pass

    def stop(self):
        import signal
        try:
            os.killpg(self.process.pid, signal.SIGTERM)
            self.process.wait(timeout=10)
        except (ProcessLookupError, subprocess.TimeoutExpired):
            pass
        try:
            os.killpg(self.process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        self.process.wait()


CHECK_TITLE = "cubit-wpe-sandbox-check"


def check(directory):
    """wpe-sandbox.sh verify: load a page from a local server in a fresh
    session; succeed only when it loaded and the session is sandboxed."""
    import http.server
    import threading

    class Page(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            body = f"<!doctype html><title>{CHECK_TITLE}</title><p>sandboxed?".encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, *arguments):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Page)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    probe = directory / "probe.js"
    probe.write_text(plan.PROBE)
    service = Service(directory / "webdriver.log")
    session = None
    try:
        session, caps, state = service.session(probe, {"width": 800, "height": 600})
        prefix = f"/session/{session}"
        service.driver.call("POST", prefix + "/url", {"url": f"http://127.0.0.1:{server.server_port}/"},
                            seconds=plan.LOAD_DEADLINE_MS / 1000)
        seen = service.driver.call("POST", prefix + "/execute/sync", {
            "script": "return document.title + ' probe=' + !!window.__cubitCompat;", "args": []})
        png = base64.b64decode(service.driver.call("GET", prefix + "/screenshot"))
        (directory / "check.png").write_bytes(png)
        if len(png) < 100:
            print(f"WPE screenshot failed ({len(png)} bytes)", flush=True)
            sys.exit(1)
        # The state again, now that the page (and the network process) ran.
        state = sandbox_state(service.process.pid, service.bwrap) or state
        print(f"WPE {caps.get('browserVersion', '?')} loaded '{seen}', screenshot {len(png)} bytes; {state}", flush=True)
        if seen != f"{CHECK_TITLE} probe=true" or not state.startswith("sandboxed"):
            sys.exit(1)
    finally:
        if session:
            service.close(session)
        service.stop()
        server.shutdown()


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--engine", default=ENGINE, choices=[ENGINE])
    parser.add_argument("--check", type=pathlib.Path, metavar="DIR",
                        help="load one local page sandboxed and stop (wpe-sandbox.sh verify)")
    parser.add_argument("--run", type=int)
    parser.add_argument("--out", type=pathlib.Path)
    parser.add_argument("--viewport")
    parser.add_argument("--sites")
    parser.add_argument("--groups")
    parser.add_argument("--sites-file", default=str(plan.HERE / "sites.json"))
    options = parser.parse_args()
    if options.check:
        check(options.check)
        return
    if options.run is None or options.out is None or options.viewport is None:
        parser.error("--run, --out and --viewport are required")
    width, height = (int(v) for v in options.viewport.split("x"))
    viewport = {"width": width, "height": height}
    _, sites = plan.load_sites(options.sites_file,
                               only=set(options.sites.split(",")) if options.sites else None,
                               groups=set(options.groups.split(",")) if options.groups else None)
    shots = options.out / "shots"
    shots.mkdir(parents=True, exist_ok=True)
    probe = options.out / "probe.js"
    probe.write_text(plan.PROBE)
    record = Recorder(options.out / "records.jsonl", options.run)
    service = Service(options.out / "webdriver.log")
    try:
        versioned = False
        for site in sites:
            started = time.monotonic()
            session = None
            try:
                session, caps, state = service.session(probe, viewport)
                if not versioned:
                    record("version", "-", "-", f"wpe {caps.get('browserVersion', '?')}")
                    versioned = True
                if not state.startswith("sandboxed"):
                    record("sandbox-missing", site["id"], "-", state)
                    sys.exit(f"wpe: not sandboxed: {state}")
                record("sandbox", site["id"], "-", state)
                console_errors = survey_site(service.driver, session, site, viewport, shots, record)
                record("console", site["id"], "count", str(console_errors))
            except SessionGone as gone:
                record("crash", site["id"], "-", f"browser gone: {gone}")
                record("console", site["id"], "count", "0")
            except (RuntimeError, OSError, KeyError) as error:
                record("site-failed", site["id"], "-", str(error).splitlines()[0][:300] if str(error) else type(error).__name__)
                record("console", site["id"], "count", "0")
            finally:
                if session:
                    service.close(session)
            print(f"wpe run {options.run} {site['id']}: {time.monotonic() - started:.1f}s", flush=True)
    finally:
        service.stop()
    record("done", "-", "-", "-")


if __name__ == "__main__":
    main()
