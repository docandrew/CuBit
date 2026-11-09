#!/usr/bin/env python3
"""PERF-010: the on-device build latency benchmark's host side.

Builds fixed apps ON CuBit through ccl-remote, cold (the Storehouse has
never seen them), warm (every step answered by the Storehouse) and after a
one-line edit, and records where the time went: ccl-build's own "timing"
lines (Build_Timing: per step spawn, tool run, input placement, hashing,
Storehouse writes, claims, ...), the host's wall time per build, and, under
QEMU, the guest's CPU time (its vCPU threads' run time, /proc schedstat),
so waiting is told apart from computing.

Usage (the headless test, tests/headless/run.sh --test bench-build):
  bench_build.py --serial LOG --qmp SOCKET --timeout S --port P BUILD_DIR
                 [--out FILE] [--linux FILE] [--apps A,B] [--warm N] [--edits N]
On real hardware, paired with tools/cubit-remote (cubit-remote pair):
  bench_build.py --host NAME BUILD_DIR [--out FILE] [--linux FILE] ...

BUILD_DIR is tests/device-builds/build (build.sh): app-<name>/ holds each
app's sources as the host puts them. The device's workspace already holds
the source subset an app's build reads (the device-build payload).

Writes FILE (default BUILD_DIR/bench-cubit.json), schema perf-010.bench/1:
  {"schema", "platform": "cubit", "where": "qemu-kvm" | "hardware",
   "contention": {...}, "runs": [Run]}
  Run: {"app", "kind": "cold" | "warm" | "edit", "index", "ok",
        "ran", "hit", "host_wall_s", "guest_cpu_s" (QEMU only: vCPU run
        time during the build), "vcpus", "qemu_cpu_s",
        "build": ccl-build's "build" timing record,
        "steps": [its "step" timing records],
        "spawns" (QEMU only: procmgr's serial lines): [{"name",
        "image_bytes", "read_ms" (procmgr reading the whole image),
        "spawn_ms" (the kernel making the process)}], every program
        started during the build, the gcc driver's own children too}
then prints the breakdown (bench_report.py) and exits 1 on any failure.
"""
import argparse
import json
import os
import re
import socket
import struct
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parents[1] / "tools" / "cubit-remote"))
sys.path.insert(0, str(HERE))
import cubit_remote as cr  # noqa: E402
import bench_report  # noqa: E402
from bench_build_common import (  # noqa: E402
    APP_DIRECTORY, MAIN_UNIT, DEFAULT_APPS, app_sources, DEFAULT_EDITS, DEFAULT_WARM, EDIT_FORMAT,
    EDIT_LINE, EDITED_APP, SCHEMA, parse_run)

CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
# procmgr's serial lines for each program it starts (userspace/services/
# procmgr Spawn_Attempt): the name, reading the whole image from the
# filesystem (ms), and the kernel's SYSCALL_SPAWN (ms).
SPAWN_NAME = re.compile(rb"procmgr: spawn: (\S+)")
SPAWN_READ = re.compile(rb"procmgr: read (\d+) bytes in (\d+)ms")
SPAWN_KERNEL = re.compile(rb"procmgr: SYSCALL_SPAWN took (\d+)ms")
LISTENING = b"ccl-remote: listening on "
STATE_RUNNING, STATE_EXITED = 1, 2
# Polls while a job runs: short, so the host's wall time is close to the
# job's (a poll is one ccl-remote request).
POLL_INTERVAL_S = 0.02
NANOSECONDS = 1e9

parser = argparse.ArgumentParser(description="PERF-010 on-device build benchmark")
parser.add_argument("build_dir", type=Path)
parser.add_argument("--serial")
parser.add_argument("--qmp")
parser.add_argument("--timeout", type=float, default=1800)
parser.add_argument("--port", type=int)
parser.add_argument("--host", help="a host paired with cubit-remote (real hardware)")
parser.add_argument("--out", type=Path)
parser.add_argument("--linux", type=Path, help="bench_linux.py's results, for the comparison")
parser.add_argument("--apps", default=DEFAULT_APPS)
parser.add_argument("--warm", type=int, default=DEFAULT_WARM)
parser.add_argument("--edits", type=int, default=DEFAULT_EDITS)
options = parser.parse_args()
deadline = time.monotonic() + options.timeout
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


# --- QEMU: pairing, vCPU threads, quitting -------------------------------------
def serial_bytes():
    try:
        return Path(options.serial).read_bytes()
    except OSError:
        return b""


def pair_with_guest():
    code = None
    while time.monotonic() < deadline:
        data = serial_bytes()
        found = CODE.search(data)
        if found and LISTENING in data:
            code = found.group(1).decode()
            break
        if b"CUBIT KERNEL PANIC" in data:
            break
        time.sleep(0.5)
    check(code is not None, "the guest showed a pairing code and listens")
    if code is None:
        return None
    host = cr.Host("guest", "127.0.0.1", options.port, cr.decode_token_text(code))
    for _ in range(20):
        try:
            bits, _, server = cr.hello(host)
            check(bits & 4 != 0, "hello: %s may build (%s)" % (server, cr.grant_names(bits)))
            return host
        except (OSError, cr.RemoteError):
            time.sleep(0.5)
    check(False, "hello with the pairing token")
    return None


def qmp(commands):
    """Runs QMP commands in one connection; their return values."""
    results = []
    with socket.socket(socket.AF_UNIX) as connection:
        connection.settimeout(5)
        connection.connect(options.qmp)
        stream = connection.makefile("rwb")
        json.loads(stream.readline())
        for command in ["qmp_capabilities"] + commands:
            stream.write(json.dumps({"execute": command}).encode() + b"\n")
            stream.flush()
            while True:
                response = json.loads(stream.readline())
                if "return" in response or "error" in response:
                    results.append(response.get("return"))
                    break
    return results[1:]


def vcpu_threads():
    """The host threads running the guest's vCPUs (QMP query-cpus-fast)."""
    try:
        cpus = qmp(["query-cpus-fast"])[0] or []
        return [cpu["thread-id"] for cpu in cpus]
    except (OSError, ValueError, KeyError) as error:
        print("bench-build: no vCPU threads (%s)" % error, flush=True)
        return []


def thread_run_ns(tid):
    """A host thread's time on a CPU, in ns (/proc/TID/schedstat)."""
    try:
        return int(Path("/proc/%d/schedstat" % tid).read_text().split()[0])
    except (OSError, ValueError, IndexError):
        return 0


def process_of(tid):
    try:
        for line in Path("/proc/%d/status" % tid).read_text().splitlines():
            if line.startswith("Tgid:"):
                return int(line.split()[1])
    except (OSError, ValueError):
        pass
    return None


def process_cpu_s(pid):
    """A host process's user + system time (seconds), all its threads."""
    try:
        fields = Path("/proc/%d/stat" % pid).read_text().rsplit(")", 1)[1].split()
        return (int(fields[11]) + int(fields[12])) / os.sysconf("SC_CLK_TCK")
    except (OSError, ValueError, IndexError):
        return 0.0


def quit_qemu():
    try:
        qmp(["quit"])
    except (OSError, ValueError) as error:
        print("bench-build: could not quit QEMU: %s" % error, file=sys.stderr)


def contention(own_qemu):
    """What else is running on this host: other QEMUs (by /proc/*/comm,
    never a pattern match on command lines) and the load average."""
    others = []
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit() or int(entry.name) == own_qemu:
            continue
        try:
            if (entry / "comm").read_text().strip().startswith("qemu-system"):
                others.append(int(entry.name))
        except OSError:
            pass
    try:
        load = [float(x) for x in Path("/proc/loadavg").read_text().split()[:3]]
    except (OSError, ValueError):
        load = []
    return {"other_qemu_processes": sorted(others), "load_average": load, "cpus": os.cpu_count()}


# --- Builds ----------------------------------------------------------------------
def put_app(host, app, main_text=None):
    for source in app_sources(options.build_dir, app):
        remote = "%s/%s" % (APP_DIRECTORY % app, source.name)
        if source.name == MAIN_UNIT and main_text is not None:
            local = options.build_dir / ("bench-edit-%s.adb" % app)
            local.write_text(main_text)
            cr.put_file(host, "workspace", str(local), remote)
            local.unlink()
        else:
            cr.put_file(host, "workspace", str(source), remote)


class Sampler:
    """The guest's CPU time (its vCPU threads) and QEMU's, around a build."""

    def __init__(self, threads):
        self.threads = threads
        self.qemu = process_of(threads[0]) if threads else None

    def sample(self):
        return (sum(thread_run_ns(t) for t in self.threads),
                process_cpu_s(self.qemu) if self.qemu else 0.0)


def spawns_in(data):
    """procmgr's spawn records in serial output, in order."""
    found = []
    for line in data.splitlines():
        name = SPAWN_NAME.search(line)
        read = SPAWN_READ.search(line)
        kernel = SPAWN_KERNEL.search(line)
        if name:
            found.append({"name": name.group(1).decode("utf-8", "replace")})
        elif read and found and "image_bytes" not in found[-1]:
            found[-1]["image_bytes"] = int(read.group(1))
            found[-1]["read_ms"] = int(read.group(2))
        elif kernel and found and "spawn_ms" not in found[-1]:
            found[-1]["spawn_ms"] = int(kernel.group(1))
    return found


def build(host, sampler, app, kind, index):
    component = "%s/component.ccl" % (APP_DIRECTORY % app)
    serial_from = len(serial_bytes()) if options.serial else 0
    before = sampler.sample()
    started = time.monotonic()
    job = struct.unpack(">I", host.call("build_start", "workspace", cr.name_field(component)))[0]
    offset, output, state, code = 0, b"", STATE_RUNNING, 0
    while time.monotonic() < deadline:
        state, code, total, first_kept, out = cr.build_poll(host, job, offset)
        if first_kept > offset:
            offset = first_kept
        output += out
        offset += len(out)
        if state != STATE_RUNNING and offset >= total:
            break
        if not out:
            time.sleep(POLL_INTERVAL_S)
    wall = time.monotonic() - started
    after = sampler.sample()
    text = output.decode("utf-8", "replace")
    run = parse_run(text, app, kind, index, state == STATE_EXITED and code == 0, wall)
    record = run["build"]
    if options.serial:
        run["spawns"] = spawns_in(serial_bytes()[serial_from:])
    if sampler.threads:
        run["guest_cpu_s"] = round((after[0] - before[0]) / NANOSECONDS, 4)
        run["vcpus"] = len(sampler.threads)
        run["qemu_cpu_s"] = round(after[1] - before[1], 3)
    print("bench-build: %s %s %d: %s, %d ran, %d hit, host %.3f s, ccl-build %.3f s%s" % (
        app, kind, index, "ok" if run["ok"] else "FAILED (job state %d, exit %d)" % (state, code),
        run["ran"], run["hit"], wall, (record or {}).get("wall_us", 0) / 1e6,
        ", guest CPU %.3f s" % run["guest_cpu_s"] if "guest_cpu_s" in run else ""), flush=True)
    if not run["ok"]:
        print(text, flush=True)
    return run


def bench_app(host, sampler, app, runs):
    main_source = (options.build_dir / ("app-%s" % app) / MAIN_UNIT).read_text()
    put_app(host, app)
    runs.append(build(host, sampler, app, "cold", 0))
    check(runs[-1]["ok"] and runs[-1]["ran"] > 0, "%s cold: builds, every step runs" % app)
    for n in range(options.warm):
        runs.append(build(host, sampler, app, "warm", n))
        check(runs[-1]["ok"] and runs[-1]["ran"] == 0, "%s warm %d: no step runs" % (app, n))
    if app == EDITED_APP and EDIT_LINE in main_source:
        for n in range(options.edits):
            put_app(host, app, main_source.replace(EDIT_LINE, EDIT_FORMAT % (n + 1)))
            runs.append(build(host, sampler, app, "edit", n))
            check(runs[-1]["ok"] and 0 < runs[-1]["ran"] < runs[-1]["ran"] + runs[-1]["hit"],
                  "%s edit %d: only the steps the change reaches run" % (app, n))
        put_app(host, app)


def main():
    if options.host:
        store = cr.load_hosts()
        entry = store["hosts"][options.host]
        host = cr.Host(options.host, entry["address"], entry["port"], bytes.fromhex(entry["token"]))
        where, threads = "hardware", []
    else:
        host = pair_with_guest()
        where, threads = "qemu-kvm", vcpu_threads()
    if host is None:
        return
    sampler = Sampler(threads)
    report = {"schema": SCHEMA, "platform": "cubit", "where": where,
              "contention": {"before": contention(sampler.qemu)}, "runs": []}
    for app in [a for a in options.apps.split(",") if a]:
        bench_app(host, sampler, app, report["runs"])
    report["contention"]["after"] = contention(sampler.qemu)
    out = options.out or options.build_dir / "bench-cubit.json"
    out.write_text(json.dumps(report, indent=1) + "\n")
    print("bench-build: results in %s" % out, flush=True)
    linux = json.loads(options.linux.read_text()) if options.linux and options.linux.exists() else None
    bench_report.show(report, linux)


try:
    main()
finally:
    if not options.host and options.qmp and os.environ.get("BENCH_BUILD_KEEP_RUNNING") != "1":
        quit_qemu()
print("BENCH-BUILD: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
