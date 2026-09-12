#!/usr/bin/env python3
"""Linux-only, opt-in vCPU affinity. Never alters host-wide scheduling policy."""
import argparse
import json
import os
from pathlib import Path
import re
import signal
import socket
import subprocess
import sys
import tempfile
import time


def cpu_list(value, count, allowed):
    if not re.fullmatch(r"[0-9]+(?:,[0-9]+)*", value):
        raise ValueError("CPU mapping must be comma-separated CPU numbers")
    cpus = [int(part) for part in value.split(",")]
    if len(cpus) != count or len(set(cpus)) != count:
        raise ValueError("provide exactly one distinct host CPU per vCPU")
    if not set(cpus) <= allowed:
        raise ValueError(f"requested CPUs outside allowed affinity: {sorted(allowed)}")
    return cpus


def pin_vcpus(entries, cpus, pid):
    if (len(entries) != len(cpus) or
            sorted(item["cpu-index"] for item in entries) != list(range(len(cpus))) or
            len({item["thread-id"] for item in entries}) != len(cpus)):
        raise ValueError("QMP did not report exactly the expected distinct vCPU threads")
    for item in sorted(entries, key=lambda item: item["cpu-index"]):
        index, tid = item["cpu-index"], item["thread-id"]
        if not Path(f"/proc/{pid}/task/{tid}").is_dir():
            raise ValueError("QMP thread does not belong to the launched QEMU")
        os.sched_setaffinity(tid, {cpus[index]})
        if os.sched_getaffinity(tid) != {cpus[index]}:
            raise ValueError("vCPU affinity verification failed")
        topology = Path(f"/sys/devices/system/cpu/cpu{cpus[index]}/topology")
        siblings = (topology / "thread_siblings_list").read_text().strip()
        print(f"AFFINITY: vcpu={index} tid={tid} host_cpu={cpus[index]} "
              f"smt_siblings={siblings} verified=1", flush=True)


class QMP:
    def __init__(self, stream):
        self.stream = stream
        self.sequence = 0
        if "QMP" not in self.read():
            raise ValueError("missing QMP greeting")

    def read(self):
        line = self.stream.readline()
        if not line:
            raise ValueError("QMP disconnected")
        return json.loads(line)

    def execute(self, command):
        self.sequence += 1
        self.stream.write(json.dumps({"execute": command, "id": self.sequence}) + "\n")
        self.stream.flush()
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            reply = self.read()
            if "event" in reply:
                continue
            if reply.get("id") != self.sequence or "error" in reply:
                raise ValueError(f"unexpected QMP reply: {reply}")
            return reply["return"]
        raise TimeoutError("QMP command deadline exceeded")


def run(cpus, command):
    # A separate QMP socket allows existing input-injection clients to coexist.
    with tempfile.TemporaryDirectory(prefix="cubit-affinity-", dir="/tmp") as directory:
        path = str(Path(directory) / "qmp.sock")
        child = subprocess.Popen(command + ["-S", "-qmp", f"unix:{path},server=on,wait=off"])
        try:
            deadline = time.monotonic() + 10
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as connection:
                connection.settimeout(5)
                while True:
                    if child.poll() is not None:
                        raise ValueError(f"QEMU exited before pinning: {child.returncode}")
                    try:
                        connection.connect(path)
                        break
                    except (FileNotFoundError, ConnectionRefusedError):
                        if time.monotonic() >= deadline:
                            raise TimeoutError("QEMU affinity socket did not become ready")
                        time.sleep(0.02)
                with connection.makefile("rw", encoding="utf-8") as stream:
                    qmp = QMP(stream)
                    qmp.execute("qmp_capabilities")
                    pin_vcpus(qmp.execute("query-cpus-fast"), cpus, child.pid)
                    qmp.execute("cont")
                    print("AFFINITY: guest resumed; other QEMU threads retain inherited affinity", flush=True)
            return child.wait()
        finally:
            # timeout sends TERM to the process group. Also reap/stop the child
            # on setup failure or when only this wrapper receives a signal.
            if child.poll() is None:
                child.terminate()
                try:
                    child.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    child.kill()
                    child.wait()


def interrupted(signum, frame):
    raise InterruptedError(f"received signal {signum}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--vcpu-cpus", required=True)
    parser.add_argument("--count", required=True, type=int, choices=range(1, 5))
    parser.add_argument("--validate-only", action="store_true")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    try:
        cpus = cpu_list(args.vcpu_cpus, args.count, os.sched_getaffinity(0))
        if args.validate_only:
            return 0
        command = args.command
        if command and command[0] == "--":
            command = command[1:]
        if not command:
            raise ValueError("missing QEMU command")
        signal.signal(signal.SIGTERM, interrupted)
        signal.signal(signal.SIGINT, interrupted)
        return run(cpus, command)
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"affinity: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
