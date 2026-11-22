#!/usr/bin/env python3
"""Drive real guest keyboard input, then inspect a stopped disposable disk.

No guest test hooks substitute for the editor/compiler/IPC path. QMP transports
keystrokes and screenshots only. SQLite runs against exported copies, not a
filesystem image mounted while CuBit is using it.
"""
import hashlib
import json
import os
from pathlib import Path
import socket
import sqlite3
import subprocess
import sys
import time


class Guest:
    def __init__(self, results, name):
        self.results = results
        self.name = name
        self.serial = results / f"{name}.serial"
        self.qmp_path = results / f"{name}.qmp"
        self.process = None
        self.socket = None
        self.stream = None

    def __enter__(self):
        # Optional KVM on machines where the caller has access; TCG is portable.
        accel = os.environ.get("CONFIG_WORKBENCH_ACCEL", "tcg,thread=multi")
        self.process = subprocess.Popen([
            "qemu-system-x86_64", "-machine", "q35", "-accel", accel,
            "-cpu", "host" if accel == "kvm" else "Broadwell", "-smp", "4",
            "-m", "512M", "-cdrom", "kernel/cubit_kernel.iso",
            "-drive", f"file={self.results / 'disk.img'},if=none,id=nvme0,format=raw",
            "-device", "nvme,serial=cubit-config-demo,drive=nvme0",
            "-device", "virtio-vga", "-display", "none", "-no-reboot",
            "-serial", f"file:{self.serial}",
            "-qmp", f"unix:{self.qmp_path},server=on,wait=off"],
            stdout=subprocess.DEVNULL,
            stderr=(self.results / f"{self.name}.qemu.log").open("wb"))
        try:
            end = time.monotonic() + 15
            while not self.qmp_path.exists():
                if self.process.poll() is not None or time.monotonic() > end:
                    raise RuntimeError("QEMU failed to expose QMP")
                time.sleep(0.05)
            self.socket = socket.socket(socket.AF_UNIX)
            self.socket.settimeout(15)
            self.socket.connect(str(self.qmp_path))
            self.stream = self.socket.makefile("rwb", buffering=0)
            assert "QMP" in json.loads(self.stream.readline())
            self.command("qmp_capabilities")
            self.wait_marker("CONFIG-STORAGE: ready")
            if os.environ.get("CONFIG_WORKBENCH_APPS") == "1":
                self.wait_marker("desktop: internal shell active")
                # Workbench is the first Apps entry, including the Config-fed
                # menu. Exercise real desktop -> procmgr admission, not the
                # trusted startup path that directly launches the test app.
                self.key("meta_l")
                self.key("ret")
            self.wait_marker("ccl-workbench: first frame presented")
            return self
        except BaseException:
            self.__exit__(*sys.exc_info())
            raise

    def command(self, command, **arguments):
        self.stream.write((json.dumps({"execute": command, "arguments": arguments}) + "\n").encode())
        while True:
            result = json.loads(self.stream.readline())
            if "error" in result:
                raise RuntimeError(result)
            if "return" in result:
                return result["return"]

    def wait_marker(self, marker, count=1, timeout=120):
        end = time.monotonic() + timeout
        while time.monotonic() < end:
            if self.serial.exists() and self.serial.read_text(errors="replace").count(marker) >= count:
                return
            if self.process.poll() is not None:
                raise RuntimeError("Guest exited before " + marker)
            time.sleep(0.1)
        self.screenshot("timeout")
        raise TimeoutError(marker + "; inspect " + str(self.serial))

    def key(self, key):
        reply = self.command("human-monitor-command", **{"command-line": f"sendkey {key} 10"})
        if reply.strip():
            raise RuntimeError("QEMU rejected key: " + reply)
        time.sleep(0.035)

    def source(self, source):
        self.key("ctrl-a")
        mapping = {"(": "shift-9", ")": "shift-0", " ": "spc", "-": "minus",
                   ".": "dot", "=": "equal", "/": "slash"}
        for char in source:
            self.key("shift-" + char.lower() if char.isupper() else mapping.get(char, char))

    def run(self, count):
        self.key("ctrl-f5")
        self.wait_marker("ccl-workbench: bytecode completed", count)
        # Completion precedes asynchronous presentation. This delay is only
        # for the visual artifact, not the functional/durability assertion.
        time.sleep(2)
        self.screenshot(f"completed-{count}")

    def screenshot(self, label):
        self.command("screendump", filename=str(self.results / f"{self.name}-{label}.ppm"))

    def __exit__(self, *_):
        if self.process is not None and self.process.poll() is None:
            try:
                if self.stream is not None:
                    self.command("quit")
                self.process.wait(timeout=10)
            except (OSError, ValueError, subprocess.TimeoutExpired):
                self.process.terminate()
                self.process.wait(timeout=10)
        if self.stream is not None:
            self.stream.close()
        if self.socket is not None:
            self.socket.close()


def check_disk(results, export):
    disk = results / "disk.img"
    subprocess.run(["e2fsck", "-fn", str(disk)], check=True)
    export.mkdir()
    database = export / "system-config.sqlite"
    for suffix in ("", "-wal"):
        target = Path(str(database) + suffix)
        subprocess.run(["debugfs", "-R", f'dump /system-config.sqlite{suffix} "{target}"',
                        str(disk)], check=True, capture_output=True)
        assert target.is_file() and target.stat().st_size > 0, target
    key = hashlib.sha256(Path("userspace/ccl/interfaces/workbench-config-value.schema").read_bytes()).digest()
    payload = b"\x84\x01\x58\x20" + key + b"\x81\x82\x18\x2a\x00\x40"
    with sqlite3.connect(database) as db:
        assert db.execute("PRAGMA integrity_check").fetchall() == [("ok",)]
        assert db.execute("SELECT version FROM config_format").fetchall() == [(4,)]
        name = "com.cubit.ccl-workbench.demo.counter"
        assert db.execute("SELECT namespace,profile,revision FROM collections").fetchall() == [(name, "machine", 2)]
        rows = db.execute("SELECT revision,payload_schema,payload FROM revisions ORDER BY revision").fetchall()
        assert rows == [(1, key, payload), (2, key, payload)], rows
    print("CONFIG-WORKBENCH: independent SQLite/WAL/ext2 check PASS", flush=True)


def read_assertion():
    read = " ".join(line.strip() for line in
        Path("userspace/ccl/samples/config-counter-read.ccl").read_text().splitlines()
        if not line.lstrip().startswith("#"))
    # Verify the returned value in the guest without a privileged test hook or
    # logging Config contents. A wrong result cannot emit the Completed marker.
    return f"(let ((observed {read})) (if (= observed 42) observed (/ 1 0)))"


def main():
    results = Path(sys.argv[1]).resolve(strict=True)
    write = "(let ((c (config-values.open))) (let ((r (config-values.read c))) (let ((w (config-values.write c 42))) (let ((closed (config-values.close c))) w))))"
    with Guest(results, "write") as guest:
        guest.source(write)
        guest.run(1)
        guest.run(2)
    check_disk(results, results / "after-write")
    with Guest(results, "reopen") as guest:
        guest.source(read_assertion())
        guest.run(1)
    check_disk(results, results / "after-reopen")
    print("CONFIG-WORKBENCH: native editor/compiler/IPC repeated-run and reboot PASS", flush=True)


if __name__ == "__main__":
    main()
