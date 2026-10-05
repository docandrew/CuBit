#!/usr/bin/env python3
"""Bounded host-side serial capture; independent of guest crash handlers."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

SEGMENT = 8 * 1024 * 1024
CONTEXT = 512 * 1024
AFTER = 256 * 1024
MARKERS = (b"PENNY-ABORT:", b"CUBITSHELL: panic", b"USER-MEMORY-FAULT:", b"KERNEL PANIC")

class Capture:
    def __init__(self, directory, limit=SEGMENT):
        self.directory = Path(directory)
        self.limit = limit
        self.serial = (self.directory / "serial.log").open("wb")
        self.size = 0
        self.context = bytearray()
        self.suffix = b""
        self.crash = None
        self.remaining = AFTER

    def write(self, data):
        if self.crash is None:
            if any(marker in self.suffix + data for marker in MARKERS):
                self.crash = (self.directory / "crash.log").open("wb")
                self.crash.write(self.context)
            else:
                self.context.extend(data)
                del self.context[:-CONTEXT]
            self.suffix = (self.suffix + data)[-64:]
        if self.crash is not None and self.remaining:
            part = data[:self.remaining]
            self.crash.write(part)
            self.crash.flush()
            self.remaining -= len(part)
        while data:
            if self.size == self.limit:
                self.serial.close()
                (self.directory / "serial.log").replace(self.directory / "serial.previous.log")
                self.serial = (self.directory / "serial.log").open("wb")
                self.size = 0
            part, data = data[:self.limit-self.size], data[self.limit-self.size:]
            self.serial.write(part)
            self.size += len(part)
        self.serial.flush()

    def close(self):
        self.serial.close()
        if self.crash is not None:
            self.crash.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--log-root", type=Path, required=True)
    parser.add_argument("--latest", type=Path, required=True)
    parser.add_argument("--hash", type=Path, action="append", default=[])
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
        parser.error("missing QEMU command")
    args.log_root.mkdir(parents=True, exist_ok=True)
    directory = Path(tempfile.mkdtemp(prefix=datetime.now(timezone.utc).strftime("run-%Y%m%dT%H%M%SZ-"), dir=args.log_root)).resolve()
    metadata = {"logger_pid": os.getpid(), "command": command, "files": {}}
    for path in args.hash:
        if path.is_file():
            with path.open("rb") as stream:
                metadata["files"][str(path)] = hashlib.file_digest(stream, "sha256").hexdigest()
    meta = directory / "run.json"
    meta.write_text(json.dumps(metadata, indent=2) + "\n")
    # Keep this run and two completed runs, plus any live runs. Only prune our marked folders.
    old = sorted((p for p in args.log_root.glob("run-*") if p.is_dir() and not p.is_symlink() and p.resolve() != directory), reverse=True)
    completed = 0
    for path in old:
        try:
            record = json.loads((path / "run.json").read_text())
            if "exit_code" not in record and Path("/proc", str(record["logger_pid"])).exists():
                continue
        except (OSError, ValueError, KeyError):
            continue
        completed += 1
        if completed >= 3:
            shutil.rmtree(path)
    # The compatibility path follows the current segment, even after rotation.
    latest = args.latest.absolute()
    if latest.exists() and not latest.is_symlink():
        with latest.open("rb") as stream:
            stream.seek(0, 2)
            stream.seek(max(0, stream.tell()-SEGMENT))
            (directory / "previous-launch.log").write_bytes(stream.read())
    link = latest.with_name(latest.name + ".new")
    link.symlink_to(directory / "serial.log")
    link.replace(latest)
    capture = Capture(directory)
    print(f"Desktop serial/crash logs: {directory}", flush=True)
    child = None
    try:
        child = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        while data := child.stdout.read1(65536):
            capture.write(data)
        code = child.wait()
    except KeyboardInterrupt:
        if child is not None and child.poll() is None:
            child.terminate()
            try:
                child.wait(timeout=5)
            except subprocess.TimeoutExpired:
                child.kill()
                child.wait()
        code = 130
    finally:
        capture.close()
    metadata["exit_code"] = code
    meta.write_text(json.dumps(metadata, indent=2) + "\n")
    print(f"Desktop exited ({code}); logs retained at {directory}", flush=True)
    return code

if __name__ == "__main__":
    raise SystemExit(main())
