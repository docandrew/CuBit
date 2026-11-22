#!/usr/bin/env python3
"""Replace one file in a private ext2 test disk and verify installed bytes.

debugfs may return success when `write` refused an existing destination. Never
use its exit status alone as evidence that a fixture was installed.
"""
import hashlib
import pathlib
import subprocess
import sys
import tempfile


def digest(path):
    result = hashlib.sha256()
    with open(path, "rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            result.update(chunk)
    return result.digest()


def install(disk, source, destination):
    disk, source = pathlib.Path(disk), pathlib.Path(source).resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix="cubit-servo-install-") as directory:
        copied = pathlib.Path(directory) / "verified"
        # These are debugfs command tokens, not shell arguments. Reject control
        # characters and quoting syntax rather than interpolate ambiguous paths.
        for value in (str(source), destination, str(copied)):
            if not value or any(char in value for char in '\n\r"\\'):
                raise ValueError("unsupported debugfs path")
        script = f'rm "{destination}"\nwrite "{source}" "{destination}"\ndump "{destination}" "{copied}"\n'
        result = subprocess.run(["debugfs", "-w", "-f", "-", str(disk)],
                                input=script, text=True, capture_output=True)
        if result.returncode or not copied.is_file() or digest(source) != digest(copied):
            raise RuntimeError(f"Servo fixture verification failed for {destination}: {result.stderr}")


if __name__ == "__main__":
    install(*sys.argv[1:4])
