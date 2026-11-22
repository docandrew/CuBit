#!/usr/bin/env python3
"""A late invalid declaration must prevent even earlier valid launches.

Run after make -C kernel iso nvme_disk.img, inside nix develop. Uses a private
copy of the development disk; the existing headless harness restores GRUB.
Do not run concurrently with other headless tests sharing kernel/isodir.
"""
import pathlib
import shutil
import subprocess
import tempfile

root = pathlib.Path(__file__).resolve().parents[2]
with tempfile.TemporaryDirectory(prefix="ccl-startup-rejection.") as temporary:
    directory = pathlib.Path(temporary)
    disk = directory / "disk.img"
    source = directory / "init.ccl"
    serial = directory / "serial.log"
    shutil.copyfile(root / "kernel/nvme_disk.img", disk)
    source.write_text('''(startup v1
      (start "shell.app" (priority (+ 2 3)))
      (start "sleep.app" (priority 0)))
''')
    for operation in ("rm init.ccl", f"write {source} init.ccl"):
        result = subprocess.run(["debugfs", "-w", "-R", operation, disk],
                                text=True, capture_output=True, check=True)
        if "File not found" in result.stderr or "Could not" in result.stderr:
            raise RuntimeError(result.stderr)
    run = subprocess.run(
        ["bash", root / "tests/headless/run.sh", "--test", "boot-shell-nvme",
         "--accel", "kvm", "--timeout", "25", "--disk", disk,
         "--serial", serial, "--keep-logs"],
        text=True, capture_output=True, timeout=90)
    log = serial.read_text(errors="replace") if serial.exists() else ""
    expected = ("devmgr: system.ccl seeded into config",
                "procmgr: init.ccl rejected", "INVALID_PRIORITY")
    if (run.returncode != 1 or not all(marker in log for marker in expected)
            or "procmgr: init spawn:" in log or "EXCEPTION" in log):
        print(run.stdout, run.stderr, log)
        raise SystemExit("FAIL: invalid native startup did not fail closed")
    print("PASS: native CCL rejects the whole startup plan before its first launch")
