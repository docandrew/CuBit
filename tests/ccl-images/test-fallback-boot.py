#!/usr/bin/env python3
"""Boot the CCL-built all-in-initrd fallback without any writable disk."""
from pathlib import Path
import subprocess
import tempfile
import time

root = Path(__file__).resolve().parents[2]
run = Path(tempfile.mkdtemp(prefix="ccl-fallback-boot.", dir="/tmp"))
serial = run / "serial.log"
command = ["qemu-system-x86_64", "-enable-kvm", "-machine", "q35",
           "-cpu", "host", "-smp", "2", "-m", "4G",
           "-cdrom", str(root / "kernel/cubit_laptop_live.iso"),
           "-boot", "d", "-nic", "none", "-display", "none",
           "-audiodev", "none,id=sound", "-device", "ich9-intel-hda",
           "-device", "hda-output,audiodev=sound",
           "-serial", f"file:{serial}", "-no-reboot"]
print(f"Fallback boot logs: {run}", flush=True)
with (run / "qemu.log").open("w") as log:
    process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 60
        while time.monotonic() < deadline and process.poll() is None:
            text = serial.read_text(errors="replace") if serial.exists() else ""
            if any(marker in text for marker in
                   ("EXCEPTION", "PANIC", "boot denied", "Illegal memory access")):
                raise RuntimeError("native fault; inspect serial.log")
            if all(marker in text for marker in (
                    "devmgr: system.ccl seeded into config",
                    "procmgr: init.ccl: 5 entries",
                    "desktop: display info ready")):
                print("PASS: CCL-built fallback reaches desktop without a writable disk")
                break
            time.sleep(0.2)
        else:
            raise RuntimeError("fallback did not reach desktop; inspect serial.log")
    finally:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
