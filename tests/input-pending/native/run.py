"""Disposable CuBit IPC oracle; no hardware-input or isolation claim."""
from pathlib import Path
import subprocess
import sys
import time

out = Path(sys.argv[1]).resolve()
serial = out / "serial.log"
required = (
    "native input retention: real loopback IPC (NO HID/ISOLATION)",
    "native input retention: transient mailbox=",
    "native input retention: overflow mailbox=",
    "TEST: PASS native input retention mailbox/refusal/deadline/authority",
)
with (out / "qemu.log").open("w") as log:
    vm = subprocess.Popen([
        "qemu-system-x86_64", "-machine", "q35", "-accel", "tcg,thread=multi",
        "-cpu", "Broadwell", "-smp", "4", "-m", "512", "-display", "none",
        "-monitor", "none", "-serial", f"file:{serial}", "-no-reboot",
        "-cdrom", str(out / "input.iso"),
    ], stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 90
        while True:
            text = serial.read_text(errors="replace") if serial.exists() else ""
            for failure in ("TEST: FAIL", "PANIC", "EXCEPTION", "last chance", "Last chance"):
                if failure in text:
                    raise RuntimeError(f"native input failure {failure}: {serial}")
            if all(marker in text for marker in required):
                positions = [text.index(marker) for marker in required]
                if positions != sorted(positions):
                    raise RuntimeError("native markers out of order")
                print(f"PASS native input retention (NO HID/ISOLATION): {serial}", flush=True)
                break
            if vm.poll() is not None:
                raise RuntimeError(f"QEMU exited {vm.returncode} before oracle: {serial}")
            if time.monotonic() >= deadline:
                raise RuntimeError(f"native input timeout: {serial}")
            time.sleep(0.25)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
