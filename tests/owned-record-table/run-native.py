"""Boot the disposable owned-mapping test; no production disk or staging."""
import pathlib
import subprocess
import sys
import time

directory = pathlib.Path(sys.argv[1]).resolve()
serial = directory / "serial.log"
required = (
 "TEST: PASS native owned records 8193 pages twice, hole reuse, protection, interleaved reservation retirement",
 "native owned records: exiting with 64 live mappings",
 "Process.reclaimProcess: owned regions retired 64",
)
with (directory / "qemu.log").open("w") as log:
    vm = subprocess.Popen([
        "qemu-system-x86_64", "-machine", "q35", "-accel", "tcg,thread=multi",
        "-cpu", "Broadwell", "-smp", "4", "-m", "512", "-display", "none",
        "-monitor", "none", "-serial", f"file:{serial}", "-no-reboot",
        "-cdrom", str(directory / "demand.iso"),
    ], stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 180
        while True:
            text = serial.read_text(errors="replace") if serial.exists() else ""
            for failure in ("TEST: FAIL", "PANIC", "EXCEPTION", "last chance", "Last chance", "USER-MEMORY-FAULT"):
                if failure in text:
                    raise RuntimeError(f"native owned-record failure {failure}: {serial}")
            if all(marker in text for marker in required):
                positions = [text.index(marker) for marker in required]
                if positions != sorted(positions):
                    raise RuntimeError("native markers out of order")
                print(f"PASS native owned records and process-exit retirement: {serial}", flush=True)
                break
            if vm.poll() is not None:
                raise RuntimeError(f"QEMU exited {vm.returncode} before oracle: {serial}")
            if time.monotonic() >= deadline:
                raise RuntimeError(f"native owned-record timeout: {serial}")
            time.sleep(0.25)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
