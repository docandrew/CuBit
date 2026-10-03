"""Boot only the privileged allocation oracle; no production disk or staging."""
import pathlib
import subprocess
import sys
import time

directory = pathlib.Path(sys.argv[1]).resolve()
serial = directory / "serial.log"
required = (
    "native demand backing: privileged disposable fixture (NO GPU/IPC)",
    "native demand backing: 4112 page sentinels and metadata extension PASS",
    "TEST: PASS native demand backing 18MiB 17 objects (NO GPU/IPC)",
)
if len(sys.argv) > 2 and sys.argv[2] == "ipc":
    required = (
        "native allocation IPC: real loopback transport (NO GPU/ISOLATION)",
        "TEST: PASS native allocation IPC 17 saved replies 18 extents both directories grew 1 interleaved request (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "views":
    required = (
        "native view retention: real self-grants (NO GPU/ISOLATION)",
        "native legacy revoke: locked admission and reader drain PASS",
        "native forwarding blocks: 32 rounds eight retained roots and children PASS",
        "TEST: PASS native view retention 3 cycles two pins terminal child queued retirement (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "mappings":
    required = (
        "native mapping growth: forced metadata, real self-grants (NO GPU/ISOLATION)",
        "TEST: PASS native mapping growth 64-128-256 record65 retained readers drained (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "lifetime":
    required = (
        "grant lifetime: disposable native owner/PID oracle (NO GPU)",
        "grant lifetime: retired owner held PID and readable backing PASS",
        "TEST: PASS grant lifetime retired owner drained exact PID reused stale identities denied (NO GPU)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "capacity":
    required = (
        "grant capacity: lazy native records 4096 slots (NO GPU)",
        "grant capacity: 4096 live readers and exhaustion PASS",
        "TEST: PASS grant capacity 4096 readers exhausted boundary reused all retired (NO GPU)",
    )
with (directory / "qemu.log").open("w") as log:
    vm = subprocess.Popen([
        "qemu-system-x86_64", "-machine", "q35", "-accel", "tcg,thread=multi",
        "-cpu", "Broadwell", "-smp", "4", "-m", "512", "-display", "none",
        "-monitor", "none", "-serial", f"file:{serial}", "-no-reboot",
        "-cdrom", str(directory / "demand.iso"),
    ], stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 120
        while True:
            text = serial.read_text(errors="replace") if serial.exists() else ""
            for failure in ("TEST: FAIL", "PANIC", "EXCEPTION", "last chance", "Last chance"):
                if failure in text:
                    raise RuntimeError(f"native demand failure {failure}: {serial}")
            if all(marker in text for marker in required):
                positions = [text.index(marker) for marker in required]
                if positions != sorted(positions):
                    raise RuntimeError("native markers out of order")
                print(f"PASS native allocation oracle ({sys.argv[2] if len(sys.argv) > 2 else 'memory'}; NO GPU): {serial}", flush=True)
                break
            if vm.poll() is not None:
                raise RuntimeError(f"QEMU exited {vm.returncode} before oracle: {serial}")
            if time.monotonic() >= deadline:
                raise RuntimeError(f"native demand timeout: {serial}")
            time.sleep(0.25)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
