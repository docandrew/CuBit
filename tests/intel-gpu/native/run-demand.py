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
if len(sys.argv) > 2 and sys.argv[2] == "dma-retirement":
    required = (
        "native DMA retirement: disposable real owner cleanup (NO GPU)",
        "native DMA quota: adoption runtime denial overlap rollback and child response PASS",
        "native DMA retirement: live grant pins dead owner and readable backing PASS",
        "native DMA metadata: 160 owner lifecycles reclaim slabs and preserve orphan backing PASS",
        "TEST: PASS native DMA retirement 40 retained records orphan backing survives PID reuse ordinary cleanup (NO GPU)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "dma-growth":
    required = (
        "native DMA growth: real kernel allocations (NO GPU)",
        "TEST: PASS native DMA growth 96 records 192MiB 49152 sentinels rollback (NO GPU)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "metadata":
    required = (
        "native update metadata: real owned-memory reservations (NO GPU/ISOLATION)",
        "native update metadata: sparse17/18/900 stable growth and overlap PASS",
        "TEST: PASS native update metadata independent demand stable ranges retained failure (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "images":
    required = (
        "native image provider: real CPU grants, synthetic GPU/output (NO GPU/ISOLATION)",
        "native image consumers: real reader retirement discharges exact obligation PASS",
        "TEST: PASS native image provider writer drain lease exclusion and retirement (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "ipc":
    required = (
        "native allocation IPC: real loopback transport (NO GPU/ISOLATION)",
        "native allocation IPC: 64-probe gap search with interleaved completion PASS",
        "native allocation IPC: bounded local work and interleaved completion PASS",
        "TEST: PASS native allocation IPC 201 saved replies 19 extents both directories grew 2 interleaved requests (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "views":
    required = (
        "native view retention: real self-grants (NO GPU/ISOLATION)",
        "native legacy revoke: locked admission and reader drain PASS",
        "native image write exclusion: open producer denied and final retirement clears hold PASS",
        "native image lease: independent pin and real CPU reader drain PASS",
        "native retained reader: closed producer read-only terminal grant drain PASS",
        "native forwarding blocks: 32 rounds eight retained roots and children PASS",
        "TEST: PASS native view retention 3 cycles two pins terminal child queued retirement (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "accounting":
    required = (
        "native accounting adapter: production client and handler, separate processes (NO GPU)",
        "native accounting adapter: thirteen authenticated calls and one DMA extent PASS",
        "TEST: PASS native accounting adapter child transport completed (NO GPU)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "quota":
    required = (
        "native client quota: real IPC and backing, forced startup policy (NO GPU/ISOLATION)",
        "TEST: PASS native client quota thirteen IPC replies own accounting denial recovery retained charges (NO GPU/ISOLATION)",
    )
if len(sys.argv) > 2 and sys.argv[2] == "mappings":
    required = (
        "native mapping growth: forced metadata, real self-grants (NO GPU/ISOLATION)",
        "TEST: PASS native mapping growth preserves pending retirement",
        "TEST: PASS native bounded mapping poll retained readers drained",
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
