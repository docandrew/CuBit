#!/usr/bin/env python3
"""Exercise real GRUB module admission without changing shared ISO staging."""
from pathlib import Path
import json
import re
import shutil
import socket
import subprocess
import tempfile
import time


def check_fatal_stop(process, serial, qmp_path, case, output):
    """Observe the real boot CPU twice, not just its last diagnostic text."""
    assert output.count("CUBIT KERNEL PANIC") == 1, output
    assert "EARLY: bootstrap allocator" not in output, output
    assert "Call Stack:" not in output, output
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as connection:
        connection.settimeout(3)
        connection.connect(str(qmp_path))
        with connection.makefile("rwb") as stream:
            assert "QMP" in json.loads(stream.readline())

            def command(name, arguments=None):
                request = {"execute": name, "id": name}
                if arguments is not None:
                    request["arguments"] = arguments
                stream.write(json.dumps(request).encode() + b"\n")
                stream.flush()
                while True:
                    line = stream.readline()
                    if not line:
                        raise RuntimeError("QMP disconnected during fatal-stop check")
                    response = json.loads(line)
                    if response.get("id") == name:
                        assert "error" not in response, response
                        return response["return"]

            command("qmp_capabilities")
            command("human-monitor-command", {"command-line": "cpu 0"})
            for observation in range(2):
                time.sleep(0.5)
                assert process.poll() is None, f"QEMU exited: {case}"
                registers = command("human-monitor-command",
                                    {"command-line": "info registers"})
                (case / f"halt-registers-{observation}.log").write_text(registers)
                flags = re.search(r"\b[ER]FL=([0-9a-fA-F]+)", registers)
                assert flags is not None, registers
                assert int(flags.group(1), 16) & 0x200 == 0, registers
                assert "HLT=1" in registers, registers
                assert serial.read_text(errors="replace") == output, "Panic output changed"


root = Path(__file__).resolve().parents[2]
run = Path(tempfile.mkdtemp(prefix="cubit-module-admission.", dir="/tmp"))
print(f"Module admission logs: {run}", flush=True)
cases = (
    ("duplicate", ["init.img", "init.img"], "Boot module admission: duplicate module name"),
    ("long-name", ["x" * 65], "Unterminated boot module name"),
    ("capacity", [f"module{i}" for i in range(65)], "Boot module catalog capacity exceeded"),
)
for label, names, expected in cases:
    case = run / label
    stage = case / "iso"
    (stage / "boot/grub").mkdir(parents=True)
    shutil.copyfile(root / "kernel/cubit_kernel", stage / "boot/cubit_kernel")
    (stage / "boot/payload").write_bytes(b"CuBit module admission fixture\n")
    modules = "".join(f"module /boot/payload {name}\n" for name in names)
    (stage / "boot/grub/grub.cfg").write_text(
        'set timeout=0\nset default=0\nmenuentry "module admission" {\n'
        'multiboot /boot/cubit_kernel\nset gfxpayload=text\n' + modules + '}\n')
    with (case / "iso.log").open("w") as log:
        subprocess.run(["grub-mkrescue", "-o", str(case / "boot.iso"), str(stage)],
                       stdout=log, stderr=subprocess.STDOUT, check=True)
    serial = case / "serial.log"
    qmp_path = case / "qmp.sock"
    with (case / "qemu.log").open("w") as log:
        process = subprocess.Popen([
            "qemu-system-x86_64", "-accel", "kvm", "-machine", "q35",
            "-cpu", "host", "-smp", "2", "-m", "512", "-display", "none",
            "-nic", "none", "-cdrom", str(case / "boot.iso"), "-boot", "d",
            "-serial", f"file:{serial}", "-no-reboot",
            "-qmp", f"unix:{qmp_path},server=on,wait=off"],
            stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 30
            while time.monotonic() < deadline:
                output = serial.read_text(errors="replace") if serial.exists() else ""
                if expected in output and "Stack trace unavailable; halting this CPU.\n" in output:
                    check_fatal_stop(process, serial, qmp_path, case, output)
                    print(f"PASS {label}: rejected before allocator admission; "
                          "original diagnostic preserved; CPU halted with interrupts disabled",
                          flush=True)
                    break
                if process.poll() is not None:
                    raise RuntimeError(f"{label}: QEMU exited; inspect {case}")
                time.sleep(0.1)
            else:
                raise RuntimeError(f"{label}: missing rejection; inspect {case}")
        finally:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
