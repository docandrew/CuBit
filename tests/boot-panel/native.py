#!/usr/bin/env python3
"""Real SMP boot-panel smoke/screenshot without starting a display owner.

Private GRUB staging, no initrd, no production image changes. The retained
panel is intentional; this is not the production desktop launch target.
"""
import json
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile
import time

root = Path(__file__).resolve().parents[2]
panic = "--panic" in sys.argv[1:]
run = Path(tempfile.mkdtemp(prefix="cubit-panel.", dir="/tmp"))
stage = run / "iso"
(stage / "boot/grub").mkdir(parents=True)
shutil.copyfile(root / "kernel/cubit_kernel", stage / "boot/cubit_kernel")
(stage / "boot/grub/grub.cfg").write_text(
    'set timeout=0\nset gfxmode=1024x768x32\nset gfxpayload=keep\n'
    'insmod all_video\ninsmod gfxterm\nterminal_output gfxterm\n'
    'menuentry "Boot panel test" { multiboot /boot/cubit_kernel; boot; }\n')
print(f"Native boot panel evidence: {run}", flush=True)
with (run / "iso.log").open("w") as log:
    subprocess.run(["grub-mkrescue", "-o", str(run / "panel.iso"), str(stage)],
                   stdout=log, stderr=subprocess.STDOUT, check=True)
with (run / "qemu.log").open("w") as log:
    vm = subprocess.Popen([
        "qemu-system-x86_64", "-accel", "kvm", "-machine", "q35", "-cpu", "host", "-smp", "4",
        "-m", "512", "-display", "none", "-nic", "none", "-no-reboot",
        "-cdrom", str(run / "panel.iso"), "-boot", "d",
        "-serial", f"file:{run / 'serial.log'}",
        "-qmp", f"unix:{run / 'qmp.sock'},server=on,wait=off"] +
        (["-S", "-gdb", f"unix:{run / 'gdb.sock'},server=on,wait=off"] if panic else []),
        stdout=log, stderr=subprocess.STDOUT)
    try:
        if panic:
            deadline = time.monotonic() + 10
            while not (run / "gdb.sock").exists():
                if vm.poll() is not None or time.monotonic() > deadline:
                    raise RuntimeError(f"No debugger socket: {run}")
                time.sleep(0.05)
            # Inject a known fatal stop after graphics setup, before APs start.
            # Reserve scratch on the large bootstrap stack, leaving the new
            # handler stack below the message. Never alter the on-disk kernel.
            script = run / "panic.gdb"
            script.write_text("\n".join([
                "set pagination off", "set confirm off", "set language c",
                f"file {root / 'kernel/cubit_kernel'}",
                f"target remote {run / 'gdb.sock'}",
                "hbreak boot_diagnostics__complete_step", "continue",
                "set $rsp = (((unsigned long long)$rsp - 1024) & ~15ULL) - 8",
                'set {char[24]}($rsp + 256) = "Boot panel test failure"',
                "set $rdi = $rsp + 256", "set $rsi = 0",
                "set $rip = (unsigned long long)__gnat_last_chance_handler",
                "detach", "quit"]) + "\n")
            with (run / "gdb.log").open("w") as debug_log:
                subprocess.run(["gdb", "-q", "-nx", "-batch", "-x", str(script)],
                               stdout=debug_log, stderr=subprocess.STDOUT,
                               check=True, timeout=30)
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            serial = run / "serial.log"
            output = serial.read_text(errors="replace") if serial.exists() else ""
            if not panic and ("PANIC" in output or "EXCEPTION" in output):
                raise RuntimeError(f"Native boot fault: {run}")
            expected = ("Stack trace unavailable; halting this CPU.\n" if panic
                        else "Starting scheduler on CPU 0")
            if expected in output:
                break
            if vm.poll() is not None:
                raise RuntimeError(f"QEMU exited: {run}")
            time.sleep(0.05)
        else:
            raise RuntimeError(f"Boot timeout: {run}")
        if panic:
            assert output.count("CUBIT KERNEL PANIC") == 1, output
            assert "EXCEPTION: Boot panel test failure" in output, output
            time.sleep(0.5)
            assert serial.read_text(errors="replace") == output, "Panic output not stable"
        else:
            assert all(f"CPU started: {cpu}" in output for cpu in (1, 2, 3)), output
        with socket.socket(socket.AF_UNIX) as connection:
            connection.settimeout(5)
            connection.connect(str(run / "qmp.sock"))
            stream = connection.makefile("rwb")
            json.loads(stream.readline())
            def command(name, arguments=None):
                stream.write((json.dumps({"execute": name, "arguments": arguments or {}}) + "\n").encode())
                stream.flush()
                while True:
                    reply = json.loads(stream.readline())
                    if "error" in reply:
                        raise RuntimeError(reply)
                    if "return" in reply:
                        return reply["return"]
            command("qmp_capabilities")
            command("stop")
            command("screendump", {"filename": str(run / "panel.ppm")})
        print("PASS native " + ("retained fatal panel" if panic else "four-CPU boot") +
              "; inspect panel.ppm for visual evidence", flush=True)
    finally:
        vm.terminate()
        try:
            vm.wait(timeout=5)
        except subprocess.TimeoutExpired:
            vm.kill()
            vm.wait()
