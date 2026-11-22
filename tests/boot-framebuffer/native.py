#!/usr/bin/env python3
"""Mutate real GRUB handoffs at the Ada entry, never the production kernel.

Requires Nix tools, KVM and GDB. Uses private staging and Unix debug sockets.
RAM-backed fixtures deliberately don't represent a physical monitor: they test
reservation/mapping and reaching a working emergency renderer without a device.
"""
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

root = Path(__file__).resolve().parents[2]
run = Path(tempfile.mkdtemp(prefix="cubit-fb.", dir="/tmp"))
print(f"Framebuffer fixtures: {run}", flush=True)
stage = run / "iso"
(stage / "boot/grub").mkdir(parents=True)
shutil.copyfile(root / "kernel/cubit_kernel", stage / "boot/cubit_kernel")
(stage / "boot/grub/grub.cfg").write_text(
    'set timeout=0\nmenuentry "framebuffer admission" {\n'
    'multiboot /boot/cubit_kernel\nset gfxpayload=text\n}\n')
with (run / "iso.log").open("w") as log:
    subprocess.run(["grub-mkrescue", "-o", str(run / "boot.iso"), str(stage)],
                   stdout=log, stderr=subprocess.STDOUT, check=True)
# offset: (C scalar type, value). A valid modest RAM-backed BGRX description.
base = {88: ("unsigned long long", 0x18000000),
        96: ("unsigned int", 2560), 100: ("unsigned int", 640),
        104: ("unsigned int", 480), 108: ("unsigned char", 32),
        109: ("unsigned char", 1), 112: ("unsigned char", 16),
        113: ("unsigned char", 8), 114: ("unsigned char", 8),
        115: ("unsigned char", 8), 116: ("unsigned char", 0),
        117: ("unsigned char", 8)}
cases = (
    ("zero-pitch", {96: 0}, "Invalid boot framebuffer geometry", False),
    ("short-pitch", {96: 2556}, "Invalid boot framebuffer geometry", False),
    ("bad-masks", {112: 0}, "Unsupported boot framebuffer format", False),
    ("wrapping-base", {88: 0xfffffffffffff000}, "Invalid boot framebuffer mapping extent", False),
    ("cpu-width-overflow", {88: 1 << 36}, "Invalid boot framebuffer mapping extent", False),
    ("budget", {104: 65535}, "Boot framebuffer exceeds backend budget", False),
    ("kernel-overlap", {88: 0x100000}, "Boot framebuffer overlaps retained boot memory", False),
    ("low-memory-overlap", {88: 0x9f001}, "Boot framebuffer overlaps retained boot memory", False),
    ("ram-backed", {}, "[ OK ] ACPI tables loaded", True),
    ("pitched-unaligned-ram", {88: 0x18000001, 96: 2576}, "[ OK ] ACPI tables loaded", True),
)
for name, changes, expected, accepted in cases:
    case = run / name
    case.mkdir()
    debug = case / "gdb.sock"
    serial = case / "serial.log"
    with (case / "qemu.log").open("w") as log:
        vm = subprocess.Popen([
            "qemu-system-x86_64", "-accel", "kvm", "-machine", "q35",
            "-cpu", ("host,host-phys-bits=off,phys-bits=36" if name == "cpu-width-overflow" else "host"),
            "-smp", "2", "-m", "512", "-display", "none",
            "-nic", "none", "-cdrom", str(run / "boot.iso"), "-boot", "d",
            "-serial", f"file:{serial}", "-no-reboot", "-S",
            "-gdb", f"unix:{debug},server=on,wait=off"],
            stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 10
            while not debug.exists():
                if vm.poll() is not None or time.monotonic() > deadline:
                    raise RuntimeError(f"No GDB socket: {case}")
                time.sleep(0.05)
            commands = ["set pagination off", "set confirm off", "set language c",
                        f"file {root / 'kernel/cubit_kernel'}",
                        f"target remote {debug}", "hbreak *kmain", "continue",
                        "set $fb = (unsigned long long)$rsi"]
            for offset, (ctype, value) in base.items():
                commands.append(f"set *({ctype}*)($fb + {offset}) = {changes.get(offset, value)}")
            commands += ["detach", "quit"]
            script = case / "inject.gdb"
            script.write_text("\n".join(commands) + "\n")
            with (case / "gdb.log").open("w") as gdb_log:
                subprocess.run(["gdb", "-q", "-nx", "-batch", "-x", str(script)],
                               stdout=gdb_log, stderr=subprocess.STDOUT, check=True, timeout=30)
            deadline = time.monotonic() + 20
            while time.monotonic() < deadline:
                output = serial.read_text(errors="replace") if serial.exists() else ""
                if expected in output:
                    if not accepted:
                        assert "EARLY: bootstrap allocator" not in output, output
                        # Wait for a complete, stable terminal panic diagnostic.
                        if "Stack trace unavailable; halting this CPU.\n" not in output:
                            time.sleep(0.05)
                            continue
                        time.sleep(0.5)
                        assert vm.poll() is None, output
                        assert serial.read_text(errors="replace") == output, output
                        assert output.count("CUBIT KERNEL PANIC") == 1, output
                    print(f"PASS {name}", flush=True)
                    break
                if vm.poll() is not None:
                    raise RuntimeError(f"QEMU exited: {case}")
                time.sleep(0.05)
            else:
                raise RuntimeError(f"Missing {expected!r}: {case}")
        finally:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
