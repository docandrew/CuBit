"""Private early-boot allocator oracle. Invoke inside Nix via workspace run."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

workspace = Path.cwd().resolve()
assert os.environ.get("IN_NIX_SHELL")
assert workspace.name.startswith("buddy-charge-refunds-")
assert json.loads((workspace / ".cubit-build-workspace.json").read_text())["complete"]
assert os.environ.get("CUBIT_BUILD_WORKSPACE") == str(workspace)

for suffix in ("ads", "adb"):
    shutil.copy2(workspace / f"tests/retained-dma/buddy_charge_native.{suffix}",
                 workspace / f"kernel/src/buddy_charge_native.{suffix}")
patch = """--- a/kernel/src/kmain.adb
+++ b/kernel/src/kmain.adb
@@ -14,6 +14,7 @@
 with ACPI;
 with BootAllocator;
 with BuddyAllocator;
+with Buddy_Charge_Native;
 with Build;
 with Config;
 with Cpuid;
@@ -172,6 +173,10 @@
     Mem_mgr.setup (bootMemoryAreas (1 .. memAreaCount));
     earlyCheckpoint ("buddy allocator");
     BuddyAllocator.setup (bootAllocationMap (1 .. memAreaCount));
+    Buddy_Charge_Native.Run;
+    loop
+        x86.halt;
+    end loop;
     earlyCheckpoint ("storage pools");
     StoragePools.setup;
     earlyCheckpoint ("process tables");
"""
subprocess.run(["patch", "--batch", "--forward", "--fuzz=0", "-p1"],
               input=patch, text=True, check=True)
evidence = Path(tempfile.mkdtemp(prefix="charge-evidence-", dir=workspace))
with (evidence / "build.log").open("w") as log:
    subprocess.run(["make", "-C", "kernel", "cubit_kernel"],
                   stdout=log, stderr=subprocess.STDOUT, check=True)
boot = evidence / "iso/boot"
(boot / "grub").mkdir(parents=True)
shutil.copy2(workspace / "kernel/cubit_kernel", boot / "cubit_kernel")
(boot / "grub/grub.cfg").write_text('''serial --speed=115200 --unit=0
terminal_input serial console
terminal_output serial console
set default=0
set timeout=0
menuentry "CuBit charged allocator oracle (NO GPU)" {
    multiboot /boot/cubit_kernel
}
''')
with (evidence / "iso.log").open("w") as log:
    subprocess.run(["grub-mkrescue", "-o", str(evidence / "test.iso"),
                    str(evidence / "iso")], stdout=log,
                   stderr=subprocess.STDOUT, check=True)
serial = evidence / "serial.log"
marker = "PASS BUDDY-CHARGE: deferred pin, identity, reuse, block, growth, post-unlock callback"
with (evidence / "qemu.log").open("w") as log:
    vm = subprocess.Popen([
        "qemu-system-x86_64", "-machine", "q35", "-accel", "tcg,thread=single",
        "-cpu", "Broadwell", "-smp", "1", "-m", "512", "-display", "none",
        "-monitor", "none", "-serial", f"file:{serial}", "-no-reboot",
        "-cdrom", str(evidence / "test.iso")], stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 60
        while True:
            text = serial.read_text(errors="replace") if serial.exists() else ""
            if any(s in text for s in ("FAIL BUDDY-CHARGE", "PANIC", "EXCEPTION", "Last chance", "last chance")):
                raise RuntimeError(f"guest failure: {serial}")
            if marker in text:
                break
            if vm.poll() is not None or time.monotonic() >= deadline:
                raise RuntimeError(f"guest exited or timed out: {serial}")
            time.sleep(0.1)
    finally:
        if vm.poll() is None:
            vm.terminate()
            try:
                vm.wait(timeout=5)
            except subprocess.TimeoutExpired:
                vm.kill()
                vm.wait()
result = {"result": "PASS", "scope": "native single-CPU early-boot allocator, NO GPU",
          "kernel_sha256": hashlib.sha256((boot / "cubit_kernel").read_bytes()).hexdigest(),
          "fixture_sha256": hashlib.sha256((workspace / "kernel/src/buddy_charge_native.adb").read_bytes()).hexdigest(),
          "evidence": str(evidence)}
(evidence / "result.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result), flush=True)
