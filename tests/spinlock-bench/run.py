#!/usr/bin/env python3
"""Build isolated native test kernels and compare unlock implementations in KVM.

Run through nix develop. Generated sources, kernels and ISOs stay under build/;
normal kernel sources and the desktop ISO are never replaced by the experiment.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
KERNEL = ROOT / "kernel"
sys.path.insert(0, str(ROOT / "tests/headless"))
from qemu_affinity import cpu_list

CAS_RELEASE = '''        if not Compare_Exchange (S, Before, After) then
            raise SpinLockException with "Spinlock ownership changed during release";
        end if;'''
STORE_RELEASE = '''        -- Experimental owner-only release for ordinary write-back memory.
        -- The successful Locks.Release check above establishes CPU ownership;
        -- IRQ exclusion prevents another local context from releasing it.
        -- An aligned x86 32-bit store is atomic. x86 store ordering publishes
        -- protected WB stores before this store; memory clobber orders compiler
        -- accesses. This is NOT a replacement for an MMIO/WC/NT-store fence.
        Asm ("movl %1, %0",
             Outputs => Locks.State'Asm_Output ("=m", S.Owner),
             Inputs => Locks.State'Asm_Input ("r", After),
             Clobber => "memory", Volatile => True);'''


def replace_once(text, old, new):
    if text.count(old) != 1:
        raise ValueError("production source changed; review the benchmark injection")
    return text.replace(old, new, 1)


def checked(command, cwd, log):
    with log.open("w") as output:
        subprocess.run(command, cwd=cwd, stdout=output, stderr=subprocess.STDOUT, check=True)


def build(workspace, variant):
    stage = workspace / variant
    stage.mkdir()
    shutil.copytree(KERNEL / "src", stage / "src")
    (stage / "runtime").symlink_to(KERNEL / "runtime", target_is_directory=True)
    for name in ("cubit.gpr", "linker.ld"):
        shutil.copy2(KERNEL / name, stage / name)
    objects = stage / "build"
    objects.mkdir()
    # Runtime objects share the kernel's link directory. Copy the existing
    # objects, then force-rebuild every kernel project unit from staged source.
    for item in (KERNEL / "build").glob("*.o"):
        shutil.copy2(item, objects / item.name)
    shutil.copy2(KERNEL / "build/init.bin", objects / "init.bin")
    boot = stage / "src/kmain.adb"
    text = replace_once(boot.read_text(), "with Scheduler;", "with Scheduler;\nwith Spinlock_Benchmark;")
    text = replace_once(text, '    showBootStage ("CPU initialization complete");',
                        '    Spinlock_Benchmark.Run (0, acpi.numCPUs);')
    text = replace_once(text, "    startingCPU := 0;",
                        "    startingCPU := 0;\n    Spinlock_Benchmark.Run (Natural(cpuNum), acpi.numCPUs);")
    boot.write_text(text)
    for suffix in ("ads", "adb"):
        shutil.copy2(HERE / f"spinlock_benchmark.{suffix}", stage / "src")
    for name in ("cubit.ads", "cubit-timing_histograms.ads", "cubit-timing_histograms.adb"):
        shutil.copy2(ROOT / "userspace/runtime/gnat" / name, stage / "src")
    spinlock = stage / "src/spinlocks.adb"
    original = spinlock.read_text()
    # Verify even the baseline still matches the intended comparison.
    candidate = replace_once(original, CAS_RELEASE, STORE_RELEASE)
    if variant == "release-store":
        spinlock.write_text(candidate)
    digest = hashlib.sha256(spinlock.read_bytes()).hexdigest()
    print(f"building {variant}: {stage}", flush=True)
    checked(["alr", "exec", "--", "gprbuild", "-f", "-b", "-c", f"-P{stage / 'cubit.gpr'}"],
            KERNEL, stage / "compile.log")
    checked(["ld", "-n", "-o", "cubit_kernel", "-T", "linker.ld",
             *[str(item.relative_to(stage)) for item in sorted(objects.glob("*.o"))],
             "-b", "binary", "build/init.bin", "-Map", "cubit_kernel.map"], stage, stage / "link.log")
    checked(["objdump", "-dr", str(objects / "spinlocks.o")], KERNEL, stage / "spinlocks.disassembly")
    iso = stage / "iso/boot"
    (iso / "grub").mkdir(parents=True)
    shutil.copy2(stage / "cubit_kernel", iso / "cubit_kernel")
    shutil.copy2(KERNEL / "isodir/boot/initrd.img", iso / "initrd.img")
    (iso / "grub/grub.cfg").write_text('''serial --speed=115200 --unit=0
terminal_input serial
terminal_output serial
set default=0
set timeout=0
menuentry "CuBit native spinlock experiment" {
    multiboot /boot/cubit_kernel
    set gfxpayload=1024x768x32
    module /boot/initrd.img init.img
}
''')
    checked(["grub-mkrescue", "-o", str(stage / "benchmark.iso"), str(stage / "iso")],
            KERNEL, stage / "iso.log")
    # Compile the existing host ownership/visibility/nested-lock/queue suite
    # against the staged spinlock body, leaving its other fixtures unchanged.
    (stage / "host.gpr").write_text(f'''project Host extends "{ROOT / 'tests/kernel-locking/locking_tests.gpr'}" is
    for Source_Dirs use ("src");
    for Source_Files use ("spinlocks.adb");
    for Object_Dir use "host";
    for Exec_Dir use "host";
end Host;
''')
    checked(["alr", "exec", "--", "gprbuild", "-p", f"-P{stage / 'host.gpr'}"],
            KERNEL, stage / "host-build.log")
    checked(["timeout", "30", str(stage / "host/main")], KERNEL, stage / "host-test.log")
    return {"path": str(stage), "spinlock_sha256": digest}


def parse(serial):
    rounds, samples = [], []
    for line in serial.splitlines():
        if line.startswith(("LOCK-ROUND:", "LOCK-SAMPLE:")):
            fields = dict(re.findall(r"(\w+)=\s*(\w+)", line))
            fields = {key: int(value) if value.isdigit() else value for key, value in fields.items()}
            (rounds if line.startswith("LOCK-ROUND:") else samples).append(fields)
    if ("LOCK-BENCH: PASS" not in serial or "LOCK-BENCH: FAIL" in serial or
            len(rounds) != 12 or len(samples) != 24):
        raise ValueError("incomplete or failed native spinlock benchmark")
    for phase, item in enumerate(rounds, 1):
        if (item.get("phase") != phase or item.get("valid") != "TRUE" or
                item.get("operations") != 200000 or item.get("elapsed_ticks", 0) <= 0 or
                item.get("repeat") != (phase - 1) // 4 + 1 or
                item.get("mode") != ("PRIVATE_LOCK" if (phase - 1) % 4 < 2 else "SHARED_LOCK") or
                item.get("measurement") != ("THROUGHPUT" if phase % 2 else "ACQUISITION_LATENCY")):
            raise ValueError("invalid round result")
        if phase % 2 == 0:
            per_cpu = [sample for sample in samples if sample.get("phase") == phase]
            if (len(per_cpu) != 4 or sorted(sample.get("cpu", -1) for sample in per_cpu) != list(range(4)) or
                    any(sample.get("count") != 50000 or
                        sample.get("p99", 0) < sample.get("p50", 0) or
                        sample.get("max", 0) <= 0 for sample in per_cpu)):
                raise ValueError("invalid per-CPU acquisition samples")
    return {"rounds": rounds, "samples": samples}


def run_one(workspace, variant, number, host_cpus):
    serial = workspace / f"{number}-{variant}.serial.log"
    run_log = workspace / f"{number}-{variant}.run.log"
    command = ["timeout", "45", "python3", str(ROOT / "tests/headless/qemu_affinity.py"),
               "--vcpu-cpus", host_cpus, "--count", "4", "--", "qemu-system-x86_64",
               "-accel", "kvm", "-machine", "q35", "-cpu", "Broadwell", "-smp", "4",
               "-m", "256M", "-display", "none", "-serial", f"file:{serial}",
               "-cdrom", str(workspace / variant / "benchmark.iso"),
               "-device", "isa-debug-exit,iobase=0xf4,iosize=0x04", "-no-reboot"]
    print(f"running {number}: {variant}", flush=True)
    with run_log.open("w") as output:
        result = subprocess.run(command, stdout=output, stderr=subprocess.STDOUT)
    if result.returncode != 33:
        raise ValueError(f"benchmark exit={result.returncode}; inspect {run_log} and {serial}")
    if len(re.findall(r"^AFFINITY: vcpu=.* verified=1$", run_log.read_text(), re.MULTILINE)) != 4:
        raise ValueError("missing verified host CPU mapping")
    return {"variant": variant, "serial": str(serial), "host_log": str(run_log), **parse(serial.read_text())}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host-cpus", default="2,3,4,5")
    parser.add_argument("--reuse", type=Path, help="reuse this experiment's already-built kernels")
    args = parser.parse_args()
    cpu_list(args.host_cpus, 4, os.sched_getaffinity(0))
    if args.reuse:
        workspace = args.reuse.resolve()
        if workspace.parent != (HERE / "build").resolve():
            raise ValueError("reuse path must name an experiment under tests/spinlock-bench/build")
        manifest = json.loads((workspace / "builds.json").read_text())
    else:
        (HERE / "build").mkdir(exist_ok=True)
        workspace = Path(tempfile.mkdtemp(prefix="experiment-", dir=HERE / "build"))
        print(f"experiment: {workspace}", flush=True)
        checked(["make", "-C", str(KERNEL), "cubit_kernel"], ROOT, workspace / "base-build.log")
        manifest = {variant: build(workspace, variant) for variant in ("compare-exchange", "release-store")}
        (workspace / "builds.json").write_text(json.dumps(manifest, indent=2) + "\n")
    # ABBA order: two boots per implementation, three repetitions per boot.
    captures = [run_one(workspace, variant, index, args.host_cpus) for index, variant in enumerate(
        ("compare-exchange", "release-store", "release-store", "compare-exchange"), 1)]
    report = {"host_cpus": args.host_cpus, "builds": manifest, "captures": captures}
    (workspace / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    for variant in manifest:
        for mode in ("PRIVATE_LOCK", "SHARED_LOCK"):
            rounds = [item for capture in captures if capture["variant"] == variant
                      for item in capture["rounds"] if item["mode"] == mode and item["measurement"] == "THROUGHPUT"]
            phases = {2, 6, 10} if mode == "PRIVATE_LOCK" else {4, 8, 12}
            samples = [item for capture in captures if capture["variant"] == variant
                       for item in capture["samples"] if item["phase"] in phases]
            print(f"{variant} {mode}: median aggregate ticks/op="
                  f"{statistics.median(item['elapsed_ticks'] / item['operations'] for item in rounds):.2f} "
                  f"median CPU p99={statistics.median(item['p99'] for item in samples):.0f} "
                  f"worst CPU p99={max(item['p99'] for item in samples)} "
                  f"max acquisition={max(item['max'] for item in samples)}", flush=True)
    print(f"results: {workspace / 'results.json'}", flush=True)


if __name__ == "__main__":
    main()
