#!/usr/bin/env python3
"""Nix + build lock: link a native CuBit sync regression, not a GPU demo.

The GPU timeline sync type (gpu-timeline-sync-test.c) with CuBit libc
threads and timed waits, the Ada timeline logic compiled for the CuBit
runtime, and the mocked session queue (gpu-queue-mock.c): no driver, no GPU.
"""
import json
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
build = Path(sys.argv[1]).resolve()
entries = json.loads((build / "compile_commands.json").read_text())
entry, = [e for e in entries if e["file"].endswith("/vulkan/anv_kmd_backend.c")]
base = shlex.split(entry["command"])
if "native-compiler.sh" not in " ".join(base):
    raise SystemExit("CuBit native compile commands required")
out = Path(tempfile.mkdtemp(prefix="native-sync.", dir=root / "tests/mesa-anv/target"))
native = root / "userspace/mesa/anv"
objects = []
for name in ("gpu-timeline-sync-test", "gpu-queue-mock"):
    args = list(base)
    args[args.index("-c") + 1] = str(root / "tests/mesa-anv" / (name + ".c"))
    for flag, suffix in (("-o", ".o"), ("-MF", ".d"), ("-MQ", ".o")):
        args[args.index(flag) + 1] = str(out / (name + suffix))
    subprocess.run(args + ["-UNDEBUG", "-DCUBIT_SYNC_NATIVE_TEST", "-I" + str(native)],
                   cwd=build, check=True)
    objects.append(str(out / (name + ".o")))
subprocess.run(["gnatmake", "-q", "-c", "-gnatA", "-gnat2022", "-O2", "-mno-red-zone", "-fno-pic",
                "--RTS=" + str(root / "userspace/runtime"), "-I" + str(native),
                str(native / "native_gpu_timeline.adb")], cwd=out, check=True)
objects.append(str(out / "native_gpu_timeline.o"))
with (out / "manifest.S").open("w") as assembly:
    subprocess.run([str(root / "userspace/ccl/build/manifest/ccl-manifest"),
                    str(root / "userspace/ccl/catalogs/native-runtime-services.ccl"),
                    str(root / "tests/mesa-anv/sync-manifest.ccl")],
                   stdout=assembly, check=True)
subprocess.run(["as", "--64", str(out / "manifest.S"), "-o", str(out / "manifest.o")], check=True)
subprocess.run(["bash", str(root / "tests/mesa-anv/native-compiler.sh"), "c",
                "-Wl,--gc-sections", *objects,
                str(root / "userspace/runtime/adalib/libgnat-user.a"), "--manifest",
                str(out / "manifest.o"), "-o", str(out / "native-mesa-sync.app")], check=True)
print("Native sync executable (not yet run):", out / "native-mesa-sync.app")
