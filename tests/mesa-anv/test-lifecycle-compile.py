#!/usr/bin/env python3
"""Compile the real changed ANV units with the pinned Linux build's flags.

Regression compilation only: does not execute a Linux or CuBit GPU backend.
All objects/dependency files go to a unique output, not the baseline build.
"""
import json
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
source = Path(sys.argv[1]).resolve()
build = root / "tests/mesa-anv/build-host"
pristine = Path("/nix/store/6pnvm1jkh0a144pmcsbkykq5crhn695a-source")
output = Path(tempfile.mkdtemp(prefix="lifecycle-compile.", dir=root / "tests/mesa-anv/target"))
entries = json.loads((build / "compile_commands.json").read_text())

for unit in ("anv_device.c", "anv_physical_device.c", "anv_gem.c", "anv_queue.c", "anv_allocator.c",
             "i915/anv_kmd_backend.c", "xe/anv_kmd_backend.c",
             "anv_physical_device_common.c", "anv_physical_device_drm.c",
             "anv_perf.c", "common/intel_common.c"):
    suffix = ("src/intel/" if unit.startswith("common/") else "src/intel/vulkan/") + unit
    template = suffix.replace("anv_physical_device_common.c", "anv_physical_device.c")
    template = template.replace("anv_physical_device_drm.c", "anv_physical_device.c")
    entry, = [e for e in entries if e["file"].endswith("/" + template)]
    args = shlex.split(entry["command"])
    for i, arg in enumerate(args):
        prefix = "-I" if arg.startswith("-I") else ""
        path = arg[2:] if prefix else arg
        if path.startswith("-"):
            continue
        absolute = (build / path).resolve()
        if absolute.is_relative_to(pristine):
            args[i] = prefix + str(source / absolute.relative_to(pristine))
    target = output / (unit.replace("/", "_") + ".o")
    args[args.index("-c") + 1] = str(source / suffix)
    args[args.index("-o") + 1] = str(target)
    args[args.index("-MF") + 1] = str(target) + ".d"
    args[args.index("-MQ") + 1] = str(target)
    subprocess.run(args, cwd=build, check=True)
    print("Linux compile PASS:", unit, flush=True)
print("Objects:", output)
