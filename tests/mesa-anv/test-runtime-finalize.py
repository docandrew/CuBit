#!/usr/bin/env python3
"""Compile the patched real Mesa finalizer; hosted semantics, not GPU execution.

Run in Nix: python3 tests/mesa-anv/test-runtime-finalize.py SOURCE HOST_BUILD
SOURCE must have runtime-finalize.patch applied. Reuses the pinned host build's
compiler options without changing any host build artifact.
"""
import json
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
source = Path(sys.argv[1]).resolve()
host = Path(sys.argv[2]).resolve()
assert (source / "VERSION").read_text().strip() == "26.2.3"
entries = json.loads((host / "compile_commands.json").read_text())
entry, = [e for e in entries if e["file"].endswith("/dev/intel_device_info.c")]
out = Path(tempfile.mkdtemp(prefix="runtime-finalize.", dir=root / "tests/mesa-anv/target"))
args = shlex.split(entry["command"])
original = (Path(entry["directory"]) / entry["file"]).resolve().parents[3]
for i, arg in enumerate(args):
    if arg.startswith("-I"):
        include = (Path(entry["directory"]) / arg[2:]).resolve()
        if include.is_relative_to(original):
            args[i] = "-I" + str(source / include.relative_to(original))
for option, value in [("-MF", "device-info.d"), ("-MQ", "device-info.o")]:
    if option in args:
        args[args.index(option) + 1] = str(out / value)
args[args.index("-o") + 1] = str(out / "device-info.o")
args[args.index("-c") + 1] = str(source / "src/intel/dev/intel_device_info.c")
args = [a for a in args if a != "-DNDEBUG"]
subprocess.run(args, cwd=entry["directory"], check=True)
topology_args = list(args)
topology_args[topology_args.index("-o") + 1] = str(out / "topology.o")
topology_args[topology_args.index("-c") + 1] = str(source / "src/intel/dev/intel_device_info_topology.c")
for option, value in [("-MF", "topology.d"), ("-MQ", "topology.o")]:
    if option in topology_args:
        topology_args[topology_args.index(option) + 1] = str(out / value)
subprocess.run(topology_args, cwd=entry["directory"], check=True)
link_command = [
    "cc", "-std=c11", "-D_GNU_SOURCE", "-DHAVE_ENDIAN_H",
    "-Wall", "-Wextra", "-Werror", "-ffunction-sections", "-fdata-sections",
    "-isystem", str(source / "include"), "-isystem", str(source / "src"),
    "-isystem", str(host / "src"),
    "-I" + str(root / "userspace/mesa/anv"),
    str(root / "userspace/mesa/anv/cubit-topology.c"),
    str(root / "tests/mesa-anv/runtime-finalize-test.c"),
    str(out / "topology.o"), str(out / "device-info.o"),
    str(host / "src/intel/dev/libintel_dev.a"),
    str(host / "src/util/libmesa_util.a"),
    "-lm", "-pthread", "-lz", "-lzstd",
    "-Wl,--gc-sections", "-o", str(out / "test"),
]
subprocess.run(link_command, check=True)
subprocess.run([str(out / "test")], check=True)
discovery_command = list(link_command)
discovery_command[discovery_command.index(str(root / "tests/mesa-anv/runtime-finalize-test.c"))] = str(root / "tests/mesa-anv/cubit-device-info-test.c")
discovery_command[discovery_command.index("-o") + 1] = str(out / "discovery-test")
discovery_command += [str(root / "userspace/mesa/anv/cubit-device-info.c"),
                      str(root / "userspace/mesa/anv/cubit-device-query.c")]
subprocess.run(discovery_command, check=True)
subprocess.run([str(out / "discovery-test")], check=True)
print(f"Mesa runtime finalization PASS (host only): {out}")
