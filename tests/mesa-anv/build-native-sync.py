#!/usr/bin/env python3
"""Nix + build lock: link a native CuBit sync regression, not a GPU demo."""
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
args = shlex.split(entry["command"])
if "native-compiler.sh" not in " ".join(args):
    raise SystemExit("CuBit native compile commands required")
out = Path(tempfile.mkdtemp(prefix="native-sync.", dir=root / "tests/mesa-anv/target"))
args[args.index("-c") + 1] = str(root / "tests/mesa-anv/cpu-sync-test.c")
for flag, suffix in (("-o", ".o"), ("-MF", ".d"), ("-MQ", ".o")):
    args[args.index(flag) + 1] = str(out / ("sync" + suffix))
subprocess.run(args + ["-UNDEBUG", "-DCUBIT_SYNC_NATIVE_TEST"], cwd=build, check=True)
with (out / "manifest.S").open("w") as assembly:
    subprocess.run([str(root / "userspace/ccl/build/manifest/ccl-manifest"),
                    str(root / "userspace/ccl/catalogs/native-runtime-services.ccl"),
                    str(root / "tests/mesa-anv/sync-manifest.ccl")],
                   stdout=assembly, check=True)
subprocess.run(["as", "--64", str(out / "manifest.S"), "-o", str(out / "manifest.o")], check=True)
subprocess.run(["bash", str(root / "tests/mesa-anv/native-compiler.sh"), "c",
                "-Wl,--gc-sections", str(out / "sync.o"), "--manifest",
                str(out / "manifest.o"), "-o", str(out / "native-mesa-sync.app")], check=True)
print("Native sync executable (not yet run):", out / "native-mesa-sync.app")
