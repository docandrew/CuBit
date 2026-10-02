#!/usr/bin/env python3
"""Check every configured CuBit static-library object's header dependencies.

Requires build-cubit.py. Host generators are excluded by compiler command;
optional executables (e.g. spirv2nir) are outside the archive-link inventory.
"""
import json
from pathlib import Path
import re
import shlex
import subprocess
import sys

root = Path(__file__).resolve().parents[2]
build = Path(sys.argv[1]).resolve()
source = build.parent / "source"
compiler = root / "tests/mesa-anv/native-compiler.sh"
cross = Path((root / "userspace/libc/build/cross-gcc").read_text().strip())
raw = Path((cross / "nix-support/orig-cc").read_text().strip())
cc = raw / "bin/x86_64-unknown-linux-musl-gcc"
version = subprocess.check_output([str(cc), "-dumpfullversion"], text=True).strip()
include = Path(subprocess.check_output(
    [str(cc), "-print-file-name=include"], text=True).strip()).resolve()
allowed = [source.resolve(), build,
           (root / "userspace/libc/build/sysroot/include").resolve(),
           (raw / "include/c++" / version).resolve(), include]
objects = set()
targets = json.loads((build / "meson-info/intro-targets.json").read_text())
library_dirs = {Path(filename + ".p").resolve()
                for target in targets if target["type"] == "static library"
                for filename in target["filename"]}
for entry in json.loads((build / "compile_commands.json").read_text()):
    command = shlex.split(entry["command"])
    if str(compiler) not in command:
        continue
    output = command[command.index("-o") + 1]
    if (build / output).resolve().parent in library_dirs:
        objects.add(output)
if not objects:
    raise SystemExit("No native-compiler target objects configured")

seen, headers = set(), set()
current = None
records = subprocess.check_output(["ninja", "-C", str(build), "-t", "deps"], text=True)
for line in records.splitlines():
    if not line:
        current = None
    elif not line.startswith("    "):
        match = re.fullmatch(r"(.+): #deps (\d+), deps mtime \d+ \((VALID|STALE)\)", line)
        current = None
        if match and match[1] in objects and match[3] == "VALID":
            current = match[1]
            if int(match[2]) == 0:
                raise SystemExit(f"Empty dependency record: {current}")
            seen.add(current)
    elif current:
        path = (build / line.strip()).resolve()
        if not any(path.is_relative_to(parent) for parent in allowed):
            raise SystemExit(f"Unexpected target dependency: {current}: {path}")
        if "linux" in path.parts:
            raise SystemExit(f"Linux header dependency: {current}: {path}")
        headers.add(path)
missing = objects - seen
if missing:
    raise SystemExit("Missing/stale target dependency records:\n" + "\n".join(sorted(missing)))
print(f"Native header audit PASS: {len(seen)}/{len(objects)} objects, "
      f"{len(headers)} unique dependencies")
