#!/usr/bin/env python3
"""Build all configured static libraries for the native ANV link probe.

Meson build_by_default=false dependencies are not pulled in by a static ANV
archive alone. Use Meson's target inventory, not only archives already on disk.
Run inside tests/mesa-anv/host-shell.nix after configure-cubit.sh.
"""
import json
from pathlib import Path
import subprocess
import sys

build = Path(sys.argv[1]).resolve()
jobs = int(sys.argv[2]) if len(sys.argv) > 2 else 4
if jobs < 1:
    raise SystemExit("jobs must be positive")
targets = json.loads((build / "meson-info/intro-targets.json").read_text())
archives = []
for target in targets:
    if target["type"] != "static library":
        continue
    for filename in target["filename"]:
        path = Path(filename).resolve()
        if not path.is_relative_to(build) or path.suffix != ".a":
            raise SystemExit(f"Unexpected static archive path: {path}")
        archives.append(str(path.relative_to(build)))
if not archives:
    raise SystemExit("No configured static archives")
subprocess.run(["ninja", "-C", str(build), f"-j{jobs}", *sorted(set(archives))], check=True)
print(f"Native static libraries built: {len(set(archives))}; not an application link")
