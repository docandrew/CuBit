#!/usr/bin/env python3
"""Prove and exercise byte-counter delta policy in an isolated output directory.

Use the pinned Nix environment. This checks arithmetic/policy only, not clocks,
IPC, grant ownership, GPU execution or physical presentation.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

root = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--output", required=True, type=Path, help="new evidence directory")
parser.add_argument("--toolchain-root", type=Path, default=root)
args = parser.parse_args()
if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run inside the pinned CuBit Nix environment")
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=False)
inputs = {}
for source in [root / "userspace/lib/compositor" / ("compositor_transfer_delta." + ext)
               for ext in ("ads", "adb")] + [Path(__file__).with_name("transfer_delta_tests.adb")]:
    data = source.read_bytes()
    inputs[str(source)] = hashlib.sha256(data).hexdigest()
    (out / source.name).write_bytes(data)
(out / "test.gpr").write_text('''project Test is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("transfer_delta_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;''')
commands = []
def run(command):
    commands.append(command)
    subprocess.run(command, cwd=args.toolchain_root / "kernel", check=True)
run(["alr", "exec", "--", "gnatprove", "-P", str(out / "test.gpr"),
     "-u", "compositor_transfer_delta.adb", "--mode=all", "--level=2",
     "--timeout=30", "-j2", "--report=all"])
report = (out / "obj/gnatprove/gnatprove.out").read_text()
summary = next(line for line in report.splitlines() if line.startswith("Total "))
assert summary.split()[-2:] == [".", "."], summary
run(["alr", "exec", "--", "gprbuild", "-p", "-P", str(out / "test.gpr")])
run([str(out / "transfer_delta_tests")])
for name, expected in inputs.items():
    assert hashlib.sha256(Path(name).read_bytes()).hexdigest() == expected, name
(out / "result.json").write_text(json.dumps({"status": "PASS", "proof": summary,
    "scope": "Pure delta policy proof and boundary regression only",
    "inputs": inputs, "commands": commands}, indent=2) + "\n")
