#!/usr/bin/env python3
"""Hosted policy tests and SPARK proof; native/FFI evidence is separate."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser(description=__doc__)
p.add_argument("--source-root", type=Path, default=root)
p.add_argument("--toolchain-root", type=Path, default=root)
a = p.parse_args()
assert os.environ.get("IN_NIX_SHELL"), "Run in the CuBit Nix environment"
r, toolchain = a.source_root.resolve(), a.toolchain_root.resolve()
w = Path(tempfile.mkdtemp(prefix="cubit-presentation-policy-", dir="/tmp"))
units = ("compositor_presentation", "compositor_pool", "compositor_damage", "compositor_frame_replacement")
inputs = {}
for name in [*(u + ext for u in units for ext in (".ads", ".adb")),
             "presentation_tests.adb", "frame_replacement_tests.adb"]:
    src = r / ("tests/compositor" if name.endswith("_tests.adb") else "userspace/lib/compositor") / name
    data = src.read_bytes()
    inputs[str(src)] = hashlib.sha256(data).hexdigest()
    (w / name).write_bytes(data)
(w / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("presentation_tests.adb", "frame_replacement_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Test;
''')
def run(args):
    subprocess.run(args, cwd=toolchain / "kernel", check=True)
run(["alr", "exec", "--", "gprbuild", "-q", "-p", "-P", str(w / "test.gpr")])
for name in ("presentation_tests", "frame_replacement_tests"):
    run([str(w / name)])
run(["alr", "exec", "--", "gnatprove", "-P", str(w / "test.gpr"), "-u",
     *(u + ".adb" for u in units), "--level=2", "--timeout=30", "-j2"])
report = (w / "obj/gnatprove/gnatprove.out").read_text()
summary = next(line for line in report.splitlines() if line.startswith("Total "))
assert summary.split()[-2:] == [".", "."], summary
for src, expected in inputs.items():
    assert hashlib.sha256(Path(src).read_bytes()).hexdigest() == expected, "Source changed: " + src
(w / "result.json").write_text(json.dumps({"status": "PASS", "scope": "hosted policy, not native/FFI",
    "proof_summary": summary, "source_sha256": inputs}, indent=2) + "\n")
print(w)
