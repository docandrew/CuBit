#!/usr/bin/env python3
"""Hosted 1TiB directory geometry, not a GPU or physical-backing test.

Run under Nix. Copies production inputs into a disjoint evidence directory;
records hashes and rechecks originals after execution. No shared build lock
is needed for execution; repository edits still follow coordination rules.
"""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

assert os.environ.get("IN_NIX_SHELL"), "run under nix develop"
fixture = Path(__file__).resolve().parent / "extent_directory_scale_tests.adb"
root = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else fixture.parents[2]
source = root / "userspace/services/intel-gpu"
names = ["intel_gpu_extent_directory.ads", "intel_gpu_extent_directory.adb",
         "intel_gpu_record_store.ads", "intel_gpu_record_store.adb",
         "intel_gpu_physical_extents.ads", "intel_gpu_physical_extents.adb"]
inputs = [fixture] + [source / name for name in names]
out = Path(tempfile.mkdtemp(prefix="extent-directory-scale-"))
print(f"Scale evidence: {out}", flush=True)
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
before = {str(path): digest(path) for path in inputs}
(out / "inputs.json").write_text(json.dumps(before, indent=2) + "\n")
for path in inputs:
    shutil.copyfile(path, out / path.name)
(out / "scale.gpr").write_text('''project Scale is
   for Source_Dirs use (".");
   for Object_Dir use "obj";
   for Main use ("extent_directory_scale_tests.adb");
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
   end Compiler;
end Scale;
''')
with (out / "build.log").open("w") as log:
    subprocess.run(["gprbuild", "-p", "-P", str(out / "scale.gpr")],
                   stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
with (out / "run.log").open("w") as log:
    subprocess.run([str(out / "obj/extent_directory_scale_tests")],
                   stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
assert all(digest(path) == before[str(path)] for path in inputs), "source changed during test"
output = (out / "run.log").read_text()
assert "PASS synthetic1TiB directory:" in output
(out / "result.json").write_text(json.dumps(dict(status="PASS",
    scope="Hosted synthetic directory geometry; NO GPU or physical backing allocation",
    source_hashes=before), indent=2) + "\n")
print(output.strip(), flush=True)
