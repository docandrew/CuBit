#!/usr/bin/env python3
"""Hosted production-source regression and isolated negative controls.

Run under Nix. Retains source hashes, mutations and build/run logs. Does not
modify production sources, build an image, or validate GPU execution.
"""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "run under nix develop"
SOURCE = ROOT / "userspace/services/intel-gpu"
TESTS = ROOT / "tests/intel-gpu"
OUT = Path(tempfile.mkdtemp(prefix="bounded-gap-"))
print(f"Bounded gap evidence: {OUT}", flush=True)
body_name = "intel_gpu_extent_allocator.adb"
body = (SOURCE / body_name).read_text()
cases = [
    ("positive", None, None, None),
    ("unbounded", "if Bounded then Gap_Probe_Limit",
     "if False then Gap_Probe_Limit", "gap-probe-bound"),
    ("stale-retirement",
     "Object.Used := Object.Used - Records.Get (Object.Items, Index).Bytes;\n      Object.Scan_Active := False;",
     "Object.Used := Object.Used - Records.Get (Object.Items, Index).Bytes;",
     "retirement-restarts-gap"),
    ("stale-insertion",
     "Object.Unassigned := Object.Unassigned - 1;\n         Object.Scan_Active := False;",
     "Object.Unassigned := Object.Unassigned - 1;", "live-buffer-address"),
]
inputs = sorted(SOURCE.glob("*.ad[bs]")) + [
    TESTS / "extent_allocator.gpr", TESTS / "extent_allocator_tests.adb"]
hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest()
          for p in inputs}
(OUT / "inputs.json").write_text(json.dumps(hashes, indent=2) + "\n")
results = []
for name, old, new, expected in cases:
    case = OUT / name
    for p in inputs:
        target = case / p.relative_to(ROOT)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(p, target)
    if old is not None:
        assert body.count(old) == 1, f"mutation anchor changed: {name}"
        (case / "userspace/services/intel-gpu" / body_name).write_text(body.replace(old, new))
    project = case / "tests/intel-gpu/extent_allocator.gpr"
    with (case / "build.log").open("w") as log:
        built = subprocess.run(["gprbuild", "-p", "-P", str(project)],
                               stdout=log, stderr=subprocess.STDOUT, timeout=120)
    assert built.returncode == 0, f"{name}: build failed; not a valid negative control"
    executable = case / "tests/intel-gpu/build-extent-allocator/extent_allocator_tests"
    run = subprocess.run([str(executable)], capture_output=True, text=True, timeout=30)
    output = run.stdout + run.stderr
    (case / "run.log").write_text(output)
    if expected is None:
        assert run.returncode == 0 and "Bounded gap search PASS:" in output, output
    else:
        assert run.returncode != 0 and "ASSERTION_ERROR" in output and expected in output, output
    results.append(dict(case=name, exit_code=run.returncode, expected_assertion=expected))
    print(f"PASS {name}", flush=True)
assert all(hashlib.sha256(p.read_bytes()).hexdigest() == hashes[str(p.relative_to(ROOT))]
           for p in inputs), "source changed during test"
(OUT / "result.json").write_text(json.dumps(dict(status="PASS", cases=results), indent=2) + "\n")
