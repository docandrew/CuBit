#!/usr/bin/env python3
"""Compare upstream and patched SWGL gradient output and host timing.

Run: nix develop -c python3 tests/servo/test_swgl_gradient.py
Uses pinned registry source; copies it to a private directory. Does not modify
native build outputs. Tests the non-blending, non-dithered span routine directly,
not whole-browser performance or GPU rendering. Timing is reported, never a gate.
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--output-dir", type=Path)
args = parser.parse_args()
if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run this test through nix develop -c.")
if args.output_dir:
    directory = args.output_dir.resolve()
    directory.mkdir(parents=True, exist_ok=False)
else:
    directory = Path(tempfile.mkdtemp(prefix="penny-swgl-gradient-"))
print(f"ARTIFACTS: {directory}", flush=True)
spec = importlib.util.spec_from_file_location("servo_fixes", ROOT / "userspace/servo/crate_fixes.py")
fixes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixes)
edits = [e for e in fixes.FIXES["swgl-0.70.0"]["edits"] if e[0] == "src/swgl_ext.h"]
assert edits, "missing gradient patch"
source = next((ROOT / "userspace/rust/build/servo-work/cargo-home/registry/src").glob("*/swgl-0.70.0/src"))
shutil.copyfile(ROOT / "tests/servo/swgl_gradient_driver.cpp", directory / "driver.cpp")
shutil.copyfile(__file__, directory / "runner.py")
for variant in ("baseline", "candidate"):
    target = directory / variant
    shutil.copytree(source, target)
    (target / "load_shader.h").write_text("ProgramLoader load_shader(const char*) { return nullptr; }\n")
    if variant == "candidate":
        for _, old, new, how in edits:
            path = target / "swgl_ext.h"
            text = path.read_text()
            assert old in text and new not in text
            path.write_text(text.replace(old, new) if how == "all" else text.replace(old, new, 1))
    command = ["g++", "-O3", "-std=c++20", "-fno-math-errno", "-I" + str(target),
               str(directory / "driver.cpp"), "-o", str(directory / (variant + "-test"))]
    with (directory / (variant + "-build.log")).open("w") as log:
        subprocess.run(command, check=True, stdout=log, stderr=subprocess.STDOUT)

results = {}
# Reverse the order for a second pair to reveal basic ordering/warmup bias.
for index, variant in enumerate(("baseline", "candidate", "candidate", "baseline")):
    output = directory / f"{index}-{variant}.bin"
    text = subprocess.check_output([str(directory / (variant + "-test")), str(output)], text=True)
    (directory / f"{index}-{variant}.log").write_text(text)
    samples = [float(x) for x in re.findall(r"ms=([0-9.]+)", text)]
    assert len(samples) == 5 and "cases=1008 " in text
    results[f"{index}-{variant}"] = {"samples_ms": samples, "median_ms": statistics.median(samples)}
    if index:
        assert output.read_bytes() == (directory / "0-baseline.bin").read_bytes(), "gradient output differs"
result = {"result": "PASS", "cases": 1008, "output_bytes": (directory / "0-baseline.bin").stat().st_size,
          "runs": results, "compiler": subprocess.check_output(["g++", "--version"], text=True).splitlines()[0],
          "limitations": "Linux-hosted direct routine, no blending or dithering; not native CuBit, GPU or page-load performance. No quiet-host claim."}
(directory / "result.json").write_text(json.dumps(result, indent=2))
print(json.dumps(result, indent=2))
