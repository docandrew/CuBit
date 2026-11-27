#!/usr/bin/env python3
"""Run the two AML-width region phase gates in a private hosted build tree."""
from pathlib import Path
from datetime import datetime
from zoneinfo import ZoneInfo
import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile

here = Path(__file__).resolve().parent
tree = here.parents[1]
tmp = os.environ.get("TMPDIR")
if not tmp or not Path(tmp).is_dir():
    raise SystemExit("export TMPDIR to an existing private build directory before Nix")
out = Path(tempfile.mkdtemp(prefix="acpi-dynamic-regions-", dir=tmp))
source = out / "source"
source.mkdir()
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
selected = {}
for relative in (
    "userspace/services/acpi", "userspace/services/acpi/lib",
    "userspace/services/acpi/region-init", "userspace/services/acpi/resource-consumer",
    "userspace/services/acpi/resource-data", "shared/acpi", "shared/firmware",
    "shared/platform",
):
    for path in (tree / relative).glob("*.ad[bs]"):
        selected[path.name] = path
for name in ("acpi_hosted_heap.ads", "acpi_hosted_heap.adb", "acpi_service_host.ads"):
    selected[name] = tree / "tests/acpi-consumer-phase/fixtures" / name
selected["phases_tests.adb"] = here / "phases_tests.adb"
inputs = {str(path): sha(path) for path in selected.values()}
for name, path in selected.items():
    shutil.copy2(path, source / name)
gpr = out / "phase.gpr"
gpr.write_text(
    'project Phase is\n'
    ' for Source_Dirs use ("source");\n'
    ' for Object_Dir use "obj";\n'
    ' for Exec_Dir use "bin";\n'
    ' for Main use ("phases_tests.adb");\n'
    ' for Excluded_Source_Files use ("acpi_service.ads");\n'
    ' package Naming is for Spec ("ACPI_Service") use "acpi_service_host.ads"; end Naming;\n'
    ' package Compiler is for Default_Switches ("Ada") use '
    '("-O0", "-gnatp", "-gnat2022", "-gnatwa", "-gnatwe"); end Compiler;\n'
    'end Phase;\n'
)
logs = {}
def run(label, argv, timeout_seconds):
    process = subprocess.run(argv, cwd=out, text=True, capture_output=True,
                             timeout=timeout_seconds)
    log = out / (label + ".log")
    log.write_text(process.stdout + process.stderr)
    logs[label] = {"argv": argv, "exit": process.returncode,
                   "log_sha256": sha(log)}
    if process.returncode != 0:
        raise RuntimeError(f"{label} failed; log: {log}")
    return process.stdout

run("build", ["timeout", "--kill-after=5s", "300s", "gprbuild", "-p", "-j2", "-P", str(gpr)], 310)
checks = {}
for width in (1, 2):
    prefix = out / f"normal{width}"
    asl = (here / "phase.asl").read_text().replace('"DSDT", 1', f'"DSDT", {width}', 1)
    (prefix.with_suffix(".asl")).write_text(asl)
    run(f"iasl{width}", ["timeout", "--kill-after=2s", "20s", os.environ.get("IASL", "iasl"), "-p", str(prefix), str(prefix.with_suffix(".asl"))], 25)
    output = run(f"phase{width}", ["timeout", "--kill-after=2s", "30s",
        "prlimit", "--as=25769803776", "--stack=1073741824",
        str(out / "bin/phases_tests"), str(prefix.with_suffix(".aml")), "normal"], 35)
    match = re.search(r"^Registration phase checks\s+(\d+)\s*$", output, re.M)
    checks[str(width)] = int(match.group(1)) if match else 0
    print("PHASE_PROGRESS", width, "/2", "PASS", sum(x == 82 for x in checks.values()),
          "PERCENT", round(100 * sum(x == 82 for x in checks.values()) / width, 2), flush=True)
    if checks[str(width)] != 82:
        raise RuntimeError(f"width {width}: expected 82 checks, got {checks[str(width)]}")
for path, digest in inputs.items():
    if sha(path) != digest:
        raise RuntimeError(f"source changed during run: {path}")
receipt = {"time": datetime.now(ZoneInfo("America/Denver")).isoformat(),
           "out": str(out), "source_count": len(selected), "input_sha256": inputs,
           "phase_source_sha256": sha(here / "phases_tests.adb"),
           "fixture_sha256": sha(here / "phase.asl"), "binary_sha256": sha(out / "bin/phases_tests"),
           "checks": checks, "passing": 2, "total": 2, "logs": logs,
           "qualification": "Hosted 32/64-bit AML width gates only; native boot and SPARK proof separate."}
(out / "RESULT.json").write_text(json.dumps(receipt, indent=2) + "\n")
print("PHASE_RECEIPT", out / "RESULT.json", sha(out / "RESULT.json"), flush=True)
