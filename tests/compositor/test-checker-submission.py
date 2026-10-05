"""Hosted checker submission policy and exact FFI envelope; run under Nix."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "Use vulkan-affine-shell.nix"
work = Path(tempfile.mkdtemp(prefix="checker-submission-", dir=ROOT / "tests/compositor/build"))
print(work, flush=True)
inputs = {}
for directory in ("userspace/lib/compositor", "userspace/lib/display", "userspace/runtime/gnat", "tests/compositor"):
    for path in (ROOT / directory).iterdir():
        if path.is_file() and path.suffix in (".ads", ".adb", ".gpr", ".c", ".h"):
            inputs[str(path.relative_to(ROOT))] = hashlib.sha256(path.read_bytes()).hexdigest()
files = ["userspace/lib/compositor/" + name for name in (
    "vulkan_checker_ffi.ads", "vulkan_checker_ffi.adb", "vulkan_submission-checkers.ads", "vulkan_submission-checkers.adb", "vulkan_checker.h", "vulkan_checker_request.h")]
files += ["tests/compositor/checker_submission_tests.adb", "tests/compositor/checker_submission_mock.c"]
for relative in files:
    path = ROOT / relative
    data = path.read_bytes()
    (work / path.name).write_bytes(data)
    inputs[relative] = hashlib.sha256(data).hexdigest()
names = ", ".join('"' + Path(n).name + '"' for n in files)
(work / "checker.gpr").write_text(f'''project Checker extends "{ROOT / 'tests/compositor/vulkan_submission_mock.gpr'}" is
   for Source_Dirs use (".");
   for Source_Files use ({names});
   for Main use ("checker_submission_tests.adb");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
end Checker;
''')
(work / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
for command in (["gprbuild", "-q", "-p", "-P", str(work / "checker.gpr")],
                ["gnatprove", "-P", str(work / "checker.gpr"), "-u", "vulkan_submission-checkers.adb", "--level=2", "--timeout=30", "-j2"]):
    subprocess.run(["alr", "exec", "--", *command], cwd=ROOT / "kernel", check=True)
report = (work / "obj/gnatprove/gnatprove.out").read_text()
total = next(line for line in report.splitlines() if line.startswith("Total "))
assert total.split()[-2:] == [".", "."], total
result = subprocess.run([str(work / "checker_submission_tests")], text=True, capture_output=True)
(work / "tests.log").write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr, end="", flush=True)
result.check_returncode()
for relative, expected in inputs.items():
    assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == expected, relative
(work / "result.json").write_text(json.dumps({"status": "PASS", "proof": total.strip(),
    "scope": "hosted SPARK submission and mock C boundary; no Vulkan/native execution"}, indent=2) + "\n")
