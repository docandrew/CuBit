"""Hosted tests of Penny's actual logical-tab module; run inside Nix.

This does not exercise the Ada bridge, Servo lifetime or native rendering.
Artifacts and source hashes are retained for the eventual bridge migration.
"""
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/servo/overlay/ports/cubitshell/src/tab_model.rs"
output = Path(tempfile.mkdtemp(prefix="tab-model-", dir=root / "tests/servo/build"))
digest = hashlib.sha256(source.read_bytes()).hexdigest()
projection = source.with_name("tab_projection.rs")
projection_digest = hashlib.sha256(projection.read_bytes()).hexdigest()
command = ["rustc", "--edition=2024", "--test", str(source), "-o", str(output / "tests")]
(output / "inputs.json").write_text(json.dumps({
    "source": str(source), "sha256": digest, "command": command,
    "projection": str(projection), "projection_sha256": projection_digest,
}, indent=2) + "\n")
subprocess.run(command, check=True)
result = subprocess.run([str(output / "tests")], text=True, capture_output=True)
(output / "tests.log").write_text(result.stdout + result.stderr)
print(result.stdout, end="")
print(result.stderr, end="")
result.check_returncode()
assert hashlib.sha256(source.read_bytes()).hexdigest() == digest, "source changed during tests"
assert hashlib.sha256(projection.read_bytes()).hexdigest() == projection_digest, "projection changed during tests"
print("PASS hosted logical-tab model; native bridge integration remains pending:", output)
