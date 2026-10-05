"""Exercise bounded serial rotation and immutable first-crash capture on the host."""
import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile
import json

script = Path(__file__).resolve().parents[2] / "tools/run_logged_qemu.py"
spec = importlib.util.spec_from_file_location("capture", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
with tempfile.TemporaryDirectory(prefix="penny-log-test-") as tmp:
    root = Path(tmp)
    capture = module.Capture(root, limit=128)
    capture.write(b"context\n")
    for char in b"PENNY-ABORT: test\n":
        capture.write(bytes([char]))
    capture.write(b"trace-address\n")
    capture.write(b"x" * (module.AFTER + 1000))
    capture.close()
    assert b"PENNY-ABORT: test\ntrace-address" in (root / "crash.log").read_bytes()
    assert (root / "crash.log").stat().st_size <= module.CONTEXT + module.AFTER
    assert (root / "serial.log").stat().st_size <= 128
    assert (root / "serial.previous.log").stat().st_size <= 128
    logs = root / "runs"
    for index in range(4):
        result = subprocess.run([sys.executable, str(script), "--log-root", str(logs),
            "--latest", str(root / "latest.log"), "--hash", str(script), "--",
            sys.executable, "-c", "print('USER-MEMORY-FAULT: test'); raise SystemExit(7)"],
            capture_output=True, text=True)
        assert result.returncode == 7, result
    runs = list(logs.glob("run-*"))
    assert len(runs) == 3, runs
    assert b"USER-MEMORY-FAULT" in (root / "latest.log").read_bytes()
    for run in runs:
        record = json.loads((run / "run.json").read_text())
        assert record["exit_code"] == 7 and record["files"]
        assert (run / "crash.log").exists()
print("PASS: bounded rotation, split markers, crash retention, exit status, run retention")
