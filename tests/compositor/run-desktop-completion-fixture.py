"""Boot a built completion fixture under an externally held build.lock.

Restores the exact previously staged Desktop even on test failure. Run in Nix.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("fixture", type=Path)
    args = parser.parse_args()
    fixture = args.fixture.resolve()
    built = json.loads((fixture / "result.json").read_text())
    if built["status"] != "BUILT":
        raise RuntimeError("fixture has no successful build record")
    binary = (fixture / "desktop.svc").read_bytes()
    if hashlib.sha256(binary).hexdigest() != built["binary_sha256"]:
        raise RuntimeError("fixture binary hash mismatch")
    mode = built["mode"]
    if mode not in ("delayed", "unsafe", "retry", "source-retirement"):
        raise RuntimeError("unsupported completion fixture mode")
    spec = importlib.util.spec_from_file_location("completion_oracle", ROOT / "tests/compositor/check-desktop-completion.py")
    oracle = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(oracle)
    staged = ROOT / "kernel/isodir/boot/desktop.svc"
    saved = staged.read_bytes()
    (fixture / "staged-desktop.saved").write_bytes(saved)
    serial = fixture / "serial.log"
    log = fixture / "boot.log"
    if serial.exists() or log.exists():
        raise RuntimeError("refusing to overwrite previous boot evidence")
    env = {**os.environ, "CUBIT_TEST_MIXED_OUTPUTS": "1" if mode != "unsafe" else "0",
           "CUBIT_TEST_PRIMARY": "1" if mode != "unsafe" else "0",
           "CUBIT_TEST_ARRANGEMENT": "1" if mode != "unsafe" else "0",
           "CUBIT_TEST_SCALING": "1" if mode != "unsafe" else "0",
           "CUBIT_TEST_SETTLE_SECONDS": "2"}
    result = {"status": "INCOMPLETE", "mode": mode, "binary_sha256": built["binary_sha256"],
              "restored": False}
    try:
        staged.write_bytes(binary)
        with log.open("w") as output:
            run = subprocess.run(["bash", "tests/headless/run.sh", "--test",
                "desktop-dual-output" if mode != "unsafe" else "desktop-display",
                "--accel", "tcg,thread=multi", "--cpus", "4", "--timeout",
                "300" if mode != "unsafe" else "40", "--serial", str(serial)],
                cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT)
        result["headless_status"] = run.returncode
        # Unsafe intentionally stops Desktop before Workbench becomes ready,
        # so the normal Desktop observer must fail. Require our precise oracle.
        expected_status = 0 if mode != "unsafe" else 1
        if run.returncode != expected_status:
            raise RuntimeError(f"unexpected headless exit {run.returncode}, expected {expected_status}")
        oracle.check(serial.read_text(errors="replace"), mode)
        if mode != "unsafe":
            output = log.read_text(errors="replace")
            for group in ("primary", "scaling", "arrangement", "Desktop"):
                if f"PASS native {group}:" not in output:
                    raise RuntimeError(f"missing native interaction acceptance: {group}")
        result["status"] = "PASS"
    except BaseException as error:
        result.update(status="FAIL", error=str(error))
        raise
    finally:
        staged.write_bytes(saved)
        result["restored"] = staged.read_bytes() == saved
        result["restored_sha256"] = hashlib.sha256(saved).hexdigest()
        (fixture / "boot-result.json").write_text(json.dumps(result, indent=2) + "\n")
        if not result["restored"]:
            raise RuntimeError("staged Desktop restoration mismatch")
    print("PASS native Desktop completion fixture:", mode, fixture)


if __name__ == "__main__":
    main()
