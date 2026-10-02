#!/usr/bin/env python3
"""Run the actual Servo patcher in a private lazy copy of its source inputs."""
import builtins
import pathlib
import runpy
import shutil
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
ACTIVE = ROOT / "userspace/rust/build/servo-work/servo"
SCRIPT = ROOT / "userspace/servo/patch_servo.py"
original_open = builtins.open
original_argv = sys.argv
with tempfile.TemporaryDirectory(prefix="cubit-servo-overlay-") as directory:
    private = pathlib.Path(directory)

    def lazy_open(file, mode="r", *args, **kwargs):
        if isinstance(file, (str, pathlib.Path)):
            path = pathlib.Path(file)
            if path.is_relative_to(private) and not path.exists() and "r" in mode:
                source = ACTIVE / path.relative_to(private)
                if source.is_file():
                    path.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(source, path)
        return original_open(file, mode, *args, **kwargs)

    try:
        builtins.open = lazy_open
        sys.argv = [str(SCRIPT), str(private)]
        runpy.run_path(str(SCRIPT), run_name="__main__")
        snapshot = {p: (p.read_bytes(), p.stat().st_mtime_ns)
                    for p in private.rglob("*") if p.is_file()}
        runpy.run_path(str(SCRIPT), run_name="__main__")
        assert all((p.read_bytes(), p.stat().st_mtime_ns) == before for p, before in snapshot.items())
        # New graphics/font edits must work from upstream originals as well
        # as from the previously patched cache used by the native build.
        paths = ["components/paint/painter.rs", "components/shared/fonts/font_identifier.rs",
                 "components/fonts/platform/freetype/font.rs"]
        expected = {name: (private / name).read_bytes() for name in paths}
        for name in paths:
            (private / name).write_bytes(subprocess.check_output(
                ["git", "-C", str(ACTIVE), "show", f"HEAD:{name}"]))
        runpy.run_path(str(SCRIPT), run_name="__main__")
        assert all((private / name).read_bytes() == value for name, value in expected.items())
    finally:
        builtins.open = original_open
        sys.argv = original_argv
print("SERVO-OVERLAY-PATCH: PASS existing cache, upstream graphics/fonts, second-run bytes/mtime stable")
