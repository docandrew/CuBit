#!/usr/bin/env python3
"""PERF-010: the same builds as bench_build.py, by Linux's ccl-build on this
host, for the comparison (hosted reference, not CuBit).

Usage (in the Nix shell, after tests/device-builds/build.sh):
  bench_linux.py BUILD_DIR [--out FILE] [--apps A,B] [--warm N] [--edits N]
                 [--sessions N]

Each session starts from an empty private Storehouse (BUILD_DIR/bench-
linux-store) and, per app as bench_build.py does on CuBit, builds cold,
warm (no-op) and after one-line edits, with ccl-build --timings. The source
tree is the device's subset (BUILD_DIR/root/work/remote) plus Linux's
ccl-manifest, where build-params.ccl names it for Linux. Writes FILE
(default BUILD_DIR/bench-linux.json) in bench_build.py's schema, with
"platform": "linux".
"""
import argparse
import json
import os
import shutil
import stat
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))
import bench_build_common as common  # noqa: E402

TOOL = REPO / "userspace/ccl/build/ccl-build/ccl-build"
MANIFEST_TOOL = "userspace/ccl/build/manifest/ccl-manifest"
DEFAULT_SESSIONS = 3

parser = argparse.ArgumentParser(description="PERF-010 Linux reference builds")
parser.add_argument("build_dir", type=Path)
parser.add_argument("--out", type=Path)
parser.add_argument("--apps", default=common.DEFAULT_APPS)
parser.add_argument("--warm", type=int, default=common.DEFAULT_WARM)
parser.add_argument("--edits", type=int, default=common.DEFAULT_EDITS)
parser.add_argument("--sessions", type=int, default=DEFAULT_SESSIONS)
options = parser.parse_args()
build_dir = options.build_dir.resolve()
store = build_dir / "bench-linux-store"
tree = build_dir / "bench-linux-tree"
out_dir = build_dir / "bench-linux-out"


def remove(path):
    """Removes a tree, Storehouse entries (read-only) included."""
    if not path.exists():
        return
    for root, dirs, files in os.walk(path):
        for name in dirs + files:
            p = Path(root) / name
            if not p.is_symlink():
                p.chmod(p.stat().st_mode | stat.S_IWUSR)
    path.chmod(path.stat().st_mode | stat.S_IWUSR)
    shutil.rmtree(path)


def put_app(app, main_text=None):
    target = tree / (common.APP_DIRECTORY % app)
    target.mkdir(parents=True, exist_ok=True)
    for source in common.app_sources(build_dir, app):
        if source.name == common.MAIN_UNIT and main_text is not None:
            (target / source.name).write_text(main_text)
        else:
            shutil.copy2(source, target / source.name)


def build(app, kind, index):
    component = tree / (common.APP_DIRECTORY % app) / "component.ccl"
    command = [str(TOOL), "build", "--store", str(store), "--source-root", str(tree),
               "--import", "gnat-15.3.0-x86_64-linux-musl=" + os.environ["CUBIT_GNAT"],
               "--import", "binutils-2.46=" + os.environ["CUBIT_BINUTILS"],
               "--out", str(out_dir), "--timings", str(component)]
    started = time.monotonic()
    done = subprocess.run(command, capture_output=True, text=True)
    wall = time.monotonic() - started
    run = common.parse_run(done.stdout, app, kind, index, done.returncode == 0, wall)
    print("bench-linux: %s %s %d: %s, %d ran, %d hit, %.3f s" % (
        app, kind, index, "ok" if run["ok"] else "FAILED", run["ran"], run["hit"], wall), flush=True)
    if not run["ok"]:
        print(done.stdout + done.stderr, flush=True)
    return run


def main():
    if not TOOL.exists():
        raise SystemExit("bench-linux: no %s (make -C kernel ccl-build)" % TOOL)
    remove(tree)
    shutil.copytree(build_dir / "root/work/remote", tree, symlinks=True)
    (tree / MANIFEST_TOOL).parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(REPO / MANIFEST_TOOL, tree / MANIFEST_TOOL)
    report = {"schema": common.SCHEMA, "platform": "linux", "where": "host", "runs": []}
    failed = 0
    for session in range(options.sessions):
        remove(store)
        remove(out_dir)
        for app in [a for a in options.apps.split(",") if a]:
            main_source = (build_dir / ("app-%s" % app) / common.MAIN_UNIT).read_text()
            put_app(app)
            report["runs"].append(build(app, "cold", session))
            for n in range(options.warm):
                report["runs"].append(build(app, "warm", session * options.warm + n))
            if app == common.EDITED_APP and common.EDIT_LINE in main_source:
                for n in range(options.edits):
                    # Unique per session too: the store is fresh, but keep
                    # the edits the same as the device's (edit 1, 2, ...).
                    put_app(app, main_source.replace(common.EDIT_LINE, common.EDIT_FORMAT % (n + 1)))
                    report["runs"].append(build(app, "edit", session * options.edits + n))
                put_app(app)
    failed = sum(1 for r in report["runs"] if not r["ok"])
    remove(store)
    remove(out_dir)
    remove(tree)
    out = options.out or build_dir / "bench-linux.json"
    out.write_text(json.dumps(report, indent=1) + "\n")
    print("bench-linux: %d runs, %d failed; results in %s" % (len(report["runs"]), failed, out), flush=True)
    sys.exit(1 if failed else 0)


main()
