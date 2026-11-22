"""PERF-010: what the build benchmark's CuBit side (bench_build.py) and Linux
side (bench_linux.py) share: the apps, the edit, and reading ccl-build's
"timing" lines (Build_Timing, schema ccl-build.timing/1) into a run."""
import json

SCHEMA = "perf-010.bench/1"
MAIN_UNIT = "main.adb"
APP_DIRECTORY = "userspace/apps/%s"
TIMING_PREFIX = "timing "
# The one-line edit: hello's greeting, a string literal in its main unit.
EDITED_APP = "hello"
EDIT_LINE = '"hello: hello from an app built on CuBit"'
EDIT_FORMAT = '"hello: hello from an app built on CuBit (edit %d)"'
# The apps (tests/device-builds/build.sh copies each to app-<name>/): hello,
# one unit and a manifest; secondary-stack-check, four units.
DEFAULT_APPS = "hello,secondary-stack-check"
DEFAULT_WARM = 3
DEFAULT_EDITS = 3


def app_sources(build_dir, app):
    """An app's sources as the host puts them: every file in app-<name>/."""
    return sorted(p for p in (build_dir / ("app-%s" % app)).iterdir() if p.is_file())


def parse_run(text, app, kind, index, exited_ok, wall_s):
    """A run record from a build's output: its step and build timings."""
    steps, record = [], None
    for line in text.splitlines():
        if line.startswith(TIMING_PREFIX):
            try:
                item = json.loads(line[len(TIMING_PREFIX):])
            except ValueError:
                continue
            if item.get("kind") == "step":
                steps.append(item)
            elif item.get("kind") == "build":
                record = item
    ran = sum(1 for s in steps if s["ran"])
    return {"app": app, "kind": kind, "index": index, "ok": exited_ok and record is not None,
            "ran": ran, "hit": len(steps) - ran, "host_wall_s": round(wall_s, 4),
            "build": record, "steps": steps}
