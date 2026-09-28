#!/usr/bin/env python3
"""Private build snapshots, not a sandbox or a replacement for source ownership.

Create takes the main lock only while copying inputs. Run takes only the private
lock. No hard links, shared output symlinks, commits, or implicit publication.
"""
import argparse
from contextlib import contextmanager
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
MARKER = ".cubit-build-workspace.json"


@contextmanager
def locked(path):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a+") as handle:
        try:
            fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise RuntimeError(f"busy: {path}") from None
        yield


def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args])


def source_paths(root):
    tracked = set(git(root, "ls-files", "-z").decode().split("\0")) - {""}
    extra = set(git(root, "ls-files", "--others", "--exclude-standard", "-z")
                .decode().split("\0")) - {""}
    # Exclude untracked build/proof trees, logs and developer disk backups.
    # Tracked files always win; ignored inputs require explicit seeding.
    extra = {name for name in extra if not any(
        p in (".build-workspaces", "__pycache__", "target", "adalib") or
        p == "build" or p.startswith(("build-", "target-"))
        for p in Path(name).parts[:-1]) and
        Path(name).suffix not in (".log", ".pcap", ".img", ".iso") and
        ".pre-" not in Path(name).name}
    return sorted(name for name in tracked | extra
                  if (root / name).exists() or (root / name).is_symlink())


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def copy_input(root, destination, name, kind):
    relative = Path(name)
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError(f"unsafe input path: {name}")
    source = root / relative
    if source.resolve() != source.absolute() or not stat.S_ISREG(source.lstat().st_mode):
        raise ValueError(f"input must be an ordinary, non-symlink file: {name}")
    before = source.stat()
    if before.st_size > 128 * 1024 * 1024:
        raise ValueError(f"input exceeds 128MiB snapshot bound: {name}")
    target = destination / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, target)
    checksum = digest(target)
    after = source.stat()
    if ((before.st_ino, before.st_size, before.st_mtime_ns, before.st_ctime_ns) !=
        (after.st_ino, after.st_size, after.st_mtime_ns, after.st_ctime_ns) or
        digest(source) != checksum):
        raise RuntimeError(f"input changed during snapshot: {name}")
    return dict(path=name, kind=kind, sha256=checksum, size=after.st_size)


def live_inputs(root):
    # Seed binaries are explicit inputs, not claims they match current sources.
    # Kernel and runtime are rebuilt privately; services are reused for boot work.
    required = ["kernel/laptop_live_rw.img",
                "userspace/ccl/build/image/ccl-image",
                "userspace/ccl/build/config/ccl-config",
                "userspace/c/sameboy_build/test.gb"]
    required += [str(p.relative_to(root)) for p in sorted((root / "kernel/isodir/boot").iterdir())
                 if p.suffix in (".svc", ".drv", ".app", ".elf")]
    if not any(p.endswith("devmgr.svc") for p in required):
        raise RuntimeError("live service staging is missing; build it first")
    return required


def create(root, label, seed_live=False):
    root = root.resolve()
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]{0,47}", label):
        raise ValueError("name must use 1-48 lowercase letters, digits or hyphens")
    with locked(root / "coordination/build.lock"):
        parent = root / ".build-workspaces"
        parent.mkdir(exist_ok=True)
        destination = Path(tempfile.mkdtemp(prefix=label + "-", dir=parent))
        report = dict(version=1, complete=False, source=str(root),
                      base_commit=git(root, "rev-parse", "HEAD").decode().strip(),
                      seed_live=seed_live, inputs=[])
        try:
            sources = source_paths(root)
            inputs = {name: "source" for name in sources}
            if seed_live:
                inputs.update({name: "seed-artifact" for name in live_inputs(root)})
            inputs = {n: k for n, k in inputs.items()
                      if not n.startswith(("coordination/", ".build-workspaces/"))}
            for name, kind in sorted(inputs.items()):
                report["inputs"].append(copy_input(root, destination, name, kind))
            # Catch ordinary concurrent edits during the snapshot interval.
            # Cross-file atomicity still relies on cooperative source ownership.
            if source_paths(root) != sources:
                raise RuntimeError("source file set changed during snapshot; retry")
            for entry in report["inputs"]:
                if digest(root / entry["path"]) != entry["sha256"]:
                    raise RuntimeError(f"input changed during snapshot: {entry['path']}")
            (destination / "coordination").mkdir(exist_ok=True)
            (destination / "tmp").mkdir()
            report["complete"] = True
        finally:
            (destination / MARKER).write_text(json.dumps(report, indent=2) + "\n")
            if not report["complete"]:
                print(f"Incomplete snapshot retained at {destination}", flush=True)
        return destination


def run(workspace, command):
    workspace = workspace.resolve()
    report = json.loads((workspace / MARKER).read_text())
    if report.get("version") != 1 or not report.get("complete"):
        raise ValueError("not a completed CuBit build workspace")
    if not command:
        raise ValueError("provide a command after --")
    if not os.environ.get("IN_NIX_SHELL"):
        raise RuntimeError("run builds/tests through nix develop")
    with locked(workspace / "coordination/build.lock"):
        env = os.environ.copy()
        env["TMPDIR"] = str(workspace / "tmp")
        env["CUBIT_BUILD_WORKSPACE"] = str(workspace)
        env.pop("SAMEBOY_ROMS_DIR", None)
        return subprocess.call(command, cwd=workspace, env=env)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="action", required=True)
    new = sub.add_parser("create")
    new.add_argument("name")
    new.add_argument("--seed-live", action="store_true")
    execute = sub.add_parser("run")
    execute.add_argument("workspace", type=Path)
    execute.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    try:
        if args.action == "create":
            print(create(ROOT, args.name, args.seed_live))
        else:
            command = args.command[1:] if args.command[:1] == ["--"] else args.command
            return run(args.workspace, command)
    except (OSError, ValueError, RuntimeError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"build-workspace: {error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
