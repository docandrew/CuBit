#!/usr/bin/env python3
"""Penny's Servo and mozjs_sys forks are what userspace/servo/FORKS pins, and
their bases are the upstream sources Penny pinned before the forks (WEB-004).

- Servo: the checkout (SERVO_DIR, default servo-work/servo) is at the
  pinned commit with no local change; its root commit is an exact snapshot
  of UPSTREAM_REVISION (Servo's commit): the root's tree is UPSTREAM_TREE,
  upstream's tree for that commit, and its message names the commit;
  no Cargo.toml names a path package by absolute path.
- mozjs_sys: the fork checkout crate_fixes.py made is at its pinned commit;
  its root commit is the crates.io package byte for byte (the .crate's
  SHA-256 is the checksum Cargo.lock records).
"""
import hashlib
import io
import os
import pathlib
import re
import subprocess
import sys
import tarfile
import tempfile
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[2]
SERVO_TOOLS = ROOT / "userspace/servo"
SERVO = pathlib.Path(os.environ.get("SERVO_DIR", ROOT / "userspace/rust/build/servo-work/servo"))
MOZJS = ROOT / "userspace/rust/build/servo-crate-downloads/src/mozjs_sys-153.3.0-0.fork"
MOZJS_CRATE = ("mozjs_sys", "153.3.0-0", "8441c2a92373a58e822e9cc213a8dd01367ec8c532c1ffa88ad9e4ab88687c13")
pins = {f[0]: f[1] for f in (line.split() for line in (SERVO_TOOLS / "FORKS").read_text().splitlines())
        if f and not f[0].startswith("#")}
failures = []


def git(directory, *args):
    return subprocess.run(["git", "-C", str(directory), *args], check=True, capture_output=True,
                          text=True).stdout.strip()


def urls():
    """name -> (commit, URL) from FORKS, the environment overriding."""
    out = {}
    for line in (SERVO_TOOLS / "FORKS").read_text().splitlines():
        fields = line.split()
        if fields and not fields[0].startswith("#"):
            name, commit, url = fields
            out[name] = (commit, os.environ.get(f"PENNY_{name.upper()}_URL", url))
    return out


def history(directory, name):
    """The fork's whole history in the checkout (the build fetches only the
    pinned commit): its root is then the fork's base."""
    if git(directory, "rev-parse", "--is-shallow-repository") == "true":
        commit, url = urls()[name]
        subprocess.run(["git", "-C", str(directory), "fetch", "-q", "--unshallow", url, commit], check=True)


def check(condition, what):
    print(("PASS " if condition else "FAIL ") + what)
    if not condition:
        failures.append(what)


subprocess.run(["bash", str(SERVO_TOOLS / "fork-checkout.sh"), "servo", str(SERVO)], check=True)
check(git(SERVO, "rev-parse", "HEAD") == pins["servo"], "Servo checkout at the pinned commit")
check(git(SERVO, "status", "--porcelain", "--", ".", ":!Cargo.lock") == "", "Servo checkout has no local change")
history(SERVO, "servo")
upstream = (SERVO_TOOLS / "UPSTREAM_REVISION").read_text().strip()
upstream_tree = (SERVO_TOOLS / "UPSTREAM_TREE").read_text().strip()
roots = git(SERVO, "rev-list", "--max-parents=0", "HEAD").split()
check(len(roots) == 1 and git(SERVO, "rev-parse", roots[0] + "^{tree}") == upstream_tree,
      f"the Servo fork's root is upstream {upstream[:12]}'s tree {upstream_tree[:12]}")
check(len(roots) == 1 and upstream in git(SERVO, "log", "-1", "--format=%B", roots[0]),
      f"the Servo fork's root commit names upstream {upstream[:12]}")
absolute = [str(p.relative_to(SERVO)) for p in SERVO.rglob("Cargo.toml")
            if ".git" not in p.parts and "cubit-build" not in p.parts and "target" not in p.parts
            and re.search(r'^[^#\n]*\bpath\s*=\s*"/', p.read_text(errors="replace"), re.M)]
check(not absolute, f"no absolute path dependencies {absolute[:3]}")

sys.path.insert(0, str(SERVO_TOOLS))
import crate_fixes  # noqa: E402
crate_fixes.fetch(str(ROOT / "userspace/rust/build/servo-crate-downloads"))
check(git(MOZJS, "rev-parse", "HEAD") == pins["mozjs"], "mozjs_sys checkout at the pinned commit")
name, version, digest = MOZJS_CRATE
data = urllib.request.urlopen(crate_fixes.CRATES_IO_DOWNLOAD.format(name=name, version=version)).read()
check(hashlib.sha256(data).hexdigest() == digest, "the crates.io package has the pinned SHA-256")
history(MOZJS, "mozjs")
root = git(MOZJS, "rev-list", "--max-parents=0", "HEAD")
with tempfile.TemporaryDirectory(prefix="penny-mozjs-") as directory:
    published = pathlib.Path(directory) / "published"
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
        archive.extractall(published, filter="data")
    published = published / f"{name}-{version}"
    base = pathlib.Path(directory) / "base"
    base.mkdir()
    archive = subprocess.run(["git", "-C", str(MOZJS), "archive", root], check=True, capture_output=True).stdout
    with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
        tree.extractall(base, filter="data")

    def files(top):
        return {str(p.relative_to(top)): p.read_bytes() for p in top.rglob("*") if p.is_file()}

    check(files(published) == files(base), "the mozjs_sys fork's root is the crates.io package byte for byte")
print("PENNY-FORKS:", "FAIL" if failures else "PASS")
sys.exit(1 if failures else 0)
