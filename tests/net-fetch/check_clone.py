#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""The headless net-fetch regression's judge of a git.clone (SELF-003 step
2): a clone the guest wrote, read back from its disk with debugfs rdump,
against the repository the fixture served, by the host's git.

check_clone.py CLONE_DIR SOURCE_REPO FORMAT URL

Prints one line per failed check and "check_clone: PASS" or "FAIL".
"""
import os
import subprocess
import sys

clone, source, object_format, url = sys.argv[1:5]
env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", LC_ALL="C")
failures = 0


def git(*args, cwd):
    return subprocess.run(["git", *args], cwd=cwd, env=env, capture_output=True)


def check(good, what):
    global failures
    if not good:
        failures += 1
        print("check_clone: FAIL:", what)


r = git("fsck", "--strict", "--no-dangling", cwd=clone)
check(r.returncode == 0, "git fsck --strict: " + r.stderr.decode()[:300])
check(git("rev-parse", "--show-object-format", cwd=clone).stdout.strip() == object_format.encode(),
      "the object format is " + object_format)
check(git("rev-parse", "HEAD", cwd=clone).stdout == git("rev-parse", "HEAD", cwd=source).stdout, "HEAD")
check(sorted(git("rev-list", "--all", cwd=clone).stdout.split()) ==
      sorted(git("rev-list", "--all", cwd=source).stdout.split()), "the same commits")
check(git("symbolic-ref", "HEAD", cwd=clone).stdout.strip() == b"refs/heads/main", "HEAD is refs/heads/main")
check(git("rev-parse", "v1^{}", cwd=clone).stdout == git("rev-parse", "v1^{}", cwd=source).stdout, "the tag v1")
check(git("rev-parse", "refs/remotes/origin/side", cwd=clone).stdout == git("rev-parse", "side", cwd=source).stdout,
      "origin/side")
check(git("status", "--porcelain", cwd=clone).stdout == b"", "git status is clean")
check(git("config", "remote.origin.url", cwd=clone).stdout.strip() == url.encode(), "origin's URL")
# Every file of HEAD's tree, byte for byte, and nothing more.
listing = git("ls-tree", "-r", "HEAD", cwd=source).stdout.decode().splitlines()
for line in listing:
    meta, path = line.split("\t", 1)
    oid = meta.split()[2]
    want = git("cat-file", "blob", oid, cwd=source).stdout
    full = os.path.join(clone, path)
    check(os.path.isfile(full) and open(full, "rb").read() == want, "the file " + path)
found = sorted(os.path.relpath(os.path.join(d, f), clone) for d, ds, fs in os.walk(clone)
               for f in fs if ".git" not in os.path.relpath(os.path.join(d, f), clone).split(os.sep))
check(found == sorted(l.split("\t", 1)[1] for l in listing), "nothing more in the working tree: %s" % found)
print("check_clone: %s %s" % ("PASS" if failures == 0 else "FAIL", clone))
sys.exit(1 if failures else 0)
