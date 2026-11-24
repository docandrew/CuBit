#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: tests of the clone writer (Git_Clones) through
#  build/clone_tool, on the Linux host. Repositories are made here by the
#  host's git (hermetic: no user or system config); the three answers a
#  clone gets come from `git upload-pack --stateless-rpc`. The host's git
#  then judges each clone: fsck --strict, log, status, checkout, and the
#  files against the tree. Refusals must leave nothing behind.
#    check.py TOOL WORKDIR SESSION_TOOL
#  SESSION_TOOL drives Git_Clone_Sessions, the whole conversation: each
#  exchange's request is the session's own, answered by upload-pack.
import os
import shutil
import subprocess
import sys

tool, work, session_tool = sys.argv[1], os.path.abspath(sys.argv[2]), sys.argv[3]
shutil.rmtree(work, ignore_errors=True)
os.makedirs(work)
env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", GIT_AUTHOR_NAME="CuBit Test",
           GIT_AUTHOR_EMAIL="test@cubit.invalid", GIT_COMMITTER_NAME="CuBit Test",
           GIT_COMMITTER_EMAIL="test@cubit.invalid", GIT_AUTHOR_DATE="2026-11-23T00:00:00Z",
           GIT_COMMITTER_DATE="2026-11-23T00:00:00Z", LC_ALL="C")
failures = checks = 0
URL = "https://git.example.invalid/test.git"


def check(cond, what):
    global failures, checks
    checks += 1
    if not cond:
        failures += 1
        print("FAIL:", what)


def git(*args, cwd=None, input=None, extra=None, ok=True):
    r = subprocess.run(["git", *args], cwd=cwd, input=input, env=dict(env, **(extra or {})), capture_output=True)
    if ok and r.returncode != 0:
        raise SystemExit("git %s: %s" % (" ".join(args), r.stderr.decode()))
    return r.stdout if ok else r


def pkt(s):
    b = s if isinstance(s, bytes) else s.encode()
    return b"%04x" % (len(b) + 4) + b


def answers(repo, extra_refs=b"", cut=0):
    """The advertisement, ls-refs and fetch answers for a clone of REPO."""
    v2 = {"GIT_PROTOCOL": "version=2"}
    adv = git("upload-pack", "--stateless-rpc", "--advertise-refs", repo, extra=v2)
    fmt = pkt("object-format=%s\n" % git("rev-parse", "--show-object-format", cwd=repo).decode().strip())
    ls = (pkt("command=ls-refs\n") + fmt + b"0001" + pkt("peel\n") + pkt("symrefs\n") + pkt("ref-prefix HEAD\n")
          + pkt("ref-prefix refs/heads/\n") + pkt("ref-prefix refs/tags/\n") + b"0000")
    refs = git("upload-pack", "--stateless-rpc", repo, input=ls, extra=v2)
    if extra_refs:
        refs = refs[:-4] + extra_refs + b"0000"
    shown = git("show-ref", "--head", cwd=repo, ok=False)
    tips = sorted({l.split()[0] for l in shown.stdout.decode().splitlines()})
    fr = (pkt("command=fetch\n") + fmt + b"0001" + pkt("ofs-delta\n") + pkt("no-progress\n")
          + b"".join(pkt("want %s\n" % t) for t in tips) + pkt("done\n") + b"0000")
    fetch = git("upload-pack", "--stateless-rpc", repo, input=fr, extra=v2) if tips else b"0000"
    if cut:
        fetch = fetch[:-cut]
    paths = []
    for n, d in (("adv", adv), ("refs", refs), ("fetch", fetch)):
        p = os.path.join(work, "answer-" + n)
        with open(p, "wb") as f:
            f.write(d)
        paths.append(p)
    return paths


def clone(repo, parent, name, objects=100000, max_bytes=1 << 30, chunk=4096, **kw):
    os.makedirs(parent, exist_ok=True)
    adv, refs, fetch = answers(repo, **kw)
    r = subprocess.run([tool, adv, refs, fetch, parent, name, URL, str(objects), str(max_bytes), str(chunk)],
                       capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else "CRASH " + r.stderr.strip()[-300:]


def judge(source, cloned, what, branch="main"):
    """The host's git on the clone."""
    r = git("fsck", "--strict", "--no-dangling", cwd=cloned, ok=False)
    check(r.returncode == 0, "%s: git fsck --strict: %s" % (what, r.stderr.decode()[:300]))
    check(git("status", "--porcelain", cwd=cloned) == b"", "%s: git status is clean" % what)
    check(git("rev-list", "--all", cwd=cloned) and
          sorted(git("rev-list", "--all", cwd=cloned).split()) == sorted(git("rev-list", "--all", cwd=source).split()),
          "%s: the same commits" % what)
    check(git("rev-parse", "HEAD", cwd=cloned) == git("rev-parse", "HEAD", cwd=source), "%s: HEAD" % what)
    # The files against HEAD's tree (links are files holding their target;
    # submodules empty directories).
    for line in git("ls-tree", "-r", "-t", "HEAD", cwd=source).decode().splitlines():
        meta, path = line.split("\t", 1)
        mode, kind, oid = meta.split()
        full = os.path.join(cloned, path)
        if kind == "blob":
            want = git("cat-file", "blob", oid, cwd=source)
            check(os.path.isfile(full) and open(full, "rb").read() == want, "%s: %s" % (what, path))
        elif kind == "tree":
            check(os.path.isdir(full), "%s: directory %s" % (what, path))
        elif kind == "commit":
            check(os.path.isdir(full) and not os.listdir(full), "%s: submodule %s" % (what, path))
    names = sorted(os.path.relpath(os.path.join(d, f), cloned) for d, ds, fs in os.walk(cloned)
                   for f in fs + ds if ".git" not in os.path.relpath(os.path.join(d, f), cloned).split(os.sep))
    want = sorted(l.split("\t", 1)[1] for l in git("ls-tree", "-r", "-t", "HEAD", cwd=source).decode().splitlines())
    check(names == want, "%s: nothing more in the working tree" % what)
    check(git("ls-files", "-s", cwd=cloned) == git("ls-files", "-s", cwd=source) or
          b"160000" in git("ls-files", "-s", cwd=cloned), "%s: the index matches" % what)
    if git("rev-parse", "--verify", "-q", "refs/remotes/origin/side", cwd=cloned, ok=False).returncode == 0:
        git("checkout", "-q", "side", cwd=cloned)
        check(git("status", "--porcelain", cwd=cloned) == b"", "%s: checkout side" % what)
        git("checkout", "-q", branch, cwd=cloned)
    check(git("config", "remote.origin.url", cwd=cloned).strip() == URL.encode(), "%s: origin's URL" % what)


def left(parent):
    return sorted(os.listdir(parent))


def empty_repo_for_session():
    d = os.path.join(work, "empty-session")
    if not os.path.isdir(d):
        git("init", "-q", "-b", "main", d)
    return d


# --- a repository with history, deltas, tags, links, submodules ----------------
src = os.path.join(work, "src")
git("init", "-q", "-b", "main", src)
text = "".join("line %d of the notes, with words to delta\n" % i for i in range(3000))
os.makedirs(os.path.join(src, "a/b/c/d"))
for i in range(5):
    with open(os.path.join(src, "notes.txt"), "w") as f:
        f.write(text[: 20000 + 9000 * i] + "edit %d\n" % i)
    with open(os.path.join(src, "a/b/c/d/deep.txt"), "w") as f:
        f.write("deep %d\n" % i)
    with open(os.path.join(src, "a/a.c"), "w") as f:
        f.write("int main(void) { return %d; }\n" % i)
    if i == 1:
        os.symlink("notes.txt", os.path.join(src, "link"))
        with open(os.path.join(src, "run"), "w") as f:
            f.write("#!/bin/sh\n")
        os.chmod(os.path.join(src, "run"), 0o755)
        os.makedirs(os.path.join(src, "a0"))
        open(os.path.join(src, "a0/empty"), "w").close()
    git("add", "-A", cwd=src)
    git("commit", "-q", "-m", "commit %d" % i, cwd=src)
    if i == 2:
        git("tag", "-a", "v1", "-m", "one", cwd=src)
        git("branch", "side", cwd=src)
# A submodule entry (a gitlink) without .gitmodules' clone: the index only.
git("update-index", "--add", "--cacheinfo", "160000,%s,mod" % git("rev-parse", "HEAD~1", cwd=src).decode().strip(), cwd=src)
git("commit", "-q", "-m", "a submodule", cwd=src)

out = os.path.join(work, "out")
r = clone(src, out, "work")
check(r.startswith("CLONED"), "clone: " + r)
if r.startswith("CLONED"):
    judge(src, os.path.join(out, "work"), "clone")
for chunk in (1, 7):
    r = clone(src, out, "work-%d" % chunk, chunk=chunk)
    check(r.startswith("CLONED"), "clone in %d-byte pieces: %s" % (chunk, r))
check(left(out) == ["work", "work-1", "work-7"], "only the clones are left: %s" % left(out))
print("git-clone: clone PASS" if failures == 0 else "git-clone: clone FAILED")

# --- a default branch named feature/x, and a detached HEAD -------------------------
fx = os.path.join(work, "fx")
git("clone", "-q", "--no-local", src, fx)
git("checkout", "-q", "-b", "feature/x", cwd=fx)
with open(os.path.join(fx, "x.txt"), "w") as f:
    f.write("x\n")
git("add", "x.txt", cwd=fx)
git("commit", "-q", "-m", "x", cwd=fx)
r = clone(fx, out, "fx")
check(r.startswith("CLONED"), "feature/x: " + r)
if r.startswith("CLONED"):
    judge(fx, os.path.join(out, "fx"), "feature/x", branch="feature/x")
    check(git("symbolic-ref", "HEAD", cwd=os.path.join(out, "fx")).strip() == b"refs/heads/feature/x",
          "HEAD is refs/heads/feature/x")
det = os.path.join(work, "det")
git("clone", "-q", "--no-local", src, det)
git("checkout", "-q", "--detach", "HEAD~2", cwd=det)
r = clone(det, out, "det")
check(r.startswith("CLONED"), "detached: " + r)
if r.startswith("CLONED"):
    check(git("rev-parse", "HEAD", cwd=os.path.join(out, "det")) == git("rev-parse", "HEAD", cwd=det), "detached HEAD")
    check(git("fsck", "--strict", "--no-dangling", cwd=os.path.join(out, "det"), ok=False).returncode == 0, "detached fsck")
print("git-clone: branches PASS" if failures == 0 else "git-clone: branches FAILED")

# --- a SHA-256 repository: format version 1, extensions.objectformat, and
#     32-byte names in the pack index and the index -------------------------
src256 = os.path.join(work, "src256")
git("init", "-q", "-b", "main", "--object-format=sha256", src256)
for i in range(4):
    os.makedirs(os.path.join(src256, "a/b"), exist_ok=True)
    with open(os.path.join(src256, "notes.txt"), "w") as f:
        f.write(text[: 15000 + 7000 * i] + "edit %d\n" % i)
    with open(os.path.join(src256, "a/b/deep.txt"), "w") as f:
        f.write("deep %d\n" % i)
    if i == 1:
        os.symlink("notes.txt", os.path.join(src256, "link"))
        with open(os.path.join(src256, "run"), "w") as f:
            f.write("#!/bin/sh\n")
        os.chmod(os.path.join(src256, "run"), 0o755)
    git("add", "-A", cwd=src256)
    git("commit", "-q", "-m", "commit %d" % i, cwd=src256)
    if i == 2:
        git("tag", "-a", "v1", "-m", "one", cwd=src256)
        git("branch", "side", cwd=src256)
for chunk in (4096, 7):
    r = clone(src256, out, "work256-%d" % chunk, chunk=chunk)
    check(r.startswith("CLONED"), "sha256 clone in %d-byte pieces: %s" % (chunk, r))
    if r.startswith("CLONED"):
        cloned = os.path.join(out, "work256-%d" % chunk)
        check(git("rev-parse", "--show-object-format", cwd=cloned).strip() == b"sha256", "the clone is sha256")
        judge(src256, cloned, "sha256 clone")
print("git-clone: sha256 PASS" if failures == 0 else "git-clone: sha256 FAILED")

# --- the whole conversation (Git_Clone_Sessions): requests made by the
#     session, answered by upload-pack -------------------------------------
def session(repo, parent, name, refs=1000, objects=100000, max_bytes=1 << 30, chunk=4096, protocol="2"):
    os.makedirs(parent, exist_ok=True)
    r = subprocess.run([session_tool, repo, os.path.join(work, "session-io"), parent, name, URL, str(refs),
                        str(objects), str(max_bytes), str(chunk), protocol], capture_output=True, text=True,
                       env=env)
    return r.stdout.strip() if r.returncode == 0 else "CRASH " + r.stderr.strip()[-300:]


sess = os.path.join(work, "sessions")
for repo, name, fmt in ((src, "s1", "sha1"), (src256, "s256", "sha256"), (fx, "sfx", "sha1")):
    for chunk in (4096, 5):
        r = session(repo, sess, "%s-%d" % (name, chunk), chunk=chunk)
        check(r.startswith("CLONED") and r.endswith(" " + fmt), "session %s in %d: %s" % (name, chunk, r))
        if r.startswith("CLONED"):
            judge(repo, os.path.join(sess, "%s-%d" % (name, chunk)), "session " + name,
                  branch="feature/x" if repo == fx else "main")
check(session(src, sess, "v0", protocol="0").startswith("Not_Smart_Http"), "a protocol v0 server")
check(session(src, sess, "few-refs", refs=2).startswith("Bad_Refs Too_Many_Refs"), "the ref ceiling")
check(session(src, sess, "few-objects", objects=10).startswith("Too_Many_Objects"), "the object ceiling")
check(session(empty_repo_for_session(), sess, "nothing").startswith("No_Refs"), "an empty repository")
check(session(src, sess, "s1-4096").startswith("Target_Exists"), "a target that exists")
check(sorted(os.listdir(sess)) == sorted(["%s-%d" % (n, c) for n in ("s1", "s256", "sfx") for c in (4096, 5)]),
      "only the clones are left: %s" % sorted(os.listdir(sess)))
print("git-clone: sessions PASS" if failures == 0 else "git-clone: sessions FAILED")

# --- refusals leave nothing ---------------------------------------------------------
ref_out = os.path.join(work, "refusals")
os.makedirs(ref_out)
before = left(ref_out)
os.makedirs(os.path.join(ref_out, "taken"))
check(clone(src, ref_out, "taken").startswith("Target_Exists"), "a target that exists")
os.makedirs(os.path.join(ref_out, ".crashed.cloning"))
check(clone(src, ref_out, "crashed").startswith("Target_Exists"), "a crashed clone's temporary directory")
for bad in ("../x", ".git", "a/b", ".", ".."):
    check(clone(src, ref_out, bad).startswith("Bad_Directory"), "directory name %r" % bad)
for bad_url in ("https://x/a b", "https://x/\"q", "https://x/\\", "https://x/\n[core]"):
    os.makedirs(ref_out, exist_ok=True)
    adv, refs, fetch = answers(src)
    r = subprocess.run([tool, adv, refs, fetch, ref_out, "u", bad_url, "100000", str(1 << 30), "4096"],
                       capture_output=True, text=True).stdout.strip()
    check(r.startswith("Bad_Url"), "URL %r: %s" % (bad_url, r))
check(clone(src, ref_out, "few", objects=10).startswith("Too_Many_Objects"), "the object ceiling")
check(clone(src, ref_out, "small", max_bytes=50000).startswith("Too_Large"), "the byte ceiling")
r = clone(src, ref_out, "cut", cut=30)
check(r.startswith("FETCH") or r.startswith("Bad_Pack"), "a cut fetch: " + r)
missing = pkt("0123456789abcdef0123456789abcdef01234567 refs/heads/ghost\n")
check(clone(src, ref_out, "ghost", extra_refs=missing).startswith("Missing_Object"), "a ref to a missing object")
empty = os.path.join(work, "empty")
git("init", "-q", "-b", "main", empty)
check(clone(empty, ref_out, "empty").startswith("No_Refs"), "an empty repository")


def evil_tree(entries):
    """A repository whose HEAD tree has the given raw entries."""
    d = os.path.join(work, "evil-%d" % len(os.listdir(work)))
    git("init", "-q", "-b", "main", d)
    blob = git("hash-object", "-w", "--stdin", cwd=d, input=b"evil\n").decode().strip()
    raw = b"".join(mode + b" " + name + b"\0" + bytes.fromhex(blob) for mode, name in entries)
    tree = git("hash-object", "-t", "tree", "-w", "--literally", "--stdin", cwd=d, input=raw).decode().strip()
    commit = git("commit-tree", tree, "-m", "evil", cwd=d).decode().strip()
    git("update-ref", "refs/heads/main", commit, cwd=d)
    return d


for entries, what in (([(b"100644", b".git")], ".git"), ([(b"100644", b"..")], ".."),
                      ([(b"100644", b".GIT")], ".GIT"), ([(b"100644", b"a/../../x")], "a slash"),
                      ([(b"100644", b"b"), (b"100644", b"a")], "out of order"),
                      ([(b"100644", b"a"), (b"100644", b"a")], "a duplicate"),
                      ([(b"100664", b"a")], "mode 100664")):
    r = clone(evil_tree(entries), ref_out, "evil")
    check(r.startswith("Bad_Object"), "a tree with %s: %s" % (what, r))
os.chmod(ref_out, 0o555)
check(clone(src, ref_out, "denied").startswith("Write_Failed"), "an unwritable parent")
os.chmod(ref_out, 0o755)
check(left(ref_out) == [".crashed.cloning", "taken"], "refusals leave nothing: %s" % left(ref_out))
print("git-clone: refusals PASS" if failures == 0 else "git-clone: refusals FAILED")

print("git-clone: %d checks, %d failures" % (checks, failures))
sys.exit(1 if failures else 0)
