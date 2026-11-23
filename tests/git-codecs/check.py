#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: tests of the git codecs (userspace/lib/git) through
#  build/git_tool, against the host's git. Everything is made here at test
#  time: repositories built by the host's git (hermetic: no user or system
#  config), protocol v2 answers from `git upload-pack --stateless-rpc`, and
#  adversarial bytes written below. No stored blobs.
#    check.py TOOL WORKDIR [--quick]
#  --quick (for the -gnata build, whose contract checks are slow) skips
#  byte-at-a-time packs and keeps 20 of the 300 mutated packs.
import hashlib
import os
import random
import shutil
import struct
import subprocess
import sys
import zlib

tool, work = sys.argv[1], sys.argv[2]
quick = "--quick" in sys.argv
shutil.rmtree(work, ignore_errors=True)
os.makedirs(work)
rng = random.Random(20261123)
env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", GIT_AUTHOR_NAME="CuBit Test",
           GIT_AUTHOR_EMAIL="test@cubit.invalid", GIT_COMMITTER_NAME="CuBit Test",
           GIT_COMMITTER_EMAIL="test@cubit.invalid", GIT_AUTHOR_DATE="2026-11-23T00:00:00Z",
           GIT_COMMITTER_DATE="2026-11-23T00:00:00Z", LC_ALL="C")
failures = checks = 0
serial = 0


def check(cond, what):
    global failures, checks
    checks += 1
    if not cond:
        failures += 1
        print("FAIL:", what)


def git(*args, cwd=None, input=None, extra=None):
    e = dict(env, **(extra or {}))
    return subprocess.run(["git", *args], cwd=cwd, input=input, env=e, check=True, capture_output=True).stdout


def file_of(data):
    global serial
    serial += 1
    p = os.path.join(work, "in-%d" % serial)
    with open(p, "wb") as f:
        f.write(data)
    return p


def run(*args, fmt="sha1"):
    r = subprocess.run([tool, *map(str, args)], capture_output=True, text=True,
                       env=dict(os.environ, GIT_FORMAT=fmt))
    if r.returncode != 0:
        return "CRASH " + r.stderr.strip().splitlines()[-1] if r.stderr.strip() else "CRASH"
    return r.stdout.strip()


def pkt(s):
    b = s if isinstance(s, bytes) else s.encode()
    return b"%04x" % (len(b) + 4) + b


# --- a repository with history, deltas, tags, odd files ---------------------
repo = os.path.join(work, "repo")
git("init", "-q", "-b", "main", repo)
words = b"alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu".split()
text = b"\n".join(b" ".join(rng.choice(words) for _ in range(12)) for _ in range(400)) + b"\n"
os.makedirs(os.path.join(repo, "src/deep/er"))
for i in range(6):
    with open(os.path.join(repo, "src/notes.txt"), "wb") as f:
        f.write(text[: 2000 + i * 3000] + b"edit %d\n" % i)
    with open(os.path.join(repo, "src/deep/er/data.bin"), "wb") as f:
        f.write(bytes(rng.getrandbits(8) for _ in range(5000)) * 2 + bytes([i]))
    with open(os.path.join(repo, "README"), "wb") as f:
        f.write(b"CuBit git test %d\n" % i)
    if i == 2:
        with open(os.path.join(repo, "run.sh"), "wb") as f:
            f.write(b"#!/bin/sh\necho hi\n")
        os.chmod(os.path.join(repo, "run.sh"), 0o755)
        os.symlink("README", os.path.join(repo, "link"))
        open(os.path.join(repo, "empty"), "wb").close()
        with open(os.path.join(repo, "big"), "wb") as f:
            f.write(bytes(rng.getrandbits(8) for _ in range(100000)))
    git("add", "-A", cwd=repo)
    git("commit", "-q", "-m", "commit %d" % i, cwd=repo)
    if i == 3:
        git("tag", "-a", "v1", "-m", "release one", cwd=repo)
        git("branch", "side", cwd=repo)
git("tag", "light", cwd=repo)

# --- 1. the advertisement --------------------------------------------------------
adv = git("upload-pack", "--stateless-rpc", "--advertise-refs", repo, extra={"GIT_PROTOCOL": "version=2"})
for chunk in (1, 3, 4096):
    check(run("advert", file_of(adv), chunk) == "ENDED agent=TRUE object-format=TRUE format=sha1", "advertisement in %d" % chunk)
svc = pkt("# service=git-upload-pack\n") + b"0000" + adv
check(run("advert", file_of(svc), 5) == "ENDED agent=TRUE object-format=TRUE format=sha1", "advertisement after # service")
v0 = git("upload-pack", "--stateless-rpc", "--advertise-refs", repo)
check(run("advert", file_of(v0), 64) == "REFUSED Not_Version_2", "a v0 advertisement")
caps = lambda *c: pkt("version 2\n") + b"".join(pkt(x + "\n") for x in c) + b"0000"
check(run("advert", file_of(caps("ls-refs", "fetch=shallow")), 9) == "ENDED agent=FALSE object-format=FALSE format=sha1",
      "minimal capabilities")
check(run("advert", file_of(caps("fetch")), 9) == "REFUSED No_Ls_Refs",
      "no ls-refs")
check(run("advert", file_of(caps("ls-refs")), 9) == "REFUSED No_Fetch", "no fetch")
check(run("advert", file_of(caps("ls-refs", "fetch", "object-format=sha256")), 9)
      == "ENDED agent=FALSE object-format=TRUE format=sha256", "a sha256 repository")
check(run("advert", file_of(caps("ls-refs", "fetch", "object-format=sha512")), 9) == "REFUSED Unsupported_Object_Format",
      "an unknown object format")
check(run("advert", file_of(caps("ls-refs", "fetch", "bad key")), 9) == "REFUSED Bad_Capability", "a space in a key")
check(run("advert", file_of(caps("ls-refs", "fetch", "agent=")), 9) == "REFUSED Bad_Capability", "an empty value")
check(run("advert", file_of(caps("ls-refs", "fetch", "agent=a\x01b")), 9) == "REFUSED Bad_Capability",
      "a control byte in a value")
check(run("advert", file_of(pkt("version 2\n") + pkt("ls-refs\n") + b"0001"), 9) == "REFUSED Unexpected_Packet",
      "a delimiter in the advertisement")
check(run("advert", file_of(adv[:-2]), 9) == "REFUSED PACKET Truncated", "a cut advertisement")
# pkt-line framing.
for bad, why in ((b"0003", "Reserved_Length"), (b"zzzz", "Bad_Length"), (b"fff1" + b"x" * 65517, "Too_Long"),
                 (b"00", "Truncated"), (b"0009abc", "Truncated")):
    check(run("advert", file_of(bad), 2) == "REFUSED PACKET " + why, "framing %r" % bad[:8])
check(run("advert", file_of(b"fff0" + b"x" * 65516), 4096) == "REFUSED Not_Version_2", "the largest packet is framed")
check(run("advert", file_of(b"0004" + caps("ls-refs", "fetch")), 3) == "REFUSED Not_Version_2",
      "an empty packet is data")
print("git-codecs: advertisement PASS" if failures == 0 else "git-codecs: advertisement FAILED")

# --- 2. ls-refs --------------------------------------------------------------------
lsreq = (pkt("command=ls-refs\n") + pkt("object-format=sha1\n") + b"0001" + pkt("peel\n") + pkt("symrefs\n")
         + pkt("ref-prefix HEAD\n") + pkt("ref-prefix refs/heads/\n") + pkt("ref-prefix refs/tags/\n") + b"0000")
refs = git("upload-pack", "--stateless-rpc", repo, input=lsreq, extra={"GIT_PROTOCOL": "version=2"})
want = {}
for line in git("show-ref", "--head", "-d", cwd=repo).decode().splitlines():
    oid, name = line.split(" ", 1)
    want[name] = oid
for chunk in (1, 7, 1 << 16):
    out = run("refs", file_of(refs), chunk).splitlines()
    check(out[-1] == "ENDED %d" % len([k for k in want if not k.endswith("^{}")]),
          "ls-refs count in %d: %s" % (chunk, out[-1]))
    got = {}
    for l in out[:-1]:
        f = l.split()
        got[f[2]] = f[1]
        if "peeled" in f:
            check(want.get(f[2] + "^{}") == f[f.index("peeled") + 1], "peeled " + f[2])
        if f[2] == "HEAD":
            check(f[3:5] == ["symref", "refs/heads/main"], "HEAD's symref")
    check(all(want[k] == v for k, v in got.items()) and set(got) == {k for k in want if not k.endswith("^{}")},
          "ls-refs names and ids in %d" % chunk)
H = "0123456789abcdef0123456789abcdef01234567"
def refs_of(*lines):
    return file_of(b"".join(pkt(l + "\n") for l in lines) + b"0000")
for line, why in ((H + " refs/heads/../x", "Bad_Ref_Name"), (H + " refs/heads/a.lock", "Bad_Ref_Name"),
                  (H + " refs/heads/a.lock/b", "Bad_Ref_Name"), (H + " refs/heads/.hidden", "Bad_Ref_Name"),
                  (H + " refs/heads//x", "Bad_Ref_Name"), (H + " refs/heads/x/", "Bad_Ref_Name"),
                  (H + " refs/heads/x.", "Bad_Ref_Name"), (H + " refs/heads/a@{b", "Bad_Ref_Name"),
                  (H + " refs/heads/a b", "Bad_Attribute"), (H + " refs/heads/a\\b", "Bad_Ref_Name"),
                  (H + " refs/heads/a~1", "Bad_Ref_Name"), (H + " refs/heads/a:b", "Bad_Ref_Name"),
                  (H + " HEAD2", "Bad_Ref_Name"), (H + " /etc/passwd", "Bad_Ref_Name"), (H + " refs/", "Bad_Ref_Name"),
                  (H.upper() + " refs/heads/main", "Bad_Object_Id"), (H[:39] + "g refs/heads/main", "Bad_Object_Id"),
                  (H + "  refs/heads/main", "Bad_Ref_Name"), (H, "Bad_Ref_Line"),
                  (H + " HEAD symref-target:../../x", "Bad_Ref_Name"),
                  (H + " HEAD symref-target:refs/heads/a symref-target:refs/heads/b", "Bad_Attribute"),
                  (H + " refs/tags/t peeled:" + H.upper(), "Bad_Object_Id"),
                  (H + " refs/tags/t peeled:" + H + " peeled:" + H, "Bad_Attribute"),
                  (H + " refs/tags/t unborn", "Bad_Attribute"), (H + " refs/heads/" + "x" * 1100, "Bad_Ref_Name")):
    check(run("refs", refs_of(line), 5).splitlines()[-1] == "REFUSED " + why, "ls-refs line %r" % line[:60])
check(run("refs", refs_of(H + " refs/heads/ok"), 5).splitlines()[0] == "REF %s refs/heads/ok" % H, "a plain ref")
print("git-codecs: ls-refs PASS" if failures == 0 else "git-codecs: ls-refs FAILED")

# --- 3. the fetch response and its pack -------------------------------------------
tips = sorted(set(want.values()))
fetchreq = (pkt("command=fetch\n") + pkt("object-format=sha1\n") + b"0001" + pkt("ofs-delta\n") + pkt("no-progress\n")
            + b"".join(pkt("want %s\n" % t) for t in tips) + pkt("done\n") + b"0000")
resp = git("upload-pack", "--stateless-rpc", repo, input=fetchreq, extra={"GIT_PROTOCOL": "version=2"})
packs = {}
for chunk in ((13, 1 << 20) if quick else (1, 13, 1 << 20)):
    out_pack = os.path.join(work, "fetched-%d.pack" % chunk)
    r = run("fetch", file_of(resp), chunk, out_pack)
    check(r.startswith("ENDED "), "fetch response in %d: %s" % (chunk, r))
    packs[chunk] = open(out_pack, "rb").read()
check(len(set(packs.values())) == 1, "the same pack in any chunking")
fetched = packs[13]
# With progress: channel 2 is passed over.
fetchreq_p = fetchreq.replace(pkt("no-progress\n"), b"")
resp_p = git("upload-pack", "--stateless-rpc", repo, input=fetchreq_p, extra={"GIT_PROTOCOL": "version=2"})
r = run("fetch", file_of(resp_p), 100, os.path.join(work, "p.pack"))
check(r.startswith("ENDED "), "fetch with progress: " + r)
for body, want_r in ((pkt("acknowledgments\n") + pkt("NAK\n") + b"0000", "REFUSED Unexpected_Section acknowledgments"),
                     (pkt("shallow-info\n") + b"0000", "REFUSED Unexpected_Section shallow-info"),
                     (pkt("packfile\n") + pkt(b"\x03no such object\n"), "REFUSED Remote_Error no such object"),
                     (pkt("packfile\n") + pkt(b"\x04x"), "REFUSED Bad_Sideband"),
                     (pkt("packfile\n") + b"0004", "REFUSED Bad_Sideband"),
                     (pkt("packfile\n") + b"0001", "REFUSED Unexpected_Packet"),
                     (pkt("packfile\n") + b"0000" + pkt("x"), "ENDED 0")):
    check(run("fetch", file_of(body), 3, os.path.join(work, "x.pack")) == want_r, "fetch response %r" % body[:30])


def verify_pack(pack_bytes, name, fmt="sha1"):
    """git's view: (id, type, size, offset, crc) per object, and the checksum."""
    d = os.path.join(work, "vp-" + name)
    os.makedirs(d, exist_ok=True)
    p = os.path.join(d, "p.pack")
    with open(p, "wb") as f:
        f.write(pack_bytes)
    git("index-pack", "--object-format=" + fmt, p)
    rows = {}
    # verify-pack takes the object format of the repository it runs in.
    for line in git("verify-pack", "-v", os.path.join(d, "p.idx"),
                    cwd=repo256 if fmt == "sha256" else None).decode().splitlines():
        f = line.split()
        if len(f) >= 5 and len(f[0]) in (40, 64):
            rows[f[0]] = [f[1], f[2], f[4]]
    for line in git("show-index", "--object-format=" + fmt,
                    input=open(os.path.join(d, "p.idx"), "rb").read()).decode().splitlines():
        off, oid, crc = line.split()
        rows[oid].append(crc.strip("()"))
    return rows


resolved = {}


def our_pack(pack_bytes, chunk=4096, out_chunk=1000, fmt="sha1"):
    out = run("pack", file_of(pack_bytes), chunk, out_chunk, fmt=fmt).splitlines()
    if not out or not out[-1].startswith("PACK "):
        return None, out[-1] if out else "nothing"
    rows = {}
    for l in out[:-1]:
        oid, kind, whole, size, off, crc = l.split()
        rows[oid] = [kind, size, off, crc]
        resolved[oid] = (kind, whole)
    return rows, out[-1].split()[1]


def compare(pack_bytes, name, chunks=((4096, 1000),), fmt="sha1"):
    theirs = verify_pack(pack_bytes, name, fmt)
    width = 32 if fmt == "sha256" else 20
    for chunk, out_chunk in chunks:
        ours, checksum = our_pack(pack_bytes, chunk, out_chunk, fmt)
        check(ours == theirs, "pack %s in %d/%d: %s" % (name, chunk, out_chunk,
                                                         checksum if ours is None else "%d vs %d objects" % (len(ours), len(theirs))))
        if ours is not None:
            check(checksum == pack_bytes[-width:].hex(), "pack %s checksum" % name)


batch = git("cat-file", "--batch-check", "--batch-all-objects", cwd=repo).decode().split("\n")
truth = {l.split()[0]: (l.split()[1], l.split()[2]) for l in batch if l}
compare(fetched, "fetched", ((7, 13), (4096, 1000)) if quick else ((1, 1), (7, 13), (4096, 1000), (1 << 20, 1 << 16)))
objs = git("rev-list", "--objects", "--all", cwd=repo)
ofs = git("pack-objects", "--stdout", "--delta-base-offset", "--window=50", "--depth=50", cwd=repo, input=objs)
ref = git("pack-objects", "--stdout", "--window=50", "--depth=50", cwd=repo, input=objs)
compare(ofs, "ofs-deltas", ((4096, 1000),) if quick else ((1, 7), (4096, 1000)))
compare(ref, "ref-deltas", ((4096, 1000),) if quick else ((3, 5), (4096, 1000)))
check(all(truth.get(k) == v for k, v in resolved.items()) and len(resolved) == len(truth),
      "every object's type and size, resolved, as git cat-file says (%d objects)" % len(truth))
kinds = lambda b: {line.split()[1] for line in git("verify-pack", "-v", os.path.join(work, "vp-" + b, "p.idx")).decode().splitlines() if len(line.split()) > 6}
check(len(kinds("ofs-deltas")) > 0, "the OFS pack has deltas")
check(len(kinds("ref-deltas")) > 0, "the REF pack has deltas")
empty = git("pack-objects", "--stdout", cwd=repo, input=b"")
check(our_pack(empty) == ({}, empty[-20:].hex()), "an empty pack")
print("git-codecs: fetch and packs PASS" if failures == 0 else "git-codecs: fetch and packs FAILED")


# --- 4. adversarial packs (trailers recomputed so the framing is what fails) --------
def seal(body):
    return body + hashlib.sha1(body).digest()


def entry(kind, size, payload, extra=b""):
    c = (kind << 4) | (size & 15)
    size >>= 4
    head = bytearray()
    while size:
        head.append(c | 0x80)
        c = size & 0x7f
        size >>= 7
    head.append(c)
    return bytes(head) + extra + payload


def ofs_bytes(n):
    out = [n & 0x7f]
    n >>= 7
    while n:
        n -= 1
        out.insert(0, 0x80 | (n & 0x7f))
        n >>= 7
    return bytes(out)


blob = b"hello pack\n"
zb = zlib.compress(blob)
hdr = lambda n, v=2: b"PACK" + struct.pack(">II", v, n)
good = seal(hdr(1) + entry(3, len(blob), zb))
check(our_pack(good)[0] is not None, "a one-blob pack")
for body, why, what in ((seal(b"PACX" + struct.pack(">II", 2, 1) + entry(3, len(blob), zb)), "Bad_Signature", "signature"),
                        (seal(hdr(1, 4) + entry(3, len(blob), zb)), "Bad_Version", "version 4"),
                        (seal(hdr(1) + entry(5, len(blob), zb)), "Bad_Object_Type", "type 5"),
                        (seal(hdr(1) + entry(0, len(blob), zb)), "Bad_Object_Type", "type 0"),
                        (seal(hdr(1) + entry(3, len(blob) + 1, zb)), "Size_Mismatch", "declared one byte long"),
                        (seal(hdr(1) + entry(3, len(blob) - 1, zb)), "Size_Mismatch", "declared one byte short"),
                        (seal(hdr(1) + entry(3, len(blob), zb[:-6] + b"\0" * 6)), "Bad_Compression", "a bad stream"),
                        (seal(hdr(2) + entry(3, len(blob), zb)), "", "fewer entries than counted (the trailer read as one)"),
                        (hdr(2) + entry(3, len(blob), zb), "Truncated", "fewer entries than counted, no trailer"),
                        (seal(hdr(1) + entry(6, 3, zlib.compress(b"\x0b\x03\x90"), ofs_bytes(100))), "Bad_Delta_Offset",
                         "an OFS base before the pack"),
                        (seal(hdr(1) + entry(6, 3, zlib.compress(b"\x0b\x03\x90"), ofs_bytes(0))), "Bad_Delta_Offset",
                         "an OFS base at the entry itself"),
                        (seal(hdr(1) + bytes([0xb0] + [0xff] * 9 + [0x01]) + zb), "Bad_Size", "a size past 2 ** 62"),
                        (hdr(1) + entry(3, len(blob), zb) + b"\0" * 20, "Bad_Checksum", "a wrong trailer"),
                        (good[:-1], "Truncated", "a cut trailer"),
                        (good + b"x", "EXTRA_DATA", "a byte after the trailer"),
                        (seal(b"PACK" + struct.pack(">II", 2, 0xFFFFFFFF)), "Too_Many_Objects", "four billion objects")):
    r = run("pack", file_of(body), 4096, 1000)
    check(r == "REFUSED " + why or r.startswith("REFUSED " + why + (" " if why else "")), "pack %s: %s" % (what, r))
# An OFS delta onto the blob, and one onto a non-entry offset.
d = bytes([len(blob), 5, 0x90, 5])
two = hdr(2) + entry(3, len(blob), zb)
at = len(two)
okd = seal(two + entry(6, len(d), zlib.compress(d), ofs_bytes(at - 12)))
r = our_pack(okd)[0]
check(r is not None and any(resolved[k] == ("blob", "5") for k in r), "an OFS delta onto the blob")
check(run("pack", file_of(seal(two + entry(6, len(d), zlib.compress(d), ofs_bytes(at - 13)))), 4096, 100)
      == "REFUSED BASE_NOT_AN_ENTRY", "an OFS base inside an entry (left to the indexer)")
for _ in range(20 if quick else 300):
    m = bytearray(ofs)
    for _ in range(rng.randint(1, 3)):
        m[rng.randrange(len(m))] ^= 1 << rng.randrange(8)
    r = run("pack", file_of(bytes(m)), rng.choice((1, 100, 4096)), rng.choice((1, 999)))
    check(r.startswith("REFUSED"), "a mutated pack is refused: " + r[:60])
print("git-codecs: adversarial packs PASS" if failures == 0 else "git-codecs: adversarial packs FAILED")


# --- 5. deltas -----------------------------------------------------------------------
def varint(n):
    out = bytearray()
    while True:
        b = n & 0x7f
        n >>= 7
        out.append(b | (0x80 if n else 0))
        if not n:
            return bytes(out)


def copy(off, size):
    op, args = 0x80, bytearray()
    for k in range(4):
        if (off >> (8 * k)) & 255:
            op |= 1 << k
            args.append((off >> (8 * k)) & 255)
    sz = 0 if size == 0x10000 else size
    for k in range(3):
        if (sz >> (8 * k)) & 255:
            op |= 1 << (4 + k)
            args.append((sz >> (8 * k)) & 255)
    return bytes([op]) + bytes(args)


def apply(base, delta):
    out = os.path.join(work, "delta.out")
    r = run("delta", file_of(base), file_of(delta), out)
    return r, (open(out, "rb").read() if r.startswith("FINISHED") else None)


base = bytes(rng.getrandbits(8) for _ in range(70000))
target = base[100:200] + b"NEW" + base[0:0x10000] + base[69990:]
dl = varint(len(base)) + varint(len(target)) + copy(100, 100) + bytes([3]) + b"NEW" + copy(0, 0x10000) + copy(69990, 10)
check(apply(base, dl) == ("FINISHED %d" % len(target), target), "a delta with a 65536-byte copy")
for delta_bytes, why, what in ((varint(len(base) + 1) + varint(1) + bytes([1]) + b"x", "Base_Size_Mismatch", "base size"),
                               (varint(len(base)) + varint(10) + copy(69995, 10), "Copy_Outside_Base", "copy past the base"),
                               (varint(len(base)) + varint(3) + copy(0, 4), "Result_Overrun", "copy past the result"),
                               (varint(len(base)) + varint(3) + bytes([4]) + b"abcd", "Result_Overrun", "insert past the result"),
                               (varint(len(base)) + varint(3) + bytes([0]), "Reserved_Instruction", "instruction 0"),
                               (varint(len(base)) + varint(3) + bytes([2]) + b"ab", "Truncated", "result short"),
                               (varint(len(base)) + varint(3) + bytes([3]) + b"abc" + bytes([1]) + b"d", "Extra_Data",
                                "an instruction after the result"),
                               (varint(len(base)) + varint(3) + bytes([3]) + b"ab", "Truncated", "insert cut"),
                               (varint(len(base)) + varint(2 ** 40), "Result_Too_Large", "a result past the ceiling"),
                               (varint(len(base)) + bytes([0xff] * 10) + b"\x01", "Bad_Size", "a size past 2 ** 56"),
                               (varint(len(base)), "Truncated", "no result size")):
    check(apply(base, delta_bytes)[0] == "REFUSED " + why, "delta " + what)
# git's own deltas: every delta in the OFS pack resolves above (compare); and
# deltas made by git diff-delta through pack-objects are covered there.
print("git-codecs: deltas PASS" if failures == 0 else "git-codecs: deltas FAILED")

# --- 6. trees ------------------------------------------------------------------------------
for t in git("rev-list", "--objects", "--all", cwd=repo).decode().splitlines():
    oid = t.split()[0]
    if git("cat-file", "-t", oid, cwd=repo).strip() != b"tree":
        continue
    raw = git("cat-file", "tree", oid, cwd=repo)
    ours = run("tree", file_of(raw)).splitlines()
    mode_names = {"100644": "REGULAR_FILE", "100755": "EXECUTABLE_FILE", "120000": "SYMBOLIC_LINK", "040000": "DIRECTORY",
                  "160000": "GITLINK"}
    theirs = ["%s %s %s" % (mode_names[l.split()[0]], l.split("\t")[1], l.split()[2])
              for l in git("ls-tree", oid, cwd=repo).decode().splitlines()]
    check(ours == theirs + ["ENDED"], "tree %s" % oid)
I = bytes(20)
te = lambda mode, name: mode + b" " + name + b"\0" + I
for raw, why in ((te(b"100644", b".."), "Unsafe_Name"), (te(b"100644", b"."), "Unsafe_Name"),
                 (te(b"40000", b".git"), "Unsafe_Name"), (te(b"40000", b".GiT"), "Unsafe_Name"),
                 (te(b"100644", b"a/b"), "Unsafe_Name"), (te(b"100644", b"a\\b"), "Unsafe_Name"),
                 (te(b"100644", b"a\x01"), "Unsafe_Name"), (te(b"100644", b""), "Unsafe_Name"),
                 (te(b"100664", b"a"), "Bad_Mode"), (te(b"040000", b"a"), "Bad_Mode"), (te(b"1006440", b"a"), "Bad_Mode"),
                 (te(b"100644", b"b") + te(b"100644", b"a"), "Out_Of_Order"),
                 (te(b"100644", b"a") + te(b"100644", b"a"), "Out_Of_Order"),
                 (te(b"40000", b"a") + te(b"100644", b"a"), "Out_Of_Order"),
                 (te(b"100644", b"a")[:-1], "Truncated_Entry"), (b"100644 a", "Truncated_Entry"),
                 (te(b"100644", b"x" * 256), "Unsafe_Name")):
    check(run("tree", file_of(raw)).splitlines()[-1] == "REFUSED " + why, "tree %r" % raw[:30])
check(run("tree", file_of(te(b"100644", b"a.c") + te(b"40000", b"a") + te(b"100644", b"a0"))).endswith("ENDED"),
      "git's order: a.c, then directory a (as a/), then a0")
print("git-codecs: trees PASS" if failures == 0 else "git-codecs: trees FAILED")

# --- 7. a SHA-256 repository: the same answers with 64-digit names, a pack
#     whose names and trailer are SHA-256, and trees of 32-byte names.
repo256 = os.path.join(work, "repo256")
git("init", "-q", "-b", "main", "--object-format=sha256", repo256)
for i in range(4):
    os.makedirs(os.path.join(repo256, "d/e"), exist_ok=True)
    with open(os.path.join(repo256, "d/e/notes.txt"), "wb") as f:
        f.write(text[: 3000 + i * 2000] + b"edit %d\n" % i)
    with open(os.path.join(repo256, "README"), "wb") as f:
        f.write(b"sha256 %d\n" % i)
    git("add", "-A", cwd=repo256)
    git("commit", "-q", "-m", "commit %d" % i, cwd=repo256)
    if i == 1:
        git("tag", "-a", "v1", "-m", "one", cwd=repo256)
adv256 = git("upload-pack", "--stateless-rpc", "--advertise-refs", repo256, extra={"GIT_PROTOCOL": "version=2"})
check(run("advert", file_of(adv256), 7) == "ENDED agent=TRUE object-format=TRUE format=sha256", "a sha256 advertisement")
lsreq256 = lsreq.replace(pkt("object-format=sha1\n"), pkt("object-format=sha256\n"))
refs256 = git("upload-pack", "--stateless-rpc", repo256, input=lsreq256, extra={"GIT_PROTOCOL": "version=2"})
want256 = dict(reversed(l.split(" ", 1)) for l in git("show-ref", "--head", "-d", cwd=repo256).decode().splitlines())
out = run("refs", file_of(refs256), 7, fmt="sha256").splitlines()
got256 = {l.split()[2]: l.split()[1] for l in out[:-1]}
check(out[-1] == "ENDED %d" % len(got256) and all(want256[k] == v and len(v) == 64 for k, v in got256.items())
      and set(got256) == {k for k in want256 if not k.endswith("^{}")}, "sha256 ls-refs: %s" % out[-1])
check(run("refs", file_of(refs), 7, fmt="sha256").splitlines()[-1] in ("REFUSED Bad_Ref_Line", "REFUSED Bad_Object_Id"),
      "sha1 refs read as sha256")
check(run("refs", file_of(refs256), 7).splitlines()[-1] == "REFUSED Bad_Ref_Line", "sha256 refs read as sha1")
fetch256 = fetchreq.replace(pkt("object-format=sha1\n"), pkt("object-format=sha256\n"))
fetch256 = fetch256[:fetch256.index(pkt("want %s\n" % tips[0]))] + b"".join(
    pkt("want %s\n" % t) for t in sorted(set(want256.values()))) + pkt("done\n") + b"0000"
resp256 = git("upload-pack", "--stateless-rpc", repo256, input=fetch256, extra={"GIT_PROTOCOL": "version=2"})
p256 = os.path.join(work, "fetched-256.pack")
check(run("fetch", file_of(resp256), 13, p256).startswith("ENDED "), "a sha256 fetch response")
compare(open(p256, "rb").read(), "fetched-256", ((7, 13), (4096, 1000)), "sha256")
objs256 = git("rev-list", "--objects", "--all", cwd=repo256)
compare(git("pack-objects", "--stdout", "--window=50", "--depth=50", cwd=repo256, input=objs256), "ref-deltas-256",
        ((4096, 1000),), "sha256")
check(run("pack", file_of(open(p256, "rb").read()), 4096, 1000).startswith("REFUSED"),
      "a sha256 pack read as sha1 is refused")
for t in objs256.decode().splitlines():
    oid = t.split()[0]
    if git("cat-file", "-t", oid, cwd=repo256).strip() != b"tree":
        continue
    ours = run("tree", file_of(git("cat-file", "tree", oid, cwd=repo256)), fmt="sha256").splitlines()
    theirs = ["%s %s %s" % (mode_names[l.split()[0]], l.split("\t")[1], l.split()[2])
              for l in git("ls-tree", oid, cwd=repo256).decode().splitlines()]
    check(ours == theirs + ["ENDED"], "sha256 tree %s" % oid)
print("git-codecs: sha256 PASS" if failures == 0 else "git-codecs: sha256 FAILED")

print("git-codecs: %d checks, %d failures" % (checks, failures))
sys.exit(1 if failures else 0)
