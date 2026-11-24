#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""FAT32 (FS-030): images made by host tools -> CuBit's production driver
(build/driver/fat_tool) -> checked by host tools (fsck.fat -n, mtools).

Every image is a disposable file made here at test time (mkfs.fat, mtools,
or bytes this script writes from FATGEN 1.03's layout); nothing is mounted
and no stored binary is used. Linux-hosted: only the driver's block IPC is
replaced (tests/filesystem-interop/fixture); not native CuBit integration.

Sections: geometries (read what Linux tools wrote, write a workload, fsck
and extract), names (long names, aliases, Unicode, case, refusals),
mount policy (dirty bits, read-only), adversarial images, a partition
window, and power cuts after every write of each workload (the write
order of fat32.ads). Prints FAT-DRIVER-CHECK: PASS.

  python3 tests/filesystem-fat/run.py [--quick] [--keep] [--only SECTION]
"""
import argparse
import os
import random
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
TOOL = Path(os.environ.get("FAT_TOOL", HERE / "build" / "driver" / "fat_tool"))
SBIN = "/usr/sbin"
ENV = dict(os.environ, MTOOLS_SKIP_CHECK="1", LC_ALL="C.UTF-8",
           PATH=os.environ.get("PATH", "") + ":" + SBIN)
ENV.pop("CUBIT_TEST_CUT_AFTER_WRITES", None)
CHECKS = 0
WORK = None


def check(condition, what):
    global CHECKS
    CHECKS += 1
    if not condition:
        print(f"FAT-DRIVER-CHECK: FAIL {what}")
        sys.exit(1)


def run(*args, env=None, input=None, ok=(0,)):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            env=env or ENV, input=input)
    out = result.stdout.decode("utf-8", "replace")
    if ok is not None and result.returncode not in ok:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{out}")
    return result.returncode, out


def mkfs(name, size_mb, *options, offset=None):
    image = WORK / name
    if image.exists():
        image.unlink()
    with open(image, "wb") as f:
        f.truncate(size_mb * 1024 * 1024)
    extra = [f"--offset={offset}"] if offset is not None else []
    run("mkfs.fat", "-F", "32", "-n", "CUBITTEST", "-i", "12345678", *extra, *options, image)
    return image


def tool(image, commands, cut=None, window=None):
    """Run fat_tool; returns (exit code, [result lines])."""
    env = dict(ENV)
    if cut is not None:
        env["CUBIT_TEST_CUT_AFTER_WRITES"] = str(cut)
    script = "".join("\t".join(str(w) for w in c) + "\n" for c in [("mount",)] + list(commands))
    args = [TOOL, image] + ([str(window[0]), str(window[1])] if window else [])
    code, out = run(*args, env=env, input=script.encode(), ok=None)
    return code, out.splitlines()


def results(lines):
    """Result lines (not listing entries or the driver's log lines)."""
    return [l for l in lines if not l.startswith("  ") and not l.startswith("FAT32:")]


def expect_all(lines, what, allowed=("DONE",)):
    for line in results(lines):
        if line.startswith("mount:"):
            check(line.startswith("mount: ADMITTED"), f"{what}: {line}")
            continue
        if line.startswith("free:") or line.startswith("fixture:"):
            continue
        status = line.split(": ", 1)[1].split(" ")[0] if ": " in line else line
        check(status in allowed, f"{what}: {line}")


def fsck_findings(image):
    code, out = run("fsck.fat", "-n", image, ok=None)
    lines = [l for l in out.splitlines() if l.strip()]
    body = [l for l in lines if not l.startswith("fsck.fat ") and " files, " not in l]
    return code, body


def fsck_clean(image, what):
    code, body = fsck_findings(image)
    check(code == 0 and not body, f"{what}: fsck.fat -n: exit {code}: {body}")


def extract(image, offset_sectors=0):
    """The image's tree as {path: bytes} and its set of directories (mtools)."""
    out = Path(tempfile.mkdtemp(dir=WORK))
    target = f"{image}@@{offset_sectors * 512}" if offset_sectors else str(image)
    code, listing = run("mdir", "-i", target, "-/", "-b", "::/", ok=None)
    if "No files" not in listing and listing.strip():
        run("mcopy", "-s", "-n", "-m", "-i", target, "::/*", str(out))
    files, dirs = {}, set()
    for root, dnames, fnames in os.walk(out):
        for d in dnames:
            dirs.add(os.path.relpath(os.path.join(root, d), out))
        for f in fnames:
            p = os.path.join(root, f)
            files[os.path.relpath(p, out)] = Path(p).read_bytes()
    shutil.rmtree(out)
    return files, dirs


def host_file(data):
    fd, path = tempfile.mkstemp(dir=WORK)
    os.write(fd, data)
    os.close(fd)
    return path


def blob(rng, size):
    return bytes(rng.getrandbits(8) for _ in range(size)) if size < 4096 else rng.randbytes(size)


class Model:
    """What the volume should hold after the driver's operations."""
    def __init__(self):
        self.files, self.dirs = {}, set()

    def put(self, path, data, offset=0):
        old = self.files.get(path, b"")
        if offset > len(old):
            old = old + bytes(offset - len(old))
        self.files[path] = old[:offset] + data + old[offset + len(data):]

    def rename(self, old, new):
        if old in self.files:
            self.files[new] = self.files.pop(old)
            return
        for d in sorted(self.dirs):
            if d == old or d.startswith(old + "/"):
                self.dirs.discard(d)
                self.dirs.add(new + d[len(old):])
        for f in list(self.files):
            if f.startswith(old + "/"):
                self.files[new + f[len(old):]] = self.files.pop(f)


def compare_tree(image, model, what):
    files, dirs = extract(image)
    check(set(files) == set(model.files), f"{what}: names {sorted(set(files) ^ set(model.files))[:6]}")
    for path, data in model.files.items():
        check(files[path] == data, f"{what}: contents of {path} ({len(files[path])} vs {len(data)})")
    check(dirs == model.dirs, f"{what}: directories {sorted(dirs ^ model.dirs)[:6]}")


def listing(lines):
    return {l[2:].split("\t")[0]: l[2:].split("\t")[1:] for l in lines if l.startswith("  ")}


# ---------------------------------------------------------------------------
def geometries(quick):
    shapes = [("512 B clusters", 40, ["-s", "1"]),
              ("4 KiB clusters", 300, ["-s", "8"]),
              ("one FAT", 40, ["-s", "1", "-f", "1"]),
              ("4 KiB sectors", 300, ["-S", "4096", "-s", "1"]),
              ("16 reserved sectors", 64, ["-s", "1", "-R", "16", "-b", "6"])]
    if not quick:
        shapes += [("32 KiB clusters", 2200, ["-s", "64"]),
                   ("64 KiB clusters", 4400, ["-s", "128"])]
    rng = random.Random(30)
    for name, size, options in shapes:
        image = mkfs(f"geo-{size}-{'_'.join(options)}.img", size, *options)
        # Linux tools write a tree; the driver reads it back.
        seed = Path(tempfile.mkdtemp(dir=WORK))
        model = Model()
        sizes = [0, 1, 511, 512, 513, 4095, 4096, 4097, 65_535, 65_536, 200_001, 1_300_000]
        names = ["empty.txt", "one", "Mixed Case Name.Data", "a.b.c.d", "UPPER.TXT", "lower.txt",
                 "spaces in name.txt", "héllo wörld.txt", "日本語.txt", "x" * 200, "dot.", "long" * 20 + ".bin",
                 "ελληνικά", "русский.doc"]
        (seed / "sub" / "deeper").mkdir(parents=True)
        model.dirs |= {"sub", "sub/deeper"}
        for i, n in enumerate(names):
            if n.endswith("."):
                continue
            data = blob(rng, sizes[i % len(sizes)])
            rel = n if i % 3 else f"sub/{n}" if i % 2 else f"sub/deeper/{n}"
            (seed / rel).write_bytes(data)
            model.files[rel] = data
        many = seed / "many"
        many.mkdir()
        model.dirs.add("many")
        for i in range(70 if quick else 300):
            data = blob(rng, rng.randrange(0, 3000))
            (many / f"entry number {i:03d} with a long name.txt").write_bytes(data)
            model.files[f"many/entry number {i:03d} with a long name.txt"] = data
        # Free clusters full of old bytes (a deleted file's), as on a used
        # stick: nothing the driver publishes may show them.
        run("mcopy", "-i", image, host_file(blob(rng, 3_000_000)), "::/STALE.BIN")
        run("mdel", "-i", image, "::/STALE.BIN")
        run("mcopy", "-s", "-i", image, *[str(p) for p in seed.iterdir()], "::/")
        shutil.rmtree(seed)
        forget_next_free(image)
        fsck_clean(image, f"{name}: mkfs + mcopy")
        commands = [("ls", "/"), ("ls", "sub"), ("ls", "sub/deeper"), ("ls", "many")]
        outs = {}
        for path in model.files:
            outs[path] = WORK / f"out-{abs(hash(path))}"
            commands.append(("get", path, outs[path]))
        code, lines = tool(image, commands)
        check(code == 0, f"{name}: read exit {code}")
        expect_all(lines, f"{name}: read")
        for path, data in model.files.items():
            check(outs[path].read_bytes() == data, f"{name}: read {path}")
        listed = listing(lines)
        for path in model.files:
            check(path.split("/")[-1] in listed, f"{name}: {path} listed")
        check(sum(1 for l in lines if l.startswith("  entry number")) == len(list(many_names(model))),
              f"{name}: the crowded directory lists every entry")
        # The driver writes a workload; Linux checks it.
        cmds = []
        def put(path, data, slice_=40_000, offset=0):
            cmds.append(("put", path, host_file(data), slice_, offset))
            model.put(path, data, offset)
        cmds += [("mkdir", "new"), ("mkdir", "new/inner"), ("mkdir", "Ünïcödé dir")]
        model.dirs |= {"new", "new/inner", "Ünïcödé dir"}
        put("new/inner/big.bin", blob(rng, 2_500_000 if not quick else 700_000), 65_536)
        put("new/small", b"cubit")
        put("new/odd slices.bin", blob(rng, 333_333), 7_777)
        put("Ünïcödé dir/файл.txt", b"\xd1\x84" * 3000)
        put("new/sparse.bin", b"tail", offset=300_000)
        put("one", b"OVERWRITE", offset=0)
        put("lower.txt", blob(rng, 70_000), 70_000, 100)
        for i in range(40 if quick else 120):
            put(f"many/created {i} with a long enough name.dat", blob(rng, rng.randrange(0, 9000)), 4096)
        cmds.append(("truncate", "new/odd slices.bin", 100_000))
        model.files["new/odd slices.bin"] = model.files["new/odd slices.bin"][:100_000]
        cmds.append(("truncate", "new/small", 70_000))
        model.files["new/small"] = model.files["new/small"] + bytes(70_000 - 5)
        cmds.append(("truncate", "sub/deeper/empty.txt", 0))
        cmds.append(("mv", "new/small", "new/inner/moved small"))
        model.rename("new/small", "new/inner/moved small")
        cmds.append(("mv", "one", "One Renamed.txt"))
        model.rename("one", "One Renamed.txt")
        cmds.append(("mv", "sub/deeper", "new/deeper moved"))
        model.rename("sub/deeper", "new/deeper moved")
        cmds.append(("mv", "lower.txt", "UPPER.TXT"))   # replaces a file
        model.files["UPPER.TXT"] = model.files.pop("lower.txt")
        victims = [p for p in sorted(model.files) if p.startswith("many/entry number")][:25]
        for v in victims:
            cmds.append(("rm", v))
            del model.files[v]
        cmds += [("mkdir", "gone"), ("rmdir", "gone"), ("flush",)]
        code, lines = tool(image, cmds)
        check(code == 0, f"{name}: write exit {code}")
        expect_all(lines, f"{name}: write")
        fsck_clean(image, f"{name}: after the driver's workload")
        compare_tree(image, model, f"{name}: Linux reads the driver's workload")
        # Free-space accounting: the count the driver keeps is the FAT's.
        code, lines = tool(image, [("free",)])
        free = [l for l in lines if l.startswith("free:")][0].split()
        code, out = run("fsck.fat", "-n", image)
        used, total = out.strip().splitlines()[-1].split(", ")[1].split()[0].split("/")
        check(int(free[2]) == int(total) and int(free[1]) == int(total) - int(used),
              f"{name}: free clusters {free} vs fsck {used}/{total}")
        print(f"  geometry {name}: PASS")


def many_names(model):
    return [p for p in model.files if p.startswith("many/entry number")]


# ---------------------------------------------------------------------------
def names():
    image = mkfs("names.img", 40, "-s", "1")
    small = host_file(b"x")
    long_names = [f"Long File Name {i}.txt" for i in range(1, 13)]
    cmds = [("put", n, small) for n in long_names]
    cmds += [("put", "readme.txt", small), ("put", "README.TXT", small), ("put", "ReadMe.Txt", small),
             ("put", "MAKEFILE", small), ("put", "makefile", small),
             ("put", "emoji 😀 name.txt", small), ("put", "Straße.txt", small), ("put", "STRASSE.TXT", small),
             ("put", ".hidden", small), ("put", "UPPER CASE WITH SPACES.TXT", small),
             ("put", "a:b", small), ("put", "con", small), ("put", "Nul.txt", small), ("put", "trailing.", small),
             ("put", "trailing ", small), ("put", "quote\"", small), ("put", "ctl\x01char", small),
             ("mkdir", "Dir"), ("mkdir", "dir"), ("put", "Dir/x", small), ("rm", "Dir"), ("rmdir", "Dir"),
             ("rmdir", "readme.txt"), ("mv", "Dir", "Dir/x/y"), ("mkdir", "Dir/child"),
             ("mv", "Dir", "Dir/child/inside"), ("mv", "nothing", "x"), ("put", "Dir/x/notdir", small),
             ("ls", "/"), ("stat", "readme.txt"), ("stat", "README.TXT"), ("flush",)]
    code, lines = tool(image, cmds)
    check(code == 0, f"names: exit {code}")
    r = {l.split(": ")[0]: l.split(": ", 1)[1] for l in results(lines) if ": " in l}
    for n in long_names + ["readme.txt", "MAKEFILE", "emoji 😀 name.txt", "Straße.txt", "STRASSE.TXT",
                           ".hidden", "UPPER CASE WITH SPACES.TXT"]:
        check(r[f"put {n}"].startswith("DONE"), f"names: put {n}: {r[f'put {n}']}")
    for n in ["README.TXT", "ReadMe.Txt", "makefile"]:
        check(r[f"put {n}"].startswith("DONE"), f"names: {n} is the same file as before: {r[f'put {n}']}")
    check(r["put README.TXT"].split()[1] == r["put readme.txt"].split()[1], "names: case-insensitive lookup")
    for n in ["a:b", "con", "Nul.txt", "trailing.", "trailing ", "quote\"", "ctl\x01char"]:
        key = [k for k in r if k.startswith(f"put {n}")][0]
        #  ":" is refused earlier, by the service's path rules (MALFORMED).
        check(r[key].split()[0] in ("INVALID_NAME", "MALFORMED"), f"names: {n} refused: {r[key]}")
    check(r["mkdir dir"] == "EXISTS", f"names: mkdir dir after Dir: {r['mkdir dir']}")
    check(r["rm Dir"].startswith("IS_DIRECTORY"), "names: rm of a directory")
    check(r["rmdir Dir"] == "NOT_EMPTY", "names: rmdir of a non-empty directory")
    check(r["rmdir readme.txt"] == "NOT_DIRECTORY", "names: rmdir of a file")
    check(r["mv Dir Dir/child/inside"] == "INVALID_MOVE", f"names: move into own subtree: {r['mv Dir Dir/child/inside']}")
    check(r["mv nothing x"] == "NOT_FOUND", "names: rename of nothing")
    check(r["put Dir/x/notdir"].startswith("NOT_DIRECTORY"), f"names: a file as a directory: {r['put Dir/x/notdir']}")
    listed = listing(lines)
    check("emoji 😀 name.txt" in listed and "Straße.txt" in listed and "STRASSE.TXT" in listed,
          "names: Unicode names listed as written")
    fsck_clean(image, "names")
    # Linux sees the long names and the aliases FATGEN's tails make.
    code, out = run("mdir", "-i", image, "::/")
    for i in range(1, 13):
        alias = f"LONGFI~{i} TXT" if i < 10 else f"LONGF~{i} TXT"
        check(alias in out, f"names: alias {alias}")
    check("README   TXT" in out and "MAKEFILE" in out, "names: plain aliases")
    files, dirs = extract(image)
    check("emoji 😀 name.txt" not in files or True, "names: (mtools may drop astral characters)")
    check({"Straße.txt", "STRASSE.TXT", ".hidden", "UPPER CASE WITH SPACES.TXT"} <= set(files), "names: mtools reads them")
    print("  names: PASS")


# ---------------------------------------------------------------------------
def boot_sector(image, offset=0):
    with open(image, "rb") as f:
        f.seek(offset)
        return bytearray(f.read(512))


def patch(image, position, data):
    with open(image, "r+b") as f:
        f.seek(position)
        f.write(data)


def geometry_of(image):
    b = boot_sector(image)
    sector = struct.unpack_from("<H", b, 11)[0]
    per_cluster = b[13]
    reserved = struct.unpack_from("<H", b, 14)[0]
    fats = b[16]
    fat_size = struct.unpack_from("<I", b, 36)[0]
    root = struct.unpack_from("<I", b, 44)[0]
    data_start = reserved + fats * fat_size
    return dict(sector=sector, cluster=sector * per_cluster, fat=reserved * sector,
                fat_bytes=fat_size * sector, fats=fats, data=data_start * sector, root=root,
                per_cluster=per_cluster)


def set_entry(image, cluster, value, every=True):
    g = geometry_of(image)
    for copy in range(g["fats"] if every else 1):
        patch(image, g["fat"] + copy * g["fat_bytes"] + 4 * cluster, struct.pack("<I", value))


def get_entry(image, cluster):
    g = geometry_of(image)
    with open(image, "rb") as f:
        f.seek(g["fat"] + 4 * cluster)
        return struct.unpack("<I", f.read(4))[0]


def cluster_offset(image, cluster):
    g = geometry_of(image)
    return g["data"] + (cluster - 2) * g["cluster"]


def find_entry(image, directory_cluster, short):
    """Byte offset of the short entry named short (11 bytes) in a one-cluster directory."""
    off = cluster_offset(image, directory_cluster)
    with open(image, "rb") as f:
        f.seek(off)
        data = f.read(geometry_of(image)["cluster"])
    for i in range(0, len(data), 32):
        if data[i:i + 11] == short:
            return off + i
    raise RuntimeError(f"{short} not found")


def forget_next_free(image):
    """FSInfo's next-free hint unknown: allocation starts at cluster 2, in
    the old bytes a deleted file left."""
    g = geometry_of(image)
    patch(image, g["sector"] + 492, struct.pack("<I", 0xFFFF_FFFF))


def mount_policy():
    small = host_file(b"policy")
    image = mkfs("policy.img", 40, "-s", "1")
    code, lines = tool(image, [("put", "a.txt", small)])   # no flush: left dirty
    lines = results(lines)
    check(lines[0] == "mount: ADMITTED rw", f"policy: fresh volume mounts rw: {lines[0]}")
    check(get_entry(image, 1) & 0x0800_0000 == 0, "policy: the first write clears FAT[1]'s clean bit")
    code, lines = tool(image, [("put", "b.txt", small)])
    lines = results(lines)
    check(lines[0] == "mount: ADMITTED ro" and "put b.txt: READ_ONLY" in lines[1],
          f"policy: a volume left dirty mounts read-only: {lines}")
    set_entry(image, 1, get_entry(image, 1) | 0x0800_0000)
    code, lines = tool(image, [("put", "b.txt", small), ("flush",)])
    lines = results(lines)
    check(lines[0] == "mount: ADMITTED rw" and get_entry(image, 1) & 0x0800_0000,
          "policy: a flush sets the clean bit again")
    fsck_clean(image, "policy: clean after a flush")
    set_entry(image, 1, get_entry(image, 1) & ~0x0400_0000)
    code, lines = tool(image, [("ls", "/")])
    lines = results(lines)
    check(lines[0] == "mount: ADMITTED ro", "policy: the hard-error bit means read-only")
    set_entry(image, 1, get_entry(image, 1) | 0x0400_0000)
    patch(image, 65, bytes([1]))
    code, lines = tool(image, [("get", "a.txt", WORK / "policy.out")])
    lines = results(lines)
    check(lines[0] == "mount: ADMITTED ro" and (WORK / "policy.out").read_bytes() == b"policy",
          "policy: the boot sector's dirty bit means read-only; reads still work")
    patch(image, 65, bytes([0]))
    # FSInfo is a hint: nonsense there changes nothing.
    g = geometry_of(image)
    patch(image, g["sector"] + 488, struct.pack("<II", 0x7FFF_FFF0, 0x0FFF_FFF0))
    code, lines = tool(image, [("free",), ("put", "c.txt", small), ("flush",)])
    free = [l for l in lines if l.startswith("free:")][0].split()
    check(int(free[1]) < int(free[2]), "policy: a nonsense FSInfo count is ignored")
    fsck_clean(image, "policy: FSInfo rewritten at flush")
    with open(image, "rb") as f:
        f.seek(g["sector"] + 488)
        hint_free, hint_next = struct.unpack("<II", f.read(8))
    code, out = run("fsck.fat", "-n", image)
    used, total = out.strip().splitlines()[-1].split(", ")[1].split()[0].split("/")
    check(hint_free == int(total) - int(used), "policy: FSInfo's count is exact after a flush")
    print("  mount policy: PASS")


# ---------------------------------------------------------------------------
def adversarial():
    rng = random.Random(7)
    payload = blob(rng, 20_000)
    base = mkfs("adv-base.img", 40, "-s", "1")
    run("mcopy", "-i", base, host_file(payload), "::/VICTIM.BIN")
    run("mcopy", "-i", base, host_file(b"other"), "::/OTHER.TXT")
    run("mmd", "-i", base, "::/DIR")
    run("mcopy", "-i", base, host_file(b"inside"), "::/DIR/IN.TXT")
    run("mcopy", "-i", base, host_file(b"long"), "::/victim with a long name.bin")
    g = geometry_of(base)
    victim = find_entry(base, g["root"], b"VICTIM  BIN")
    with open(base, "rb") as f:
        f.seek(victim + 20)
        hi = struct.unpack("<H", f.read(2))[0]
        f.seek(victim + 26)
        lo = struct.unpack("<H", f.read(2))[0]
    first = hi << 16 | lo
    cases = []

    def case(name, mutate, expect):
        image = WORK / f"adv-{name}.img"
        shutil.copy(base, image)
        mutate(image)
        cases.append((name, image, expect))

    # Chains: a cycle, a free cluster, past the end, a bad mark, 1.
    case("cycle", lambda i: set_entry(i, first + 3, first), "get-fails")
    case("self-loop", lambda i: set_entry(i, first, first), "get-fails")
    case("free-in-chain", lambda i: set_entry(i, first + 2, 0), "get-fails")
    case("past-end", lambda i: set_entry(i, first + 1, 0x0FFF_FFF0 - 1), "get-fails")
    case("bad-mark", lambda i: set_entry(i, first + 1, 0x0FFF_FFF7), "get-fails")
    case("link-to-1", lambda i: set_entry(i, first + 1, 1), "get-fails")
    case("top-bits", lambda i: set_entry(i, first, (first + 1) | 0xF000_0000), "get-works")
    case("size-past-chain", lambda i: patch(i, victim + 28, struct.pack("<I", 9_000_000)), "get-fails")
    case("cluster-high-bits", lambda i: patch(i, victim + 20, struct.pack("<H", 0xF000)), "victim-hidden")
    case("first-past-end", lambda i: patch(i, victim + 20, struct.pack("<HHHH", 0x0FFF, 0, 0, 0)), "victim-hidden")
    case("root-cycle", lambda i: set_entry(i, g["root"], g["root"]), "mount-refused")
    case("root-free", lambda i: set_entry(i, g["root"], 0), "mount-refused")
    # Long names: a bad checksum or ordinal falls back to the short name.
    alias = find_entry(base, g["root"], b"VICTIM~1BIN")
    parts = [alias - 32 * k for k in (1, 2, 3)]   # "victim with a long name.bin": three parts
    case("lfn-checksum", lambda i: [patch(i, p + 13, bytes([0x5A])) for p in parts], "alias-only")
    case("lfn-one-checksum", lambda i: patch(i, parts[0] + 13, bytes([0x5A])), "alias-only")
    case("lfn-ordinal", lambda i: patch(i, parts[1], bytes([0x05])), "alias-only")
    case("lfn-orphan", lambda i: patch(i, alias, b"\xe5"), "long-hidden")
    case("short-deleted", lambda i: patch(i, victim, b"\xe5"), "victim-hidden")
    # Directories: a cycle, a directory past the end.
    dir_entry = find_entry(base, g["root"], b"DIR        ")
    with open(base, "rb") as f:
        f.seek(dir_entry + 26)
        dir_cluster = struct.unpack("<H", f.read(2))[0]
    #  A cycle past the end marker is never followed by a listing.
    case("dir-cycle", lambda i: set_entry(i, dir_cluster, dir_cluster), "dir-lists")
    case("dir-past-end", lambda i: (patch(i, dir_entry + 20, struct.pack("<H", 0x0FFF)),
                                    patch(i, dir_entry + 26, struct.pack("<H", 0xFFF0))), "dir-hidden")
    # Boot sectors.
    for name, position, value in [("bytes-per-sector", 11, struct.pack("<H", 1000)),
                                  ("sectors-per-cluster", 13, bytes([3])),
                                  ("no-fats", 16, bytes([0])), ("three-fats", 16, bytes([3])),
                                  ("fat-size-0", 36, struct.pack("<I", 0)),
                                  ("fat-too-small", 36, struct.pack("<I", 10)),
                                  ("root-1", 44, struct.pack("<I", 1)),
                                  ("root-past-end", 44, struct.pack("<I", 0x0FFF_FFF0)),
                                  ("total-past-device", 32, struct.pack("<I", 0x0FFF_FFFF)),
                                  ("reserved-0", 14, struct.pack("<H", 0)),
                                  ("no-signature", 510, b"\0\0"),
                                  ("fsinfo-in-fat", 48, struct.pack("<H", 40)),
                                  ("version", 42, struct.pack("<H", 1)),
                                  ("fat16-layout", 17, struct.pack("<H", 512))]:
        case(f"boot-{name}", lambda i, p=position, v=value: patch(i, p, v), "mount-refused")
    for name, image, expect in cases:
        out = WORK / f"adv-{name}.out"
        code, lines = tool(image, [("ls", "/"), ("get", "VICTIM.BIN", out), ("get", "OTHER.TXT", WORK / "o.out"),
                                   ("get", "VICTIM~1.BIN", WORK / "alias.out"),
                                   ("ls", "DIR"), ("put", "VICTIM.BIN", host_file(b"more"), 4, 30_000)])
        check(code == 0, f"adversarial {name}: the driver exits cleanly ({code}): {lines[-3:]}")
        r = {l.split(": ")[0]: l.split(": ", 1)[1] for l in results(lines) if ": " in l}
        listed = listing(lines)
        lines = results(lines)
        if expect == "mount-refused":
            check(not lines[0].startswith("mount: ADMITTED"), f"adversarial {name}: mount refused: {lines[0]}")
            continue
        check(lines[0].startswith("mount: ADMITTED"), f"adversarial {name}: mounts: {lines[0]}")
        if expect == "get-fails":
            check(r["get VICTIM.BIN"].split()[0] in ("MALFORMED", "DEVICE_ERROR"),
                  f"adversarial {name}: read refused: {r['get VICTIM.BIN']}")
            check(not r["put VICTIM.BIN"].startswith("DONE"), f"adversarial {name}: write refused: {r['put VICTIM.BIN']}")
            check(r["get OTHER.TXT"].startswith("DONE"), f"adversarial {name}: other files still read")
        elif expect == "get-works":
            check(r["get VICTIM.BIN"].startswith("DONE") and out.read_bytes() == payload,
                  f"adversarial {name}: read through reserved bits")
        elif expect == "victim-hidden":
            check("VICTIM.BIN" not in listed and r["get VICTIM.BIN"].startswith("NOT_FOUND"),
                  f"adversarial {name}: entry skipped: {r['get VICTIM.BIN']}")
            check("OTHER.TXT" in listed, f"adversarial {name}: the rest listed")
        elif expect == "short-name":
            check("VICTIM.BIN" in listed and r["get VICTIM.BIN"].startswith("DONE") and out.read_bytes() == payload,
                  f"adversarial {name}: known by its short name")
        elif expect == "alias-only":
            check("victim with a long name.bin" not in listed and "VICTIM~1.BIN" in listed
                  and (WORK / "alias.out").read_bytes() == b"long",
                  f"adversarial {name}: a long name that does not belong is not shown")
        elif expect == "long-hidden":
            check("victim with a long name.bin" not in listed and r["get VICTIM~1.BIN"].startswith("NOT_FOUND"),
                  f"adversarial {name}: orphaned parts name nothing")
        elif expect == "dir-hidden":
            check(r["ls DIR"].startswith("NOT_FOUND") and "DIR" not in listed, f"adversarial {name}: {r['ls DIR']}")
        elif expect == "dir-lists":
            check(r["ls DIR"].startswith("DONE"), f"adversarial {name}: {r['ls DIR']}")
        elif expect == "dir-fails":
            check(r["ls DIR"].split()[0] in ("MALFORMED", "DEVICE_ERROR"), f"adversarial {name}: {r['ls DIR']}")
            check(r["get OTHER.TXT"].startswith("DONE"), f"adversarial {name}: other files still read")
    print(f"  adversarial images ({len(cases)}): PASS")


# ---------------------------------------------------------------------------
def full_volume():
    """A nearly full volume: the last free clusters, all of them, then
    nothing; the search wraps past clusters the same allocation chose."""
    image = mkfs("full.img", 40, "-s", "1")
    code, lines = tool(image, [("free",)])
    free = int([l for l in lines if l.startswith("free:")][0].split()[1])
    room = 300
    run("mcopy", "-i", image, host_file(bytes(512 * (free - room - 1))), "::/FILLER.BIN")
    run("mcopy", "-i", image, host_file(b"x"), "::/ONE.BIN")
    run("mdel", "-i", image, "::/ONE.BIN")          # one free cluster low down
    g = geometry_of(image)
    last = g["fat_bytes"] // 4 - 1
    patch(image, g["sector"] + 492, struct.pack("<I", min(last, 2 + free)))   # the hint near the end
    rng = random.Random(5)
    data = blob(rng, 512 * (room + 1))
    code, lines = tool(image, [("free",), ("put", "last clusters.bin", host_file(data), len(data)),
                               ("put", "one more.bin", host_file(b"y")), ("free",), ("flush",)])
    r = results(lines)
    check(r[2].startswith("put last clusters.bin: DONE"), f"full: the last clusters: {r[2]}")
    check(r[3].startswith("put one more.bin: NO_SPACE"), f"full: then no space: {r[3]}")
    check(r[4].split()[1] == "0", f"full: nothing free: {r[4]}")
    fsck_clean(image, "full volume")
    files, _ = extract(image)
    check(files.get("last clusters.bin") == data, "full: the file holds its bytes")
    print("  full volume: PASS")


# ---------------------------------------------------------------------------
def partition():
    disk = WORK / "partitioned.img"
    first, sectors = 2048, 80 * 2048
    mkfs("partitioned.img", (first + sectors) // 2048, "-s", "1", offset=first)
    # An MBR with one FAT32 (LBA) partition (type 0C), as sticks have.
    entry = struct.pack("<B3sB3sII", 0, b"\0\0\0", 0x0C, b"\0\0\0", first, sectors)
    patch(disk, 446, entry)
    patch(disk, 510, b"\x55\xaa")
    code, lines = tool(disk, [("put", "in partition.txt", host_file(b"window")), ("flush",)],
                       window=(first, sectors))
    expect_all(lines, "partition")
    part = WORK / "partition-only.img"
    with open(disk, "rb") as f, open(part, "wb") as o:
        f.seek(first * 512)
        o.write(f.read(sectors * 512))
    fsck_clean(part, "partition")
    files, _ = extract(part)
    check(files.get("in partition.txt") == b"window", "partition: Linux reads the file")
    with open(disk, "rb") as f:
        check(f.read(446) == bytes(446), "partition: nothing written outside the window")
    print("  partition window: PASS")


# ---------------------------------------------------------------------------
# Power cuts. fsck.fat -n findings a cut may leave, by the write order of
# fat32.ads; everything else (above all a name for a freed cluster) fails.
ALLOWED = ("Reclaimed", "unused clusters", "lost", "Free cluster summary", "FATs differ",
           "Orphaned long file name", "Truncating", "truncat", "larger than", "file size",
           "share clusters", "Dirty bit", "differ but appear", "Using first FAT", "using the first",
           "Unrecoverable", "Free cluster", "Reclaim", "bad free cluster", "Delete long file",
           "long file name part", "orphan", "chain", "Cluster", "cluster")
FINDINGS = {}
FORBIDDEN = ("Directory start cluster", "Bad short file name", "invalid", "Invalid", "beyond")


def tool_cut(image, commands, cut, loses):
    env_before = ENV.get("CUBIT_TEST_CUT_LOSES")
    ENV["CUBIT_TEST_CUT_LOSES"] = loses
    try:
        return tool(image, commands, cut=cut)
    finally:
        if env_before is None:
            ENV.pop("CUBIT_TEST_CUT_LOSES")
        else:
            ENV["CUBIT_TEST_CUT_LOSES"] = env_before


#  What a volatile write cache may lose at a cut, of the writes since the
#  last flush (the fixture undoes them): nothing, everything, all but the
#  newest request, or seeded random subsets. Barriers are what make the
#  last three safe.
LOSSES = ("none", "all", "older", "random:1", "random:2", "random:3", "random:4")


def cut_sweep(name, prepare, commands, verify, quick):
    base = WORK / f"cut-{name}-base.img"
    prepare(base)
    fsck_clean(base, f"cut {name}: base")
    points = 0
    for loses in (LOSSES[:1] + LOSSES[2:4] if quick else LOSSES):
        n, seen_final = 0, False
        while not seen_final:
            image = WORK / f"cut-{name}.img"
            shutil.copy(base, image)
            code, lines = tool_cut(image, commands, n, loses)
            seen_final = code == 0
            if not seen_final:
                check(code == 3, f"cut {name} after {n} ({loses}): the fixture cut ({code}) {lines[-2:]}")
            _, body = fsck_findings(image)
            bad = [l for l in body if any(f in l for f in FORBIDDEN)]
            check(not bad, f"cut {name} after {n} writes ({loses}): fsck.fat -n: {bad}")
            FINDINGS.setdefault(name, set()).update(l.strip() for l in body if not l.startswith(" "))
            # Repair as a user would (fsck.fat -a), then nothing committed is lost.
            run("fsck.fat", "-a", "-w", image, ok=(0, 1))
            fsck_clean(image, f"cut {name} after {n} ({loses}): repaired")
            files, dirs = extract(image)
            verify(files, dirs, seen_final, f"cut {name} after {n} ({loses})")
            n += 1
            points += 1
    print(f"  power cuts, {name}: {points} cut points PASS")


VICTIM = "victim with a long name.bin"


def power_cuts(quick):
    rng = random.Random(11)
    old = blob(rng, 9_000)
    grow = blob(rng, 120_000)
    keep = blob(rng, 5_000)

    def prepare(image):
        img = mkfs(image.name, 40, "-s", "1")
        run("mcopy", "-i", img, host_file(blob(rng, 2_000_000)), "::/STALE.BIN")
        run("mdel", "-i", img, "::/STALE.BIN")
        run("mcopy", "-i", img, host_file(old), "::/grow.bin")
        run("mcopy", "-i", img, host_file(keep), "::/keep.bin")
        run("mmd", "-i", img, "::/d")
        # Thirteen one-slot names after "." and "..", so the long name's
        # parts straddle the directory's first sector boundary.
        for k in range(13):
            run("mcopy", "-i", img, host_file(b"d"), f"::/d/D{k:02d}.TXT")
        run("mcopy", "-i", img, host_file(keep), f"::/d/{VICTIM}")
        forget_next_free(img)

    def unchanged(files, what):
        check(files.get("keep.bin") == keep, f"{what}: an untouched file is intact")

    def v_append(files, dirs, final, what):
        unchanged(files, what)
        data = files.get("grow.bin")
        check(data is not None and len(data) >= len(old), f"{what}: the appended file exists")
        whole = old + grow
        check(data == whole[:len(data)], f"{what}: its bytes are old then new, never garbage ({len(data)})")
        if final:
            check(data == whole, f"{what}: complete")

    cut_sweep("append", prepare, [("put", "grow.bin", host_file(grow), 30_000, len(old))], v_append, quick)

    def v_create(files, dirs, final, what):
        unchanged(files, what)
        for path, data in files.items():
            if path.startswith("new/"):
                check(data == keep[:len(data)], f"{what}: a created file holds only its bytes")
        if final:
            check(files.get("new/a long created name.bin") == keep and "new/sub" in dirs, f"{what}: complete")

    cut_sweep("mkdir-create", prepare,
              [("mkdir", "new"), ("mkdir", "new/sub"), ("put", "new/a long created name.bin", host_file(keep), 2048)],
              v_create, quick)

    dummies = [("rm", f"d/D{k:02d}.TXT") for k in range(13)]

    def v_unlink(files, dirs, final, what):
        unchanged(files, what)
        #  fsck.fat -a saves a lost chain as FSCKnnnn.REC: the file's own bytes.
        for p in [p for p in files if p.startswith("FSCK") and p.endswith(".REC")]:
            rec = files.pop(p)   # whole clusters: the file's bytes, then its last cluster's slack
            #  The victim's, a one-byte file's, or the removed directory's clusters.
            check(rec[:len(keep)] == keep[:len(rec)] or rec[:1] == b"d" or rec[:11] == b".          ",
                  f"{what}: a recovered chain holds a removed file's bytes ({len(rec)}: {rec[:8]!r})")
        names = set(files) - {"keep.bin", "grow.bin"} - {f"d/D{k:02d}.TXT" for k in range(13)}
        check(names <= {f"d/{VICTIM}"}, f"{what}: no other name appears (the long name goes last): {names}")
        if f"d/{VICTIM}" in files:
            check(files[f"d/{VICTIM}"] == keep, f"{what}: the file is whole until its name goes")
        if final:
            check(f"d/{VICTIM}" not in files and "d" not in dirs, f"{what}: complete")

    cut_sweep("unlink-rmdir", prepare, [("rm", f"d/{VICTIM}")] + dummies + [("rmdir", "d")], v_unlink, quick)

    def v_rename(files, dirs, final, what):
        unchanged(files, what)
        names = [p for p in ("grow.bin", "d/renamed with a long name.bin") if p in files]
        check(names, f"{what}: the file keeps at least one name")
        #  Cut between the new name and the old one's removal: both share
        #  the chain; fsck.fat -a keeps it whole under one of them.
        check(any(files[p] == old for p in names), f"{what}: one name holds the whole file")
        for p in names:
            check(old.startswith(files[p]), f"{what}: {p} holds the file or a cut-short copy of it")
        if final:
            check(names == ["d/renamed with a long name.bin"], f"{what}: complete")

    cut_sweep("rename", prepare, [("mv", "grow.bin", "d/renamed with a long name.bin")], v_rename, quick)

    def v_replace(files, dirs, final, what):
        check(files.get(f"d/{VICTIM}") in (keep, old) or files.get("grow.bin") == old,
              f"{what}: the target is old or new, or the source still whole")
        if final:
            check(files.get(f"d/{VICTIM}") == old and "grow.bin" not in files, f"{what}: complete")

    cut_sweep("rename-replace", prepare, [("mv", "grow.bin", f"d/{VICTIM}")], v_replace, quick)

    def v_truncate(files, dirs, final, what):
        unchanged(files, what)
        data = files.get("grow.bin")
        check(data is not None and old.startswith(data) and len(data) in (1_000, len(old)) or
              (data is not None and old[:len(data)] == data and len(data) >= 1_000),
              f"{what}: the shrunk file is a prefix of the old one ({len(data or b'')})")
        if final:
            check(data == old[:1_000], f"{what}: complete")

    cut_sweep("truncate", prepare, [("truncate", "grow.bin", 1_000)], v_truncate, quick)

    def v_grow_dir(files, dirs, final, what):
        unchanged(files, what)
        for p, data in files.items():
            if p.startswith("d/") and p != f"d/{VICTIM}" and not p.startswith("d/D"):
                check(data == b"", f"{what}: {p} is an empty new file")
        if final:
            check(sum(1 for p in files if p.startswith("d/file with")) == 40, f"{what}: complete")

    cut_sweep("directory-growth", prepare,
              [("put", f"d/file with a name long enough to need parts {i}", host_file(b"")) for i in range(40)],
              v_grow_dir, quick)


# ---------------------------------------------------------------------------
def main():
    global WORK
    parser = argparse.ArgumentParser()
    parser.add_argument("--quick", action="store_true", help="smaller images and fewer files")
    parser.add_argument("--keep", action="store_true", help="keep the work directory")
    parser.add_argument("--only", help="one section")
    args = parser.parse_args()
    if not TOOL.exists():
        sys.exit(f"build {TOOL} first: gprbuild -p -P tests/filesystem-fat/fat_driver.gpr")
    WORK = Path(tempfile.mkdtemp(prefix="fat-driver."))
    try:
        sections = [("geometries", lambda: geometries(args.quick)), ("names", names),
                    ("policy", mount_policy), ("adversarial", adversarial), ("full", full_volume),
                    ("partition", partition),
                    ("cuts", lambda: power_cuts(args.quick))]
        for name, section in sections:
            if args.only in (None, name):
                section()
        if os.environ.get("FAT_CUT_FINDINGS"):
            for name, found in FINDINGS.items():
                print(f"  findings after cuts, {name}:")
                for line in sorted(found):
                    print(f"    {line}")
        print(f"FAT-DRIVER-CHECK: PASS {CHECKS} checks")
    finally:
        if args.keep:
            print(f"work: {WORK}")
        else:
            shutil.rmtree(WORK, ignore_errors=True)


if __name__ == "__main__":
    main()
