#!/usr/bin/env python3
"""Copy a Rust crate and let target_os = "cubit" take its Linux arms.

For crates that only *describe* the Linux/musl C ABI (the libc crate), which
the CuBit libc implements. Not for crates that rely on Linux behavior
(epoll, /proc): those get a CuBit backend or are left out instead.

    cubitize.py <crate-src-dir> <destination-dir>
"""
import os, re, shutil, sys

LINUX = re.compile(r'target_os\s*=\s*"linux"')
BOTH = 'any(target_os = "linux", target_os = "cubit")'

def writable(path):
    for root, dirs, files in os.walk(path):
        for name in [root] + [os.path.join(root, n) for n in dirs + files]:
            if not os.path.islink(name):
                os.chmod(name, os.stat(name).st_mode | 0o200)

def cubitize_tree(root):
    changed = 0
    for d, _, files in os.walk(root):
        for name in files:
            # Rust sources, and Cargo.toml target tables
            # ([target.'cfg(... target_os = "linux" ...)'.dependencies]).
            if not (name.endswith(".rs") or name == "Cargo.toml"):
                continue
            p = os.path.join(d, name)
            s = open(p, encoding="utf-8").read()
            t = LINUX.sub(BOTH, s)
            if name == "build.rs":
                t = t.replace('target_os == "linux"', '(target_os == "linux" || target_os == "cubit")')
            if t != s:
                open(p, "w", encoding="utf-8").write(t)
                changed += 1
    return changed

def cubitize_crate(src, dst):
    if os.path.exists(dst):
        writable(dst)
        shutil.rmtree(dst)
    shutil.copytree(src, dst, symlinks=False)
    writable(dst)
    for junk in (".cargo-checksum.json", ".cargo_vcs_info.json"):
        p = os.path.join(dst, junk)
        if os.path.exists(p):
            os.remove(p)
    return cubitize_tree(dst)

if __name__ == "__main__":
    n = cubitize_crate(sys.argv[1], sys.argv[2])
    print(f"{sys.argv[2]}: {n} files take the Linux ABI arms")
