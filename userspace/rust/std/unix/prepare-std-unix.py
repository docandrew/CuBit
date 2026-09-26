#!/usr/bin/env python3
"""Prepare Rust std sources for the Unix-family CuBit target (docs/rust-std.md).

CuBit's libc (userspace/libc) is musl with CuBit's own system-call layer, so
it presents exactly the Linux/musl C ABI. This makes target_os = "cubit"
take std's and the libc crate's Linux arms: ABI facts (struct layouts,
constants, symbols) are right by construction, and any Linux-specific
*behavior* std relies on reaches CuBit's syscall layer, which implements it
deliberately or returns ENOSYS and reports the call on the console.

Writes <out>/library (std sources with a patched copy of the libc crate).
Run in the Nix shell: prepare-std-unix.py <out>
"""
import os, re, shutil, subprocess, sys

out = os.path.normpath(sys.argv[1])
sysroot = subprocess.check_output(["rustc", "--print", "sysroot"], text=True).strip()
src = os.path.join(sysroot, "lib/rustlib/src/rust/library")
lib = os.path.join(out, "library")

def writable(path):
    for root, dirs, files in os.walk(path):
        for name in [root] + [os.path.join(root, n) for n in dirs + files]:
            if not os.path.islink(name):
                os.chmod(name, os.stat(name).st_mode | 0o200)

if os.path.exists(out):
    writable(out)
    shutil.rmtree(out)
shutil.copytree(src, lib, symlinks=False)
writable(lib)

LINUX = re.compile(r'target_os\s*=\s*"linux"')
BOTH = 'any(target_os = "linux", target_os = "cubit")'

def cubitize(root, exts=(".rs",)):
    changed = 0
    for d, _, files in os.walk(root):
        for name in files:
            if not name.endswith(exts):
                continue
            p = os.path.join(d, name)
            s = open(p, encoding="utf-8").read()
            t = LINUX.sub(BOTH, s)
            if t != s:
                open(p, "w", encoding="utf-8").write(t)
                changed += 1
    return changed

n_std = cubitize(os.path.join(lib, "std"))

# CuBit has no standard descriptors or signals: a descriptor exists only
# for a granted CuBit object, and stdout/stderr are the program's streams.
# Skip std's Unix startup that reopens "closed" 0-2 on /dev/null and sets
# SIGPIPE handling.
p = os.path.join(lib, "std", "src", "sys", "pal", "unix", "mod.rs")
s = open(p).read()
for call in ("    sanitize_standard_fds();\n", "    reset_sigpipe(sigpipe);\n"):
    assert s.count(call) == 1, call
    s = s.replace(call, '    #[cfg(not(target_os = "cubit"))]\n' + call)
open(p, "w").write(s)
p = os.path.join(lib, "std", "build.rs")
s = open(p).read()
s = s.replace('target_os == "linux"', '(target_os == "linux" || target_os == "cubit")')
open(p, "w").write(s)

# The libc crate std uses, patched the same way.
libc_src = [d for d in os.listdir(os.path.join(lib, "vendor")) if d.startswith("libc-0.2")][0]
libc_dir = os.path.join(out, "libc")
shutil.copytree(os.path.join(lib, "vendor", libc_src), libc_dir)
writable(libc_dir)
n_libc = cubitize(os.path.join(libc_dir, "src"))
p = os.path.join(libc_dir, "build.rs")
if os.path.exists(p):
    s = open(p).read()
    s = s.replace('"linux"', '"linux" | "cubit"') if 'match' in s and '"linux" =>' in s else s
    open(p, "w").write(s)

# std accepts the cfg value; std's libc dependency comes from the patched copy.
p = os.path.join(lib, "std", "Cargo.toml")
s = open(p).read()
s = re.sub(r"(check-cfg\s*=\s*\[)",
           lambda m: m.group(1) + "\n    'cfg(target_os, values(\"cubit\"))',", s, count=1)
open(p, "w").write(s)
p = os.path.join(lib, "Cargo.toml")
s = open(p).read()
rel = os.path.relpath(libc_dir, lib)
entry = f'libc = {{ path = "{rel}" }}'
s = s.replace("[patch.crates-io]", f"[patch.crates-io]\n{entry}", 1) if "[patch.crates-io]" in s \
    else s + f"\n[patch.crates-io]\n{entry}\n"
open(p, "w").write(s)
print(f"prepared {out}: std {n_std} files, libc {n_libc} files take the Linux ABI arms")
