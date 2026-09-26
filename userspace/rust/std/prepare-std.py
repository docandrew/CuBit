#!/usr/bin/env python3
"""Prepare patched Rust std sources for CuBit (docs/rust-std.md).

Copies the pinned toolchain's rust-src library into build/std-src and makes
target_os = "cubit" take Motor OS's std arms. Motor's std layer calls a thin
runtime table (moto-rt); CuBit's fork of moto-rt (moto-rt-cubit/) fills that
table from CuBit syscalls. Run inside the Nix shell:

    python3 userspace/rust/std/prepare-std.py [output-library-dir]

Normally run through cargo-cubit.sh, which caches the result per input.
"""
import os, re, shutil, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
sysroot = subprocess.check_output(["rustc", "--print", "sysroot"], text=True).strip()
src = os.path.join(sysroot, "lib/rustlib/src/rust/library")
out = os.path.normpath(sys.argv[1] if len(sys.argv) > 1 else
                       os.path.join(here, "..", "build", "std-src", "library"))

def writable(path):
    os.chmod(path, os.stat(path).st_mode | 0o200)
    for root, dirs, files in os.walk(path):
        for name in dirs + files:
            p = os.path.join(root, name)
            if not os.path.islink(p):
                os.chmod(p, os.stat(p).st_mode | 0o200)

if os.path.exists(out):
    writable(out)
    shutil.rmtree(out)
shutil.copytree(src, out, symlinks=False)
writable(out)

MOTOR = 'target_os = "motor"'
BOTH = 'any(target_os = "motor", target_os = "cubit")'
changed = 0
for crate in ("std", "core", "alloc", "panic_abort", "unwind", "std_detect"):
    for root, _, files in os.walk(os.path.join(out, crate)):
        for name in files:
            if not name.endswith(".rs"):
                continue
            p = os.path.join(root, name)
            s = open(p, encoding="utf-8").read()
            if MOTOR in s:
                s = s.replace(MOTOR, BOTH)
                open(p, "w", encoding="utf-8").write(s)
                changed += 1

p = os.path.join(out, "std", "build.rs")
s = open(p).read()
s = s.replace('|| target_os == "motor"', '|| target_os == "motor"\n        || target_os == "cubit"', 1)
open(p, "w").write(s)

p = os.path.join(out, "std", "Cargo.toml")
s = open(p).read()
s = s.replace("[target.'cfg(target_os = \"motor\")'.dependencies]",
              "[target.'cfg(any(target_os = \"motor\", target_os = \"cubit\"))'.dependencies]", 1)
# CuBit is not a target rustc knows; accept its cfg value.
s = re.sub(r"(check-cfg\s*=\s*\[)", lambda m: m.group(1) + "\n    'cfg(target_os, values(\"cubit\"))',", s, count=1)
open(p, "w").write(s)

p = os.path.join(out, "Cargo.toml")
s = open(p).read()
fork = os.path.relpath(os.path.join(here, "moto-rt-cubit"), out)
if "[patch.crates-io]" in s:
    s = s.replace("[patch.crates-io]", f'[patch.crates-io]\nmoto-rt = {{ path = "{fork}" }}', 1)
else:
    s += f'\n[patch.crates-io]\nmoto-rt = {{ path = "{fork}" }}\n'
open(p, "w").write(s)

print(f"std-src prepared at {out} ({changed} files take Motor's arms for CuBit)")
