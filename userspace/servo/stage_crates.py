#!/usr/bin/env python3
"""Stage CuBit's own crates inside a Penny Servo checkout.

    stage_crates.py <servo-checkout>

The shared UI font crate (bundled faces), a path dependency of Penny, is
copied inside the Servo workspace (cubit-build/, which the fork's Cargo.toml
files name by relative path) with the crates its (optional, native)
dependencies name by relative path, and Penny's cryptography (penny-crypto): cargo hashes a path package's location
into every dependent's metadata, relative to the workspace root only inside
it (servo-cargo.sh). Unchanged files keep their mtimes.
"""
import os
import sys

RUST_CRATES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../rust")
INSIDE = os.path.join(os.path.abspath(sys.argv[1]), "cubit-build")
for crate in ("fonts", "allocator", "cubit", "penny-crypto"):
    source_root = os.path.join(RUST_CRATES, crate)
    names = ["Cargo.toml"] + [name for name in ("build.rs",) if os.path.exists(os.path.join(source_root, name))] + [
        os.path.relpath(os.path.join(d, f), source_root)
        for d, _, files in os.walk(os.path.join(source_root, "src")) for f in files]
    for name in sorted(names):
        destination = os.path.join(INSIDE, crate, name)
        content = open(os.path.join(source_root, name), "rb").read()
        if not os.path.exists(destination) or open(destination, "rb").read() != content:
            os.makedirs(os.path.dirname(destination), exist_ok=True)
            with open(destination, "wb") as output:
                output.write(content)
