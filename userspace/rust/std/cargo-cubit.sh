#!/usr/bin/env bash
# Run a command (normally cargo) with CuBit's Rust std (docs/rust-std.md).
# Usage, in the Nix shell:
#   bash userspace/rust/std/cargo-cubit.sh cargo build --release \
#       --target "$CUBIT_RUST_TARGET" -Zjson-target-spec -Zbuild-std=std,panic_abort
# Exports CUBIT_RUST_TARGET (the target spec) and points -Zbuild-std at the
# patched std sources, prepared once per toolchain and input revision.
set -euo pipefail
std_dir=$(cd "$(dirname "$0")" && pwd)
rust_source="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
test -f "$rust_source/Cargo.lock"
key=$({ printf '%s\n' "$rust_source"
        cat "$std_dir/prepare-std.py"
        find "$std_dir/moto-rt-cubit" -type f -name '*.rs' -o -name Cargo.toml -type f |
            sort | xargs cat; } | sha256sum | cut -c1-20)
prepared=$(realpath -m "$std_dir/../build/std-src-$key")
if [ ! -f "$prepared/ready" ]; then
    python3 "$std_dir/prepare-std.py" "$prepared/library" >/dev/null
    touch "$prepared/ready"
fi
export __CARGO_TESTS_ONLY_SRC_ROOT="$prepared/library"
export RUSTC_BOOTSTRAP=1
# -Zbuild-std builds run --offline, so std's own crates (hashbrown, dlmalloc,
# moto-rt, ...) must already be in Cargo's cache. A fresh CARGO_HOME has none:
# fetch them once, from the toolchain's pinned Cargo.lock (the patched copy's
# lock differs and its tree is partial, so fetch from the pristine source).
if ! cargo fetch --locked --offline --manifest-path "$rust_source/Cargo.toml" >/dev/null 2>&1; then
    cargo fetch --locked --quiet --manifest-path "$rust_source/Cargo.toml"
fi
export CUBIT_RUST_TARGET="$std_dir/x86_64-unknown-cubit.json"
exec "$@"
