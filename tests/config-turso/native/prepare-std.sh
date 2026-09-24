#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
native_dir=$PWD
rust_source="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
test -f "$rust_source/Cargo.lock"
# Each input revision gets a fresh, isolated generated tree. Never edit the
# Nix source, Cargo registry, or a source tree used by normal no_std apps.
source_key=$({ printf '%s\n' "$rust_source"; sha256sum prepare-std.sh std.patch std/*.rs; } | sha256sum | cut -c1-20)
prepared="$(realpath -m "$native_dir/../target/native-std-$source_key")"
if [ ! -f "$prepared/ready" ]; then
    mkdir "$prepared"
    cp -RL "$rust_source" "$prepared/library"
    chmod -R u+w "$prepared/library"
    patch --batch --fuzz=0 -p1 -d "$prepared/library" < std.patch
    cp std/alloc.rs "$prepared/library/std/src/sys/alloc/cubit.rs"
    cp std/error.rs "$prepared/library/std/src/sys/io/error/cubit.rs"
    cp std/random.rs "$prepared/library/std/src/sys/random/cubit.rs"
    cp std/time.rs "$prepared/library/std/src/sys/time/cubit.rs"
    cp std/stdio.rs "$prepared/library/std/src/sys/stdio/cubit.rs"
    touch "$prepared/ready"
fi
export __CARGO_TESTS_ONLY_SRC_ROOT="$prepared/library"
export RUSTC_BOOTSTRAP=1
# This is an explicitly supplied platform port, not upstream's unmodified
# unsupported std. Keep the bootstrap opt-in local to this experiment.
export RUSTC_BOOTSTRAP_SYNTHETIC_TARGET=1
exec "$@"
