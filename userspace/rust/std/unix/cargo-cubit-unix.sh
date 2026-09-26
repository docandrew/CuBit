#!/usr/bin/env bash
# Run a command (normally cargo) with CuBit's Unix-family Rust std: std over
# the CuBit libc (userspace/libc). Exports CUBIT_RUST_TARGET; links through
# userspace/libc/cubit-cc (start code, libc, link layout, stack contract).
# Pass a CCL manifest object in CUBIT_MANIFEST_O to embed it.
set -euo pipefail
unix_dir=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$unix_dir/../../../.." && pwd)
rust_source="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
test -f "$repo/userspace/libc/build/sysroot/lib/libc.a" || bash "$repo/userspace/libc/build.sh" >/dev/null
key=$({ printf '%s\n' "$rust_source"; cat "$unix_dir/prepare-std-unix.py"; } | sha256sum | cut -c1-20)
prepared=$(realpath -m "$repo/userspace/rust/build/std-unix-$key")
if [ ! -f "$prepared/ready" ]; then
    python3 "$unix_dir/prepare-std-unix.py" "$prepared" >/dev/null
    touch "$prepared/ready"
fi
export __CARGO_TESTS_ONLY_SRC_ROOT="$prepared/library"
export RUSTC_BOOTSTRAP=1
export CUBIT_RUST_TARGET="$unix_dir/x86_64-unknown-cubit.json"
linkflags="-C linker=$repo/userspace/libc/cubit-cc"
if [ -n "${CUBIT_MANIFEST_O:-}" ]; then
    linkflags="$linkflags -C link-arg=--manifest -C link-arg=$CUBIT_MANIFEST_O"
fi
export RUSTFLAGS="${RUSTFLAGS:-} $linkflags"
exec "$@"
