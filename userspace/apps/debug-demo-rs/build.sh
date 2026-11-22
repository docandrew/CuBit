#!/usr/bin/env bash
# Build the debugger's Rust demo target (DBG-001) with CuBit's std
# (userspace/rust/std/README.md), as build-hello.sh builds rust-std-hello.
# Run in the Nix shell from anywhere; writes
# userspace/rust/build/debug-demo-rs.app (unstripped: image staging splits
# the debug file by build-id).
set -euo pipefail
cd "$(dirname "$0")"
R=$(realpath ../../rust)
B=$R/build
mkdir -p "$B/debug-demo-rs-manifest"
../../ccl/build/manifest/ccl-manifest ../../ccl/catalogs/native-runtime-services.ccl \
    manifest.ccl > "$B/debug-demo-rs-manifest/manifest.S"
as --64 "$B/debug-demo-rs-manifest/manifest.S" -o "$B/debug-demo-rs-manifest/manifest.o"
CARGO_TARGET_DIR="$B/debug-demo-rs" \
RUSTFLAGS="-C link-arg=-T$(realpath ../../c/link.ld) -C link-arg=-zstack-size=1048576 -C link-arg=--build-id=sha1 -C link-arg=$B/debug-demo-rs-manifest/manifest.o -C force-unwind-tables=yes" \
bash "$R/std/cargo-cubit.sh" sh -c 'cargo build --offline -Zbuild-std=std,panic_abort \
    -Zjson-target-spec --target "$CUBIT_RUST_TARGET" --release'
cp "$B/debug-demo-rs/x86_64-unknown-cubit/release/debug-demo-rs" "$B/debug-demo-rs.app"
echo "built $B/debug-demo-rs.app"
