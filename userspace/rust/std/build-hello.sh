#!/usr/bin/env bash
# Build the std test program for CuBit (docs/rust-std.md). Run in the Nix shell
# from the repository root; writes userspace/rust/build/rust-std-hello.app.
set -euo pipefail
cd "$(dirname "$0")"
R=$(realpath ..)
B=$R/build
mkdir -p "$B/std-hello-manifest"
../../ccl/build/manifest/ccl-manifest ../../ccl/catalogs/native-runtime-services.ccl \
    hello/manifest.ccl > "$B/std-hello-manifest/manifest.S"
as --64 "$B/std-hello-manifest/manifest.S" -o "$B/std-hello-manifest/manifest.o"
(
    cd hello
    CARGO_TARGET_DIR="$B/std-hello" \
    RUSTFLAGS="-C link-arg=-T$(realpath ../../../c/link.ld) -C link-arg=-zstack-size=1048576 -C link-arg=$B/std-hello-manifest/manifest.o" \
    bash ../cargo-cubit.sh sh -c 'cargo build --offline -Zbuild-std=std,panic_abort \
        -Zjson-target-spec --target "$CUBIT_RUST_TARGET" --release'
)
cp "$B/std-hello/x86_64-unknown-cubit/release/rust-std-hello" "$B/rust-std-hello.app"
echo "built $B/rust-std-hello.app"
