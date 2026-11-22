#!/usr/bin/env bash
# Build cubitshell for x86_64-unknown-linux-gnu: the Linux-hosted baseline of
# the WEB-001 performance breakdown (a Linux demonstration, not CuBit).
# The same Servo checkout, lock, crate fixes and Servo patches as the CuBit
# build (servo-cargo.sh), the same release profile and SWGL, no JIT; its own
# target directory. Differences, by design: Linux's own threads, futexes,
# epoll/eventfd (no forced poll(2)/pipe waker cfgs), glibc malloc, dynamic
# linking, a prebuilt std, and no media feature.
# Output: $CARGO_TARGET_DIR/x86_64-unknown-linux-gnu/release/cubitshell.
#   build-cubitshell-hosted.sh [cargo args]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
work=$repo/userspace/rust/build/servo-work
export SERVO_DIR=${SERVO_DIR:-$work/servo}
export CARGO_HOME=${CARGO_HOME:-$work/cargo-home}
export CARGO_TARGET_DIR=${CARGO_HOSTED_TARGET_DIR:-$work/target-hosted}
servo=$SERVO_DIR
bash "$here/fork-checkout.sh" servo "$servo"
# Penny's build time is its Servo fork commit's (servo's devtools build ID
# and anything else reading SOURCE_DATE_EPOCH), not the CuBit commit's
# kernel/Makefile exports: an unrelated CuBit commit leaves Penny's bytes
# unchanged (BLD-004).
SOURCE_DATE_EPOCH=$(git -C "$servo" log -1 --format=%ct HEAD)
export SOURCE_DATE_EPOCH
downloads=$repo/userspace/rust/build/servo-crate-downloads
patched=$servo/cubit-build/crates
mkdir -p "$downloads" "$patched"
fixed=$(python3 "$here/crate_fixes.py" "$downloads" "$patched")
patch_args=()
while IFS='=' read -r name path; do
    key=$(basename "$path" | tr '.' '_')
    patch_args+=(--config "patch.crates-io.$key.path=\"$path\""
                 --config "patch.crates-io.$key.package=\"$name\"")
done <<< "$fixed"
python3 "$here/stage_crates.py" "$servo"
# Penny's cryptography (WEB-004) for Linux: the runtime is encapsulated.
(cd "$repo/kernel" && PENNY_CRYPTO_TARGET=host alr exec -- gprbuild -p \
    -P ../userspace/servo/crypto/penny_crypto.gpr -j8)
export PENNY_CRYPTO_LIB_DIR="$here/crypto/build/host/lib"
cp "$here/Cargo.lock" "$servo/Cargo.lock"
export CARGO_INCREMENTAL=${CARGO_INCREMENTAL:-0}

# SWGL's CuBit diagnostics (crate_fixes.py) write slow draws through
# cubit_debug_write; on Linux that is stderr, so both builds compile the
# same SWGL source.
shim=$CARGO_TARGET_DIR/cubit-debug-shim
mkdir -p "$shim/cubit"
cat > "$shim/cubit/debug.h" <<'EOF'
/* Linux-hosted stand-in for CuBit's debug channel (WEB-001 baseline). */
#pragma once
#include <stddef.h>
#include <unistd.h>
static inline void cubit_debug_write(const char *data, size_t length) {
    ssize_t ignored = write(2, data, length);
    (void)ignored;
}
EOF
export CFLAGS_x86_64_unknown_linux_gnu="-I$shim"
export CXXFLAGS_x86_64_unknown_linux_gnu="-I$shim"
# Nix's gcc wrapper adds hardening (stack protector, fortify, PIE,
# -Werror=format-security) that the CuBit cross build has not: off, so both
# compile C/C++ alike.
export NIX_HARDENING_ENABLE=""
# Mozilla assembles through a compiler driver (as servo-cargo.sh does).
export AS_x86_64_unknown_linux_gnu=gcc
export MOZJS_FROM_SOURCE=1
export HOST_CC=gcc HOST_CXX=g++
llvm=$(nix build --inputs-from "$repo" nixpkgs#llvm --no-link --print-out-paths | head -1)
export PATH="$PATH:$llvm/bin"
export LIBCLANG_PATH=$(nix build --inputs-from "$repo" nixpkgs#llvmPackages.libclang.lib --no-link --print-out-paths | head -1)/lib
# bindgen (SpiderMonkey) with libclang's own builtin headers, never the
# host distribution's clang (as servo-cargo.sh does for CuBit).
clang_resource=$(dirname "$(ls -d "$LIBCLANG_PATH"/clang/*/include | head -1)")
export BINDGEN_EXTRA_CLANG_ARGS_x86_64_unknown_linux_gnu="-resource-dir=$clang_resource"
cd "$servo"
exec cargo build --frozen --release -p cubitshell --features bundled \
    --target x86_64-unknown-linux-gnu "${patch_args[@]}" "$@"
