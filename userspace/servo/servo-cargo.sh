#!/usr/bin/env bash
# Run cargo on a Servo checkout for the Unix-family x86_64-unknown-cubit
# target (docs/servo-port.md), without editing Servo's files.
#   SERVO_DIR=<checkout> [CARGO_HOME=...] servo-cargo.sh check -p servo
# - std: userspace/rust/std/unix (Rust's Unix std over the CuBit libc);
# - crates that only describe the C ABI (libc) are cubitized copies,
#   patched in with cargo --config;
# - C/C++ build scripts use the CuBit libc wrappers;
# - features: no JIT, no multiprocess/sandbox, no clipboard, WebGPU,
#   WebGL or WebXR; baked-in resources and bundled FreeType.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
servo=${SERVO_DIR:?set SERVO_DIR to a Servo checkout}
registry=$(ls -d "${CARGO_HOME:-$HOME/.cargo}"/registry/src/index.crates.io-*/ | head -1)
patched=$repo/userspace/rust/build/servo-crates
mkdir -p "$patched"
patch_args=()
# One patch key per fixed crate version (several versions of one crate,
# e.g. getrandom 0.2/0.3/0.4, are patched side by side).
while IFS='=' read -r name path; do
    key=$(basename "$path" | tr '.' '_')
    patch_args+=(--config "patch.crates-io.$key.path=\"$path\""
                 --config "patch.crates-io.$key.package=\"$name\"")
done < <(python3 "$here/crate_fixes.py" "$registry" "$patched")
python3 "$here/patch_servo.py" "$servo" >/dev/null


# Servo's incremental caches run to many GB; builds here are whole-tree.
export CARGO_INCREMENTAL=${CARGO_INCREMENTAL:-0}

export CC_x86_64_unknown_cubit=$repo/userspace/libc/cubit-cc
export CXX_x86_64_unknown_cubit=$repo/userspace/libc/cubit-c++
export AR_x86_64_unknown_cubit=ar
# musl exposes POSIX/GNU interfaces (sigaction, ...) only on request; C
# builds that assume a known OS's defaults (aws-lc's strict C11) need it.
export CFLAGS_x86_64_unknown_cubit="-D_GNU_SOURCE"
export CPP_x86_64_unknown_cubit="$repo/userspace/libc/cubit-cc -E"
# Mozilla assembles through a compiler driver.
export AS_x86_64_unknown_cubit=$repo/userspace/libc/cubit-cc
# Host tools for build systems that compile helpers (SpiderMonkey).
export HOST_CC=gcc HOST_CXX=g++
# SpiderMonkey's configure also wants LLVM's binary tools (llvm-objdump).
# TODO: provide these from the flake's dev shell.
llvm=$(nix build --inputs-from "$repo" nixpkgs#llvm --no-link --print-out-paths | head -1)
export PATH="$PATH:$llvm/bin"
# bindgen (SpiderMonkey's bindings) needs libclang.
export LIBCLANG_PATH=$(nix build --inputs-from "$repo" nixpkgs#llvmPackages.libclang.lib --no-link --print-out-paths | head -1)/lib
# ... and the CuBit target's headers: libstdc++ from the musl cross g++,
# clang's own builtin headers (not gcc's), then the CuBit libc headers.
cross_gxx=$(cat "$repo/userspace/libc/build/cross-gcc")/bin/x86_64-unknown-linux-musl-g++
cxx_includes=$(echo | "$cross_gxx" -E -x c++ - -v 2>&1 |
    sed -n '/#include <...> search starts here/,/End of search list/p' |
    grep -E 'include/c\+\+' | sed 's/^ */-isystem /' | tr '\n' ' ')
clang_resource=$(ls -d "$LIBCLANG_PATH"/clang/*/include | head -1)
export BINDGEN_EXTRA_CLANG_ARGS_x86_64_unknown_cubit="--target=x86_64-unknown-linux-musl -nostdinc $cxx_includes -isystem $clang_resource -isystem $repo/userspace/libc/build/sysroot/include"
# getrandom: the libc's getrandom(2), the one place randomness comes from
# (RDRAND there today; CuBit's entropy service later).
# mio: its poll(2) selector and pipe waker (libc objects over CuBit), not
# epoll or eventfd.
export RUSTFLAGS="${RUSTFLAGS:-} --cfg getrandom_backend=\"linux_getrandom\" --cfg mio_unsupported_force_poll_poll --cfg mio_unsupported_force_waker_pipe"
command=$1; shift
cd "$servo"
exec bash "$repo/userspace/rust/std/unix/cargo-cubit-unix.sh" sh -c '
    command=$1; shift
    exec cargo "$command" "$@" --target "$CUBIT_RUST_TARGET" -Zjson-target-spec \
        -Zbuild-std=std,panic_abort' sh "$command" "${patch_args[@]}" "$@"
