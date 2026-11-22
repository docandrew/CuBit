#!/usr/bin/env bash
# Run cargo on a Servo checkout for the Unix-family x86_64-unknown-cubit
# target (docs/servo-port.md), without editing Servo's files by hand.
#   SERVO_DIR=<checkout> [CARGO_HOME=...] servo-cargo.sh fetch
#   SERVO_DIR=<checkout> [CARGO_HOME=...] servo-cargo.sh build -p cubitshell ...
#   SERVO_DIR=<checkout> [CARGO_HOME=...] servo-cargo.sh update-lock
# Reproducible inputs (BLD-004):
# - Servo is Penny's fork, checked out at its pinned commit (FORKS,
#   fork-checkout.sh): CuBit's Servo changes are commits there, and the
#   checkout must be exactly that commit;
# - the lock file is ours (userspace/servo/Cargo.lock): `fetch` fills
#   CARGO_HOME with exactly its crates (cargo fetch --locked), every other
#   command runs --frozen (that lock, no network);
# - crates we fix are fetched by SHA-256 (crate_fixes.py);
# - checkout, CARGO_HOME and target paths are remapped in the output.
# `update-lock` is the one command that may change the lock: it resolves
# the workspace from our lock (or Servo's own, the first time), adding only
# what is new, and writes the result back to userspace/servo/Cargo.lock.
# Target setup:
# - std: userspace/rust/std/unix (Rust's Unix std over the CuBit libc);
# - crates that only describe the C ABI (libc) are cubitized copies,
#   patched in with cargo --config;
# - C/C++ build scripts use the CuBit libc wrappers;
# - features: no JIT, no multiprocess/sandbox, no clipboard, WebGPU,
#   WebGL or WebXR; Web Crypto on; baked-in resources and bundled FreeType.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
servo=${SERVO_DIR:?set SERVO_DIR to a Servo checkout (created if missing)}
export CARGO_HOME=${CARGO_HOME:-$HOME/.cargo}
command=${1:?usage: servo-cargo.sh fetch|update-lock|<cargo command> [args]}; shift
lock=$here/Cargo.lock

bash "$here/fork-checkout.sh" servo "$servo"
# Penny's build time is its Servo fork commit's (servo's devtools build ID
# and anything else reading SOURCE_DATE_EPOCH), not the CuBit commit's
# kernel/Makefile exports: an unrelated CuBit commit leaves Penny's bytes
# unchanged (BLD-004).
SOURCE_DATE_EPOCH=$(git -C "$servo" log -1 --format=%ct HEAD)
export SOURCE_DATE_EPOCH

downloads=$repo/userspace/rust/build/servo-crate-downloads
# Every path package (fixed crates, cubit-fonts) lives inside the Servo
# workspace: cargo hashes a path package's location into every dependent's
# metadata, relative to the workspace root when it is inside it and
# absolutely otherwise, which would make the output depend on the
# checkout's path (std: cargo-cubit-unix.sh).
patched=$servo/cubit-build/crates
mkdir -p "$downloads" "$patched"
fixed=$(python3 "$here/crate_fixes.py" "$downloads" "$patched")
patch_args=()
# One patch key per fixed crate version (several versions of one crate,
# e.g. getrandom 0.2/0.3/0.4, are patched side by side).
while IFS='=' read -r name path; do
    key=$(basename "$path" | tr '.' '_')
    patch_args+=(--config "patch.crates-io.$key.path=\"$path\""
                 --config "patch.crates-io.$key.package=\"$name\"")
done <<< "$fixed"
python3 "$here/stage_crates.py" "$servo"

case $command in
    update-lock)
        if [ -f "$lock" ]; then
            cp "$lock" "$servo/Cargo.lock"
        else
            git -C "$servo" show HEAD:Cargo.lock > "$servo/Cargo.lock"
        fi
        (cd "$servo" && cargo metadata --format-version 1 "${patch_args[@]}" >/dev/null)
        if grep -q '^\[\[patch.unused\]\]' "$servo/Cargo.lock"; then
            echo "servo-cargo.sh: a crate_fixes.py version is not the locked one:" >&2
            sed -n '/^\[\[patch.unused\]\]/,$p' "$servo/Cargo.lock" >&2
            exit 1
        fi
        cp "$servo/Cargo.lock" "$lock"
        echo "servo-cargo.sh: wrote $lock"
        exit 0
        ;;
esac
cp "$lock" "$servo/Cargo.lock"

# Servo's incremental caches run to many GB; builds here are whole-tree.
export CARGO_INCREMENTAL=${CARGO_INCREMENTAL:-0}

target_dir=${CARGO_TARGET_DIR:-$servo/target}
# Checkout-independent output: the paths that differ between checkouts are
# remapped (the most specific last: the last matching map wins in rustc
# and gcc). Nix store paths are already the same on every host.
prefix_maps=("$repo=/cubit" "$CARGO_HOME=/cargo-home" "$servo=/servo" "$target_dir=/target")
rust_maps="" c_maps=""
for map in "${prefix_maps[@]}"; do
    rust_maps+=" --remap-path-prefix=$map"
    c_maps+=" -ffile-prefix-map=$map"
done
# For C compiled by our own build scripts (cubitshell's media adapters).
export CUBIT_FILE_PREFIX_MAPS=${c_maps# }

export CC_x86_64_unknown_cubit=$repo/userspace/libc/cubit-cc
export CXX_x86_64_unknown_cubit=$repo/userspace/libc/cubit-c++
export AR_x86_64_unknown_cubit=ar
# musl exposes POSIX/GNU interfaces (sigaction, ...) only on request; C
# builds that assume a known OS's defaults (aws-lc's strict C11) need it.
export CFLAGS_x86_64_unknown_cubit="-D_GNU_SOURCE$c_maps"
export CXXFLAGS_x86_64_unknown_cubit="${c_maps# }"
export CPP_x86_64_unknown_cubit="$repo/userspace/libc/cubit-cc -E"
# Mozilla assembles through a compiler driver.
export AS_x86_64_unknown_cubit=$repo/userspace/libc/cubit-cc
# Host tools for build systems that compile helpers (SpiderMonkey).
export HOST_CC=gcc HOST_CXX=g++
# SpiderMonkey without the JIT is always built from source; never let its
# build script try a prebuilt archive download.
export MOZJS_FROM_SOURCE=1
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
export RUSTFLAGS="${RUSTFLAGS:-} --cfg getrandom_backend=\"linux_getrandom\" --cfg mio_unsupported_force_poll_poll --cfg mio_unsupported_force_waker_pipe$rust_maps"
case $command in
    fetch) lock_mode=--locked ;;   # the only command that downloads
    *) lock_mode=--frozen ;;       # the lock as committed, and no network
esac
cd "$servo"
if [ "$command" = fetch ]; then
    # Every platform's crates: cargo build may consult packages that a
    # target-filtered fetch leaves out (gaol). Then std's own (below).
    cargo fetch --locked "${patch_args[@]}"
fi
exec bash "$repo/userspace/rust/std/unix/cargo-cubit-unix.sh" sh -c '
    command=$1; shift
    exec cargo "$command" "$@" --target "$CUBIT_RUST_TARGET" -Zjson-target-spec \
        -Zbuild-std=std,panic_abort' sh "$command" "$lock_mode" "${patch_args[@]}" "$@"
