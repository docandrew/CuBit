#!/usr/bin/env bash
# Build the processes test programs (docs/process-arguments.md) into $1
# (default kernel/isodir/boot): spawn-check (C parent), args-check (C
# child), ada-args-check (Ada.Command_Line) and rust-args-check (Rust std
# over the libc), and delegate-check (Ada launcher handing a child a place). Run in the Nix shell, under the shared build lock, after
# `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"
catalog=$repo/userspace/ccl/catalogs/native-runtime-services.ccl
manifest_tool=$repo/userspace/ccl/build/manifest/ccl-manifest

manifest() {   # name source.ccl -> $b/name-manifest.o
    "$manifest_tool" "$catalog" "$2" > "$b/$1-manifest.S"
    as --64 "$b/$1-manifest.S" -o "$b/$1-manifest.o"
}

manifest args-check "$here/args-manifest.ccl"
manifest spawn-check "$here/spawn-manifest.ccl"
"$repo/userspace/libc/cubit-cc" -O2 -g -Wall -o "$out/args-check.app" \
    "$here/args-check.c" --manifest "$b/args-check-manifest.o"
manifest greedy-check "$here/greedy-manifest.ccl"
"$repo/userspace/libc/cubit-cc" -O2 -g -Wall -o "$out/greedy-check.app" \
    "$here/args-check.c" --manifest "$b/greedy-check-manifest.o"
# INTERIM: the launch table comes from launch-table.py until the manifest
# form (may-launch ...) exists; see that script.
python3 "$here/launch-table.py" args-check.app ada-args-check.app \
    rust-args-check.app greedy-check.app no-such-program.app \
    > "$b/spawn-check-launch.S"
as --64 "$b/spawn-check-launch.S" -o "$b/spawn-check-launch.o"
"$repo/userspace/libc/cubit-cc" -O2 -g -Wall -o "$out/spawn-check.app" \
    "$here/spawn-check.c" "$b/spawn-check-launch.o" \
    --manifest "$b/spawn-check-manifest.o"

mkdir -p "$here/ada/build/generated"
"$manifest_tool" "$catalog" "$here/ada/manifest.ccl" \
    --ada-output "$here/ada/build/generated/ccl_manifest_bindings.ads" \
    > "$here/ada/build/manifest.S"
as --64 "$here/ada/build/manifest.S" -o "$here/ada/build/manifest.o"
# gprbuild does not track crt0.o or the runtime library: always relink.
rm -f "$here/ada/build/ada-args-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild -q -P "$here/ada/ada_args_check.gpr")
cp "$here/ada/build/ada-args-check.app" "$out/ada-args-check.app"

# delegate-check (Ada launcher): delegated places through CuBit.Launching.
mkdir -p "$here/delegate/build"
"$manifest_tool" "$catalog" "$here/delegate/manifest.ccl" \
    > "$here/delegate/build/manifest.S"
as --64 "$here/delegate/build/manifest.S" -o "$here/delegate/build/manifest.o"
python3 "$here/launch-table.py" args-check.app > "$here/delegate/build/launch.S"
as --64 "$here/delegate/build/launch.S" -o "$here/delegate/build/launch.o"
rm -f "$here/delegate/build/delegate-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild -q -P "$here/delegate/delegate_check.gpr")
cp "$here/delegate/build/delegate-check.app" "$out/delegate-check.app"

manifest rust-args-check "$here/rust/manifest.ccl"
# Cargo does not see libc.a or the start code change: always relink.
touch "$here/rust/src/main.rs"
(
    cd "$here/rust"
    CARGO_TARGET_DIR="$b/rust" CUBIT_MANIFEST_O="$b/rust-args-check-manifest.o" \
    bash "$repo/userspace/rust/std/unix/cargo-cubit-unix.sh" sh -c \
        'cargo build --offline -Zbuild-std=std,panic_abort -Zjson-target-spec \
             --target "$CUBIT_RUST_TARGET" --release'
)
cp "$b/rust/x86_64-unknown-cubit/release/rust-args-check" "$out/rust-args-check.app"
echo "processes test programs: $out"
