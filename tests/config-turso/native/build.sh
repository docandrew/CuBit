#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
native_dir=$PWD
repo_dir=$(realpath ../../..)
build_dir=$(realpath -m ../target/native)
mkdir -p "$build_dir"
cd "$repo_dir/kernel"
alr exec -- gprbuild -p -P ../userspace/allocator/allocator_native.gpr
cd "$native_dir"
bash prepare-turso.sh
"$repo_dir/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo_dir/userspace/ccl/catalogs/native-runtime-services.ccl" \
    manifest.ccl --rust-output "$build_dir/bindings.rs" > "$build_dir/manifest.S"
as --64 "$build_dir/manifest.S" -o "$build_dir/manifest.o"
export CUBIT_BINDINGS_DIR="$build_dir"
export CARGO_TARGET_DIR="$build_dir/cargo"
export RUSTFLAGS='--cfg getrandom_backend="rdrand" --cfg polyval_force_soft --cfg aes_force_soft -C code-model=small -C relocation-model=static'
bash prepare-std.sh cargo build --manifest-path Cargo.toml --locked --release \
    --target "$native_dir/x86_64-cubit.json" -Zjson-target-spec -Zbuild-std=std,panic_abort \
    -Zbuild-std-features=compiler-builtins-mem -j4 "$@"
ld -nostdlib -static -z noexecstack -z stack-size=16777216 \
    -T "$repo_dir/userspace/c/link.ld" \
    --start-group "$build_dir/cargo/x86_64-cubit/release/libcubit_turso_native_probe.a" \
    "$repo_dir/userspace/allocator/build/native-lib/libcubit_allocator.a" \
    --end-group "$build_dir/manifest.o" -o "$build_dir/turso-native-probe.debug.app"
# Keep symbols for diagnosis, but do not make procmgr buffer tens of MiB of
# non-loadable Rust debug sections just to load this experimental service.
objcopy --strip-debug "$build_dir/turso-native-probe.debug.app" \
    "$build_dir/turso-native-probe.app"
