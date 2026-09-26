#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
service_dir=$PWD
repo_dir=$(realpath ../../..)
build_dir="$service_dir/build"
probe_dir="$repo_dir/tests/config-turso/native"
mkdir -p "$build_dir"
cd "$repo_dir/kernel"
alr exec -- gprbuild -p -P ../userspace/allocator/allocator_native.gpr
alr exec -- gprbuild -p -P ../userspace/runtime/user_runtime.gpr
alr exec -- gprbuild -p -P ../userspace/services/config-storage/worker_host.gpr
cd "$service_dir"
bash "$probe_dir/prepare-turso.sh"
"$repo_dir/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo_dir/userspace/ccl/catalogs/native-runtime-services.ccl" \
    manifest.ccl --rust-output "$build_dir/bindings.rs" > "$build_dir/manifest.S"
as --64 "$build_dir/manifest.S" -o "$build_dir/manifest.o"
export CUBIT_BINDINGS_DIR="$build_dir"
export CARGO_TARGET_DIR="$build_dir/cargo"
export RUSTFLAGS='--cfg getrandom_backend="rdrand" --cfg polyval_force_soft --cfg aes_force_soft -C code-model=small -C relocation-model=static'
bash "$repo_dir/userspace/rust/std/cargo-cubit.sh" cargo build --manifest-path "$service_dir/Cargo.toml" \
    --locked --offline --release --target "$repo_dir/userspace/rust/std/x86_64-unknown-cubit.json" -Zjson-target-spec \
    -Zbuild-std=std,panic_abort -Zbuild-std-features=compiler-builtins-mem -j4
ld -nostdlib -static -z noexecstack -z stack-size=16777216 \
    -T "$repo_dir/userspace/c/link.ld" \
    --start-group "$build_dir/cargo/x86_64-unknown-cubit/release/libcubit_config_storage.a" \
    "$repo_dir/userspace/allocator/build/native-lib/libcubit_allocator.a" \
    "$build_dir/ada-lib/libconfig_storage_host.a" \
    "$repo_dir/userspace/runtime/adalib/libgnat-user.a" \
    --end-group "$build_dir/manifest.o" -o "$build_dir/config-storage.debug.svc"
objcopy --strip-debug "$build_dir/config-storage.debug.svc" "$build_dir/config-storage.svc"
