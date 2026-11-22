#!/usr/bin/env bash
# Nix and coordination/build.lock required for the ENTIRE build/test window.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo 'Usage: run-sql-profile.sh NEW_RESULTS_DIRECTORY' >&2
    exit 2
fi
results=$(realpath -m "$1")
mkdir "$results"
make -C kernel filesystem ccl-manifest
bash tests/config-turso/native/build.sh --features sql-bench
{
    uname -a
    qemu-system-x86_64 --version
    lscpu
    rustc --version
    git rev-parse HEAD
    sha256sum tests/config-turso/src/transport_metrics.rs \
        tests/config-turso/src/native_io.rs tests/config-turso/src/lib.rs \
        tests/config-turso/native/src/sql_benchmark.rs \
        tests/config-turso/target/native/turso-native-probe.app \
        userspace/lib/storage/native/native_storage.adb \
        userspace/services/filesystem/ext2.adb
    echo 'Store-level diagnostic, 129 growing-WAL commits; 4-vCPU KVM host CPU; unpinned.'
} > "$results/environment.txt"
QEMU_CPU_MODEL=host bash tests/headless/run.sh --test turso-native \
    --accel kvm --cpus 4 --timeout 120 --serial "$results/profile.serial" \
    --turso-export "$results/validated" > "$results/headless.log" 2>&1
python3 tests/config-turso/native/check-sql-profile.py \
    "$results/profile.serial" "$results/validated/disk.img" | tee "$results/report.md"
# Leave the ordinary smoke-test artifact as the default, not an instrumented one.
bash tests/config-turso/native/build.sh --features turso
