#!/usr/bin/env bash
# Nix + coordination/build.lock required. Never mutate the user's base disk.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo 'Usage: run-benchmark.sh NEW_RESULTS_DIRECTORY' >&2
    exit 2
fi
results=$(realpath -m "$1")
mkdir "$results"
make -C kernel config procmgr clock filesystem ccl-manifest
bash userspace/services/config-storage/build.sh
bash tests/config-object-client/native-app/build.sh
{
    uname -a
    qemu-system-x86_64 --version
    lscpu
    rustc --version
    git rev-parse HEAD
    sha256sum tests/config-object-client/native-app/benchmark.adb \
        tests/config-object-client/native-app/build/config-objects-benchmark.app \
        userspace/lib/config/config_object_client.adb \
        kernel/isodir/boot/config.svc \
        kernel/isodir/boot/filesystem.svc \
        userspace/services/config-storage/build/config-storage.svc \
        userspace/lib/storage/native/native_storage.adb \
        userspace/lib/storage/storage_channel.ads \
        userspace/lib/storage/storage_channel.adb \
        tests/config-turso/src/native_io.rs
    echo 'KVM host CPU, 4 vCPUs, 128 MiB; unpinned; host load uncontrolled.'
    echo 'Three sequential boots with independent disposable ext2 disks.'
} > "$results/environment.txt"
for run in 1 2 3; do
    QEMU_CPU_MODEL=host bash tests/headless/run.sh --test config-objects-benchmark \
        --accel kvm --cpus 4 --timeout 120 --serial "$results/run-$run.serial" \
        > "$results/run-$run.log" 2>&1
done
python3 tests/config-object-client/native-app/report-benchmark.py "$results"/*.serial \
    | tee "$results/report.md"
