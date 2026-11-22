#!/usr/bin/env bash
# Run in Nix while holding coordination/build.lock. Requires existing base disk.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo 'Usage: run-benchmark.sh NEW_RESULTS_DIRECTORY' >&2
    exit 2
fi
results=$(realpath -m "$1")
mkdir "$results"
make -C kernel filesystem ccl-manifest
bash tests/config-turso/native/build.sh --features bench
{
    uname -a
    qemu-system-x86_64 --version
    lscpu
    rustc --version
    git rev-parse HEAD
    sha256sum userspace/lib/storage/storage_channel.ads \
        userspace/lib/storage/storage_channel.adb \
        userspace/lib/storage/native/native_storage.adb \
        userspace/lib/storage/native/bridge.rs \
        tests/config-turso/src/native_io.rs \
        tests/config-turso/src/io_workload.rs \
        tests/config-turso/target/native/turso-native-probe.app
    echo 'Guest: KVM, host CPU model, 4 vCPUs, 128 MiB; unpinned; host load uncontrolled.'
    echo 'Three sequential runs; fresh disposable disk per run; warm initialized scratch file.'
} > "$results/environment.txt"
for run in 1 2 3; do
    QEMU_CPU_MODEL=host bash tests/headless/run.sh --test turso-native \
        --accel kvm --cpus 4 --timeout 120 --serial "$results/run-$run.serial" \
        > "$results/run-$run.log" 2>&1
done
python3 tests/config-turso/native/report-benchmark.py "$results"/*.serial \
    | tee "$results/report.md"
