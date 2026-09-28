#!/usr/bin/env bash
# Caller must hold coordination/build.lock and enter the Nix environment.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo 'Usage: run-nvme-profile.sh NEW_RESULTS_DIRECTORY' >&2
    exit 2
fi
results=$(realpath -m "$1")
if [ -e "$results" ]; then
    echo 'Results directory must not already exist.' >&2
    exit 2
fi
restore() {
    result=$?
    trap - EXIT
    # Also restore boot artifacts: rebuilding only nvme.drv would leave the
    # diagnostic driver inside the last headless initrd/ISO.
    CUBIT_NVME_IO_PROFILE=off make -C kernel nvme initrd iso || result=1
    bash tests/config-turso/native/build.sh --features turso || result=1
    exit "$result"
}
trap restore EXIT
export CUBIT_NVME_IO_PROFILE=on
make -C kernel nvme
bash tests/config-turso/native/run-sql-profile.sh "$results"
sha256sum userspace/services/nvme/nvme.adb \
    userspace/services/nvme/nvme_drv.gpr \
    kernel/isodir/boot/nvme.drv >> "$results/environment.txt"
python3 tests/config-turso/native/check-nvme-waits.py "$results/profile.serial" \
    | tee "$results/nvme-waits.md"
