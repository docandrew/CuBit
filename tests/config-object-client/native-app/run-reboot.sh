#!/usr/bin/env bash
# Nix + coordination/build.lock required. New output directory, disposable disks.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo 'Usage: run-reboot.sh NEW_RESULTS_DIRECTORY' >&2
    exit 2
fi
results=$(realpath -m "$1")
mkdir "$results"
make -C kernel config procmgr clock ccl-manifest
bash userspace/services/config-storage/build.sh
bash tests/config-object-client/native-app/build.sh
# TCG works independently of host CPU identity/entropy restrictions. A separate
# KVM run exercises hardware-assisted execution of the same public protocol.
bash tests/headless/run.sh --test config-objects --accel tcg,thread=multi \
    --timeout 40 --serial "$results/create.serial" --config-export "$results/create"
bash tests/headless/run.sh --test config-objects-reopen --accel tcg,thread=multi \
    --timeout 40 --serial "$results/reopen.serial" --disk "$results/create/disk.img" \
    --config-export "$results/reopen"
echo "CONFIG-OBJECTS: read-only recovery across two independent boots PASS: $results"
