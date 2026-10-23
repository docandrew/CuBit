#!/usr/bin/env bash
# Invoke under the shared build lock and nix develop. No production staging.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
root=$(pwd)
test -s kernel/cubit_kernel
task_dir=$(mktemp -d "${TMPDIR:-/tmp}/owned-records-native.XXXXXX")
mkdir -p "$task_dir/tmp" "$task_dir/obj" "$task_dir/cpio" "$task_dir/iso/boot/grub"
export TMPDIR="$task_dir/tmp"
echo "Native owned-record evidence: $task_dir"
(
    cd kernel
    alr exec -- gprbuild $gprbuild_config -p -P ../tests/owned-record-table/native.gpr \
        -XDEMAND_BUILD="$task_dir/obj"
)
cp "$task_dir/obj/devmgr.svc" "$task_dir/cpio/devmgr.svc"
(
    cd "$task_dir/cpio"
    printf '%s\n' devmgr.svc | cpio -o -H newc > "$task_dir/iso/boot/initrd.img"
)
# Use the existing built kernel; record its identity, do not claim source rebuild.
cp kernel/cubit_kernel "$task_dir/iso/boot/cubit_kernel"
cp tests/owned-record-table/grub.cfg "$task_dir/iso/boot/grub/grub.cfg"
sha256sum "$task_dir/iso/boot/cubit_kernel" "$task_dir/obj/devmgr.svc" > "$task_dir/input.sha256"
grub-mkrescue -o "$task_dir/demand.iso" "$task_dir/iso"
python3 tests/owned-record-table/run-native.py "$task_dir"
