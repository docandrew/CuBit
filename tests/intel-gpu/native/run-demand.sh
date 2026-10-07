#!/usr/bin/env bash
# Invoke under the shared build lock and nix develop. No production staging.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
test -s kernel/cubit_kernel
mode=${1:-memory}
case "$mode" in
    memory) main_source=demand_backing_check.adb ;;
    ipc) main_source=allocation_ipc_check.adb ;;
    views) main_source=view_retention_check.adb ;;
    images) main_source=image_provider_check.adb ;;
    mappings) main_source=mapping_growth_check.adb ;;
    metadata) main_source=update_metadata_check.adb ;;
    *) echo 'Expected memory, ipc, views, images, mappings or metadata' >&2; exit 2 ;;
esac
task_dir=$(mktemp -d "$root/tests/intel-gpu/demand-backing.XXXXXX")
mkdir -p "$task_dir/tmp" "$task_dir/obj" "$task_dir/cpio" "$task_dir/iso/boot/grub"
export TMPDIR="$task_dir/tmp"
echo "Native demand evidence: $task_dir"
(
    cd kernel
    alr exec -- gprbuild -p -P ../tests/intel-gpu/native/demand.gpr \
        -XDEMAND_BUILD="$task_dir/obj" -XDEMAND_MAIN="$main_source"
)
cp "$task_dir/obj/devmgr.svc" "$task_dir/cpio/devmgr.svc"
(
    cd "$task_dir/cpio"
    printf '%s\n' devmgr.svc | cpio -o -H newc > "$task_dir/iso/boot/initrd.img"
)
# Use the existing built kernel; record its identity, do not claim source rebuild.
cp kernel/cubit_kernel "$task_dir/iso/boot/cubit_kernel"
cp tests/intel-gpu/native/demand-grub.cfg "$task_dir/iso/boot/grub/grub.cfg"
sha256sum "$task_dir/iso/boot/cubit_kernel" "$task_dir/obj/devmgr.svc" > "$task_dir/input.sha256"
grub-mkrescue -o "$task_dir/demand.iso" "$task_dir/iso"
python3 tests/intel-gpu/native/run-demand.py "$task_dir" "$mode"
