#!/usr/bin/env bash
# Run under shared build lock and nix develop; never changes production staging.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
mode=${1:-lifetime}
case "$mode" in
    lifetime|capacity) ;;
    *) echo 'Expected lifetime or capacity' >&2; exit 2 ;;
esac
task_dir=$(mktemp -d "$root/tests/grant-storage/build/lifetime.XXXXXX")
mkdir -p "$task_dir/obj" "$task_dir/tmp" "$task_dir/cpio" "$task_dir/iso/boot/grub"
export TMPDIR="$task_dir/tmp"
echo "Grant lifetime evidence: $task_dir"
(
    cd kernel
    alr exec -- gprbuild -p -P ../tests/grant-storage/native/lifetime.gpr \
        -XLIFETIME_BUILD="$task_dir/obj" -XLIFETIME_MAIN="$mode.adb"
)
cp "$task_dir/obj/devmgr.svc" "$task_dir/cpio/devmgr.svc"
(
    cd "$task_dir/cpio"
    printf '%s\n' devmgr.svc | cpio -o -H newc > "$task_dir/iso/boot/initrd.img"
)
cp kernel/cubit_kernel "$task_dir/iso/boot/cubit_kernel"
cp tests/intel-gpu/native/demand-grub.cfg "$task_dir/iso/boot/grub/grub.cfg"
sha256sum "$task_dir/iso/boot/cubit_kernel" "$task_dir/obj/devmgr.svc" > "$task_dir/input.sha256"
grub-mkrescue -o "$task_dir/demand.iso" "$task_dir/iso"
python3 tests/intel-gpu/native/run-demand.py "$task_dir" "$mode"
