#!/usr/bin/env bash
# Invoke under the shared build lock and nix develop. No production staging.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
test -s kernel/cubit_kernel
out=$(mktemp -d "$root/tests/input-pending/build/native.XXXXXX")
mkdir -p "$out/tmp" "$out/obj" "$out/cpio" "$out/iso/boot/grub"
export TMPDIR="$out/tmp"
echo "Native input retention evidence: $out"
(
    cd kernel
    alr exec -- gprbuild $gprbuild_config -p -P ../tests/input-pending/native/input_native.gpr \
        -XINPUT_NATIVE_BUILD="$out/obj"
)
cp "$out/obj/devmgr.svc" "$out/cpio/devmgr.svc"
(
    cd "$out/cpio"
    printf '%s\n' devmgr.svc | cpio -o -H newc > "$out/iso/boot/initrd.img"
)
# Reuse the built kernel, explicitly recording its binary identity.
cp kernel/cubit_kernel "$out/iso/boot/cubit_kernel"
cp tests/input-pending/native/grub.cfg "$out/iso/boot/grub/grub.cfg"
sha256sum "$out/iso/boot/cubit_kernel" "$out/obj/devmgr.svc" \
    userspace/lib/input/input_pending.ads userspace/lib/input/input_pending.adb \
    userspace/runtime/gnat/cubit-input.adb \
    tests/input-pending/native/input_native_check.adb > "$out/input.sha256"
grub-mkrescue -o "$out/input.iso" "$out/iso"
python3 tests/input-pending/native/run.py "$out"
