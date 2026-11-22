#!/usr/bin/env bash
#  HW-002 device matching, hosted: the real drivers' manifests compiled by
#  ccl-manifest, indexed by ccl-device-index as the image realizer does,
#  then decoded and bound against the IdeaPad's and QEMU's PCI devices
#  (device_matching_tests.adb). Also: the index tool refuses an image with
#  two equally specific drivers. --prove runs gnatprove on the formats.
#  Run in the Nix shell (after `make -C kernel ccl-manifest ccl-image`).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
objects=$here/build/objects
mkdir -p "$objects"
tool=../userspace/ccl/build/manifest/ccl-manifest
index=../userspace/ccl/build/image/ccl-device-index
args=()
for d in ata hda intel-gpu nvme ps2 ramdisk virtio-gpu virtio-input virtio-net xhci ufs mixer; do
    $tool ../userspace/ccl/catalogs/driver-services.ccl ../userspace/services/$d/manifest.ccl > "$objects/$d.S"
    alr exec -- gcc -c "$objects/$d.S" -o "$objects/$d.o"
    suffix=.drv; [[ $d == mixer ]] && suffix=.svc
    args+=("$d$suffix=$objects/$d.o")
done
$index --output "$here/build/devices.index" "${args[@]}"
#  Two drivers declaring the same class: refused, naming both.
if out=$($index --output "$here/build/ambiguous.index" "nvme.drv=$objects/nvme.o" "nvme2.drv=$objects/nvme.o" 2>&1); then
    echo "DEVICE-MATCHING: FAIL an ambiguous image was indexed"; exit 1
fi
grep -q "ambiguous drivers: nvme.drv (PCI_Class 01.08.02) and nvme2.drv" <<<"$out" ||
    { echo "DEVICE-MATCHING: FAIL ambiguity not named: $out"; exit 1; }
alr exec -- gprbuild -q -p -P "$here/device_matching_tests.gpr"
#  ufs.drv's resource requests, for the window-size check.
objcopy --dump-section .cubit.resources="$here/build/ufs.resources" "$objects/ufs.o" "$objects/ufs.dump.o"
"$here/build/device_matching_tests" "$here/build/devices.index" "$here/build/ufs.resources"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 --quiet \
        alr exec -- gnatprove -P "$here/device_matching_tests.gpr" \
        -u ccl-device_sections.adb -u ccl-device_index.adb -u devmgr_bindings.adb -u devmgr_windows.ads \
        --level=2 -j2 --report=statistics
fi
