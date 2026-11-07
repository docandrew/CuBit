#!/usr/bin/env bash
# SELF-002 on the device-build live image (make -C kernel
# usb-live-build-uefi-iso), in QEMU with KVM: both routes the laptop takes.
#   1. Live: boot the image's -test twin (pairing code on the serial
#      console, nothing else different: checked first) from USB with 8 GiB,
#      and run the device-build flow against its live RAM root (@mem:0,
#      @system): build swap-probe through ccl-remote, its SHA-256 the Linux
#      reference build's, rebuild, build 2, then app replace and rollback
#      behind the Desktop's "Allow app replacement" switch
#      (tests/device-builds/device_build.py through run-live.py
#      --device-build). The memory it saw is in the run's memory.json.
#   2. Install: boot the image again with a blank UFS disk, install onto it
#      from the CCL console (@cd:0/install, which carries the live RAM
#      root's payload too), then boot the installed disk alone and build
#      swap-probe on it (tests/install-reboot/run.sh with
#      CUBIT_INSTALLED_DEVICE_BUILD; build + SHA-256 match).
# usage: tests/device-builds/run-live.sh [live|install]...  (default: both)
#   inside nix develop, under the shared build lock, after make -C kernel
#   world and with the Servo payload at kernel/isodir/boot/cubitshell.app.
#   CUBIT_DEVICE_BUILD_IMAGE=<image> skips building the -test image.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
steps=("$@")
[[ ${#steps[@]} -gt 0 ]] || steps=(live install)
# The live boot runs three builds and two swaps; the install writes the
# whole payload; generous deadlines for both under KVM.
Live_Seconds=1500
Install_Seconds=900
Guest_Memory=8G
Guest_CPUs=4

# The -test twin differs from the image the laptop boots only in where the
# pairing code is shown (Window -> Kernel_Console), in its profile's
# startup plan name and its install payload's startup plan.
python3 -I - "$repo" <<'EOF'
import re, sys
from pathlib import Path
root = Path(sys.argv[1])
def body(path, swaps):
    lines = [l for l in (root / path).read_text().splitlines() if not l.lstrip().startswith("#")]
    text = "\n".join(lines)
    for old, new in swaps:
        text = text.replace(old, new)
    return text
pairs = [
    ("images/laptop-build.ccl", "images/laptop-build-test.ccl",
     [('"init-usb-live-build-test"', '"init-usb-live-build"')]),
    ("tests/hardware/init-usb-live-build.ccl", "tests/device-builds/init-usb-live-build-test.ccl",
     [("Pairing_Display.Kernel_Console", "Pairing_Display.Window")]),
    ("tests/hardware/init-installed-build.ccl", "tests/device-builds/init-installed-build-test.ccl",
     [("Pairing_Display.Kernel_Console", "Pairing_Display.Window")]),
]
for shipped, twin, swaps in pairs:
    if body(shipped, []) != body(twin, swaps):
        sys.exit(f"device-build: {twin} differs from {shipped} beyond the pairing display")
    print(f"PASS: {twin} is {shipped} but for the pairing display")
EOF

image=${CUBIT_DEVICE_BUILD_IMAGE:-}
if [[ -z $image ]]; then
    make -C "$repo/kernel" usb-live-build-test-uefi-iso
    image=$repo/kernel/cubit_live_build_test_uefi.img
fi
work=$(mktemp -d "${TMPDIR:-/tmp}/cubit-device-build-live.XXXXXX")
status=0
for step in "${steps[@]}"; do
    case $step in
        live)
            python3 "$repo/tests/usb-optical/run-live.py" --uefi --image "$image" --quiet-xhci \
                --memory "$Guest_Memory" --cpus "$Guest_CPUs" --timeout "$Live_Seconds" \
                --device-build "$here/build" || status=1
            ;;
        install)
            if python3 "$repo/tests/usb-optical/run-live.py" --uefi --image "$image" --quiet-xhci \
                   --memory "$Guest_Memory" --cpus "$Guest_CPUs" --timeout "$Install_Seconds" \
                   --install-to "$work/installed.img"; then
                CUBIT_INSTALLED_DISK="$work/installed.img" CUBIT_INSTALLED_DEVICE_BUILD="$here/build" \
                    bash "$repo/tests/install-reboot/run.sh" || status=1
            else
                status=1
            fi
            ;;
        *)
            echo "device-build: unknown step $step (live, install)" >&2
            exit 2
            ;;
    esac
done
rm -rf "$work"
if [[ $status == 0 ]]; then echo "DEVICE-BUILD LIVE IMAGE: PASS"; else echo "DEVICE-BUILD LIVE IMAGE: FAIL"; fi
exit "$status"
