#!/usr/bin/env bash
# Run inside Nix, with coordination/build.lock held. Never touches a user disk.
set -euo pipefail
cd "$(dirname "$0")/../.."
if [[ $# -lt 1 || $# -gt 2 ]]; then
    echo 'Usage: run.sh NEW_RESULTS_DIRECTORY [--test | --apps-test | --reuse]' >&2
    exit 2
fi
results=$(realpath -m "$1")
mode=${2:-interactive}
case "$mode" in interactive|--test|--apps-test|--reuse) ;; *) echo 'Unknown mode' >&2; exit 2;; esac
if [[ $mode != --reuse ]]; then mkdir "$results"; fi
python3 tests/config-workbench/check-schema-keys.py
make -C kernel ccl-workbench desktop display clock logstore config config-storage procmgr devmgr ccl-manifest
make -C kernel iso
if [[ $mode == --apps-test ]]; then
    # Uses the ordinary desktop startup and exact same staging recipe as the
    # launchers, but writes only our new disposable result disk. The base is
    # read-only, including its Servo/fonts assets; no user disk is replaced.
    make -C kernel prepare-desktop-disk DESKTOP_SCRATCH_DISK="$results/disk.img"
elif [[ $mode != --reuse ]]; then
    python3 tools/build_development_disk.py "$results/disk.img" --boot \
      kernel/isodir/boot/ccl-workbench.app kernel/isodir/boot/desktop.svc \
      kernel/isodir/boot/display.svc kernel/isodir/boot/clock.svc \
      kernel/isodir/boot/logstore.svc kernel/isodir/boot/virtio-gpu.drv \
      userspace/services/config-storage/build/config-storage.svc \
      --file init.ccl=tests/config-workbench/init.ccl \
      --file work/config-counter.ccl=userspace/ccl/samples/config-counter.ccl \
      --file work/config-counter-read.ccl=userspace/ccl/samples/config-counter-read.ccl
fi
if [[ $mode == --apps-test ]]; then
    CONFIG_WORKBENCH_APPS=1 python3 tests/config-workbench/exercise.py "$results"
elif [[ $mode == --test ]]; then
    python3 tests/config-workbench/exercise.py "$results"
else
    # Reuse retains the exact boot snapshot and database; never restage files
    # over a running or previously edited disk behind the user's back.
    test -f "$results/disk.img"
    GDK_BACKEND=x11 qemu-system-x86_64 -enable-kvm -machine q35 -cpu host -smp 4 -m 512M \
      -cdrom kernel/cubit_kernel.iso -drive "file=$results/disk.img,if=none,id=nvme0,format=raw" \
      -device nvme,serial=cubit-config-demo,drive=nvme0 -device virtio-vga \
      -display gtk,zoom-to-fit=on -serial "file:$results/serial.log" -no-reboot
fi
