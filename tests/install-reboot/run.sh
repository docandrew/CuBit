#!/usr/bin/env bash
# SYS-001 end to end in QEMU: install the real payload onto a UFS disk from
# the CCL console (tests/headless ccl-console, CCL_CONSOLE_DEMO=install),
# then boot the installed disk alone, twice. Each disk boot runs
# install-boot-check from the installed root; the second must read the
# first boot's records in the persistent log (@ufs:0/logstore.log).
#
# QEMU's OVMF has no UFS boot driver, so the firmware reads the same image
# through a read-only virtio-blk view (file.locking=off) to find
# \EFI\BOOT\BOOTX64.EFI (4 KiB blocks, as the UFS disk's GPT is laid out);
# CuBit has no virtio-blk driver and sees only UFS.
#
# usage: tests/install-reboot/run.sh   (after make -C kernel world
#        install-efi installed-initrd, inside nix develop, under the
#        build lock). CUBIT_INSTALLED_DISK=<copy of an installed disk>
#        skips the install and boots that disk.
set -u
ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BOOT="$ROOT_DIR/kernel/isodir/boot"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/cubit-install-reboot.XXXXXX")"
DISK="$WORK/ufs.img"
BOOT_SECONDS=120
# logstore flushes the persistent log every second (log_persistence.ads).
FLUSH_SECONDS=3
FIRST_MARKER="install-boot-check: first boot of the installed system"
READ_MARKER="install-boot-check: an earlier boot's records are readable PASS"
DONE_MARKER="install-boot-check: boot marker"
WRITABLE_MARKER="devmgr: disk 0 attached as @ufs:0 (writable)"

for NEEDED in install-bootx64.efi cubit_kernel installed-initrd.img \
              logstore.svc clock.svc install-boot-check.app; do
    if [ ! -f "$BOOT/$NEEDED" ]; then
        echo "install-reboot: missing $BOOT/$NEEDED" >&2
        exit 1
    fi
done
python3 -I "$ROOT_DIR/tools/install_payload.py" "$WORK/payload" \
  --efi "$BOOT/install-bootx64.efi" --kernel "$BOOT/cubit_kernel" \
  --initrd "$BOOT/installed-initrd.img" \
  --root "init.ccl=$ROOT_DIR/tests/headless/init-installed-check.ccl" \
  --root "clock.svc=$BOOT/clock.svc" \
  --root "install-boot-check.app=$BOOT/install-boot-check.app" \
  --on-medium "logstore.svc=install/medium/logstore.svc" || exit 1
# A file INDEX names where it already is on the medium (as the live image's
# closure is named): the payload lands at @nvme:0/install.
mkdir -p "$WORK/payload/medium"
cp "$BOOT/logstore.svc" "$WORK/payload/medium/logstore.svc"

if [ -n "${CUBIT_INSTALLED_DISK:-}" ]; then
    cp "$CUBIT_INSTALLED_DISK" "$DISK" || exit 1
elif ! CCL_CONSOLE_DEMO=install CUBIT_INSTALL_PAYLOAD="$WORK/payload" CUBIT_INSTALL_DISK="$DISK" \
     bash "$ROOT_DIR/tests/headless/run.sh" --test ccl-console --accel kvm --timeout 240; then
    echo "install-reboot: the install failed (work: $WORK)" >&2
    exit 1
fi

QEMU_SHARE="$(dirname "$(readlink -f "$(command -v qemu-system-x86_64)")")/../share/qemu"
FIRMWARE="$QEMU_SHARE/edk2-x86_64-code.fd"

disk_boot() {
    local LOG="$1"
    qemu-system-x86_64 -machine q35 -accel kvm -cpu host -smp 2 -m 1G \
      -display none -no-reboot -serial "file:$LOG" \
      -drive "if=pflash,format=raw,unit=0,readonly=on,file=$FIRMWARE" \
      -drive "file=$DISK,if=none,id=firmware,format=raw,readonly=on,file.locking=off" \
      -device virtio-blk-pci,drive=firmware,bootindex=0,logical_block_size=4096,physical_block_size=4096 \
      -drive "file=$DISK,if=none,id=ufs0,format=raw,file.locking=off" \
      -device ufs,serial=cubitufs,id=ufs -device ufs-lu,drive=ufs0,bus=ufs,lun=0 &
    local QEMU_PID=$!
    local WAITED=0
    while [ "$WAITED" -lt "$BOOT_SECONDS" ] && kill -0 "$QEMU_PID" 2>/dev/null; do
        if grep -qaF "$DONE_MARKER" "$LOG" 2>/dev/null; then
            sleep "$FLUSH_SECONDS"
            break
        fi
        sleep 1
        WAITED=$((WAITED + 1))
    done
    kill "$QEMU_PID" 2>/dev/null
    wait "$QEMU_PID" 2>/dev/null
}

STATUS=0
disk_boot "$WORK/boot-a.log"
disk_boot "$WORK/boot-b.log"
for CHECK in "boot-a.log:$WRITABLE_MARKER" "boot-a.log:$FIRST_MARKER" \
             "boot-b.log:$WRITABLE_MARKER" "boot-b.log:$READ_MARKER"; do
    LOG="$WORK/${CHECK%%:*}"
    if ! grep -qaF "${CHECK#*:}" "$LOG"; then
        echo "install-reboot: ${CHECK%%:*} lacks: ${CHECK#*:}" >&2
        STATUS=1
    fi
done
grep -aF "install-boot-check:" "$WORK/boot-a.log" "$WORK/boot-b.log"
if [ "$STATUS" = 0 ]; then
    echo "INSTALL-REBOOT: PASS"
    rm -rf "$WORK"
else
    echo "INSTALL-REBOOT: FAIL (logs: $WORK)"
fi
exit "$STATUS"
