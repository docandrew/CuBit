#!/usr/bin/env bash
# SYS-001 installer v2, SYS-002 and SYS-003 in QEMU (KVM), on scratch disk
# images only:
#   1. install: the CCL console installs the development disk's files (named
#      where they are on @nvme:0, every program with its SHA-256) and a
#      desktop startup plan onto QEMU's UFS disk; the installer's steps are
#      logged in order (partition, format, storehouse, generation 1, verify
#      the root, the ESP, verify it, make bootable last);
#   2. the disk, checked on the host: GPT, fsck.fat, e2fsck, every
#      component in storehouse/executables under its own SHA-256, and
#      generation 1 (typed record) current;
#   3. boot: the installed disk alone reaches the desktop, and procmgr reads
#      generation 1 as current (no pins, no drift);
#   4. interrupted: a reinstall over that bootable disk, its power cut while
#      it fills the storehouse, leaves a disk the firmware cannot boot (no
#      FAT boot sector on the ESP: no loader is read);
#   0. the deadline lint (check_deadlines.py), the hang's regression;
#   5. missing startup profile: the installed disk without init.ccl stops
#      with procmgr's named error well inside its deadline, never a hang.
# The activation at next boot is tests/headless service-switch then
# service-switch-reboot (the same disk booted again).
#
# QEMU's OVMF has no UFS boot driver, so the firmware reads the image
# through a read-only virtio-blk view (as tests/install-reboot does); CuBit
# sees only the UFS disk. CuBit installs only to disks devmgr's generic
# storage launch serves (UFS today; nvme.drv is not a disk-install target).
#
# usage: tests/install-generations/run.sh   (inside nix develop, under the
#        build lock, after make -C kernel world install-efi installed-initrd)
set -u
ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
HERE="$ROOT_DIR/tests/install-generations"
BOOT="$ROOT_DIR/kernel/isodir/boot"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/cubit-install-generations.XXXXXX")"
DISK="$WORK/ufs.img"
DEV="$WORK/dev.img"
DISK_MIB="${CUBIT_INSTALL_DISK_MIB:-2048}"
BLOCK_BYTES=4096
# Generous for KVM: the install writes and reads back the whole payload.
INSTALL_SECONDS=900
DESKTOP_SECONDS=180
# procmgr's Startup_Profile_Deadline_Ms is 10 s; the named error must come
# well before a hang would be called one.
PROFILE_SECONDS=90
QEMU_SHARE="$(dirname "$(readlink -f "$(command -v qemu-system-x86_64)")")/../share/qemu"
FIRMWARE="$QEMU_SHARE/edk2-x86_64-code.fd"
STATUS=0
fail() { echo "install-generations: FAIL: $*" >&2; STATUS=1; }
pass() { echo "install-generations: PASS: $*"; }

# 0. The hang's regression: every call on the install path and procmgr's
#    boot reads has an explicit deadline (fails on the code before the fix).
if python3 -I "$HERE/check_deadlines.py"; then pass "every install-path call has a deadline"
else fail "a call on the install path waits forever"; fi

for NEEDED in install-bootx64.efi cubit_kernel installed-initrd.img installer.svc ccl-console.app; do
    if [ ! -f "$BOOT/$NEEDED" ]; then
        echo "install-generations: missing $BOOT/$NEEDED (make -C kernel world install-efi installed-initrd)" >&2
        exit 1
    fi
done

# The development disk as the install's medium, with the programs the
# console test writes onto it already current (the payload's digests are
# of these bytes).
cp "$ROOT_DIR/kernel/nvme_disk.img" "$DEV" || exit 1
for IMAGE in installer.svc ccl-console.app clock.svc desktop.svc display.svc logstore.svc; do
    debugfs -w -R "rm $IMAGE" "$DEV" >/dev/null 2>&1
    debugfs -w -R "write $BOOT/$IMAGE $IMAGE" "$DEV" >/dev/null 2>&1 || exit 1
done
python3 -I "$HERE/make_payload.py" "$DEV" "$WORK/payload" "$HERE/init-installed-desktop.ccl" \
    "$BOOT/install-bootx64.efi" "$BOOT/cubit_kernel" "$BOOT/installed-initrd.img" || exit 1

install_once() {   # install_once LOG SERIAL [extra env...]
    local LOG="$1" SERIAL_FILE="$2"; shift 2
    env "$@" CCL_CONSOLE_DEMO=install CUBIT_INSTALL_PAYLOAD="$WORK/payload" CUBIT_INSTALL_DISK="$DISK" \
        CUBIT_INSTALL_DISK_MIB="$DISK_MIB" QEMU_MEMORY=2G \
        bash "$ROOT_DIR/tests/headless/run.sh" --test ccl-console --accel kvm \
        --disk "$DEV" --serial "$SERIAL_FILE" --timeout "$INSTALL_SECONDS" > "$LOG" 2>&1
}

disk_boot() {   # disk_boot DISK LOG SECONDS MARKER...: boots DISK alone until every MARKER
    local IMAGE="$1" LOG="$2" SECONDS_LEFT="$3"; shift 3
    qemu-system-x86_64 -machine q35 -accel kvm -cpu host -smp 2 -m 2G \
      -display none -no-reboot -serial "file:$LOG" \
      -drive "if=pflash,format=raw,unit=0,readonly=on,file=$FIRMWARE" \
      -drive "file=$IMAGE,if=none,id=firmware,format=raw,readonly=on,file.locking=off" \
      -device virtio-blk-pci,drive=firmware,bootindex=0,logical_block_size=4096,physical_block_size=4096 \
      -drive "file=$IMAGE,if=none,id=ufs0,format=raw,file.locking=off" \
      -device ufs,serial=cubitufs,id=ufs -device ufs-lu,drive=ufs0,bus=ufs,lun=0 &
    local QEMU_PID=$! STARTED=$SECONDS MARKER ALL
    while [ $((SECONDS - STARTED)) -lt "$SECONDS_LEFT" ] && kill -0 "$QEMU_PID" 2>/dev/null; do
        ALL=1
        for MARKER in "$@"; do
            grep -qaF "$MARKER" "$LOG" 2>/dev/null || ALL=0
        done
        [ "$ALL" = 1 ] && break
        sleep 1
    done
    BOOT_SECONDS=$((SECONDS - STARTED))
    kill "$QEMU_PID" 2>/dev/null
    wait "$QEMU_PID" 2>/dev/null
}

has() { grep -qaF "$2" "$1"; }

# 1. Install.
if install_once "$WORK/install.log" "$WORK/install-serial.log"; then
    pass "installed from the console onto the UFS disk"
else
    fail "the install (log: $WORK/install.log)"
fi
grep -aE "installer: (step|installing|install done|payload|stopped)" "$WORK/install-serial.log"

# 2. The disk on the host.
if python3 -I "$HERE/check_disk.py" "$DISK" "$BLOCK_BYTES" "$WORK/payload"; then
    pass "the disk checks: GPT, ESP, ext2 root, storehouse by SHA-256, generation 1 current"
else
    fail "the installed disk does not check"
fi
cp "$DISK" "$WORK/installed.img"

# 3. Boot the installed disk to the desktop.
disk_boot "$DISK" "$WORK/boot.log" "$DESKTOP_SECONDS" \
    "procmgr: generation 1 is current: 0 pin(s), 0 not applied" "desktop: active outputs=" \
    "desktop: internal shell active"
for MARKER in "devmgr: disk 0 attached as @ufs:0 (writable)" "procmgr: generation 1 is current: 0 pin(s), 0 not applied" \
              "desktop: active outputs=" "desktop: internal shell active"; do
    if has "$WORK/boot.log" "$MARKER"; then pass "installed boot: $MARKER"; else fail "installed boot lacks: $MARKER"; fi
done
if has "$WORK/boot.log" "procmgr: DRIFT"; then fail "installed boot logged drift"; fi

# 4. A reinstall over the bootable disk, interrupted while it fills the
#    storehouse: the disk must no longer boot.
install_once "$WORK/interrupt.log" "$WORK/interrupt-serial.log" CUBIT_INSTALL_REINSTALL=1 \
    CUBIT_INSTALL_INTERRUPT_AT="installer: step: filling the storehouse"
if has "$WORK/interrupt.log" "headless: install interrupted at: installer: step: filling the storehouse"; then
    pass "the reinstall's power was cut while it filled the storehouse"
else
    fail "the reinstall was not interrupted mid-way (log: $WORK/interrupt.log)"
fi
if python3 -I "$HERE/check_disk.py" --not-bootable "$DISK" "$BLOCK_BYTES"; then
    pass "interrupted: the ESP holds no FAT boot sector (no loader can be read)"
else
    fail "interrupted: the disk still looks bootable"
fi
disk_boot "$DISK" "$WORK/interrupted-boot.log" 40 "Loading CuBit kernel" "CuBit"
if has "$WORK/interrupted-boot.log" "Loading CuBit kernel"; then
    fail "the interrupted disk booted CuBit's loader"
else
    pass "the firmware found no loader on the interrupted disk"
fi

# 5. The installed disk without its startup profile.
cp "$WORK/installed.img" "$WORK/no-profile.img"
python3 -I "$HERE/check_disk.py" --remove-from-root "$WORK/no-profile.img" "$BLOCK_BYTES" init.ccl || \
    fail "could not remove init.ccl from the copy"
disk_boot "$WORK/no-profile.img" "$WORK/no-profile.log" "$PROFILE_SECONDS" \
    "procmgr: bootstrap STOPPED: startup profile init.ccl on the system volume is missing"
if has "$WORK/no-profile.log" \
     "procmgr: bootstrap STOPPED: startup profile init.ccl on the system volume is missing"; then
    pass "a missing startup profile stops with procmgr's named error (seen after ${BOOT_SECONDS} s of boot)"
else
    fail "no named error for the missing startup profile within ${PROFILE_SECONDS} s"
fi

if [ "$STATUS" = 0 ]; then
    echo "INSTALL-GENERATIONS: PASS"
    rm -rf "$WORK"
else
    echo "INSTALL-GENERATIONS: FAIL (work: $WORK)"
fi
exit "$STATUS"
