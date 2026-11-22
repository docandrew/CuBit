#!/usr/bin/env bash
# The Bible demo (APP-005): a bootable ISO and a guest disk with the full
# desktop session (the Apps menu's Reference group holds Dictionary and
# Bible; the Bible starts with the session), the cubit-bible package, the
# wallpaper and icons; and run.sh, one plain QEMU command.
#
# Run inside nix develop, after `make -C kernel world bible-assets`, under
# the shared build lock (it stages into kernel/isodir):
#   flock coordination/build.lock nix develop -c tests/bible-app/make-demo.sh [OUT_DIR]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
out="${1:-/home/doc/cubit-build-tmp/bible-demo}"
mkdir -p "$out"
cd "$root/kernel"
# What make run-desktop stages: the whole session (metrics.svc too).
make desktop-session-content bible
make nvme_disk.img
make initrd
make prepare-desktop-disk DESKTOP_INIT=../tests/headless/init-bible-demo.ccl \
  DESKTOP_SCRATCH_DISK="$out/cubit-bible-demo.img"
cp cubit_kernel isodir/boot/cubit_kernel
# GRUB's desktop entry (as make run-desktop boots it), then back.
sed -i 's/^set default=.*/set default=4/' isodir/boot/grub/grub.cfg
grub-mkrescue -o "$out/cubit-bible-demo.iso" isodir 2>/dev/null
sed -i 's/^set default=.*/set default=0/' isodir/boot/grub/grub.cfg
cat > "$out/run.sh" <<'RUN'
#!/bin/sh
# Boot the CuBit Bible demo: the desktop at 1920x1080 with the Bible open.
# Needs only qemu-system-x86_64 with KVM. Serial output goes to serial.log.
here="$(cd "$(dirname "$0")" && pwd)"
exec qemu-system-x86_64 -accel kvm -machine q35 -cpu Broadwell,vendor=GenuineIntel -smp 4 -m 4G \
  -cdrom "$here/cubit-bible-demo.iso" \
  -drive file="$here/cubit-bible-demo.img",if=none,id=nvme0,format=raw \
  -device nvme,serial=cubitnvme,drive=nvme0 \
  -vga none -device virtio-vga,xres=1920,yres=1080 -device virtio-tablet-pci \
  -display gtk,zoom-to-fit=on -serial file:"$here/serial.log" -no-reboot
RUN
chmod +x "$out/run.sh"
ls -la "$out"
