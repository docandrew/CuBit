#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root/kernel"
stage=$(mktemp -d /tmp/cubit-usb-image.XXXXXX)
# Retain staging for inspection, including exact bootstrap/payload membership.
mkdir -p "$stage/initrd" "$stage/iso/boot/grub" "$stage/iso/apps"
for file in devmgr.svc filesystem.svc ps2.drv xhci.drv; do
    cp "isodir/boot/$file" "$stage/initrd/"
done
cp laptop_live_rw.img "$stage/initrd/live-rw.ext2"
cp ../tests/hardware/init-usb-live.conf "$stage/initrd/init.conf"
cp ../tests/hardware/system-live.conf "$stage/initrd/system.conf"
for file in config.svc netmgr.svc netstack.svc virtio-net.drv virtio-gpu.drv \
    hda.drv mixer.svc procmgr.svc logstore.svc clock.svc display.svc desktop.svc \
    ccl-workbench.app devices.app files.app doom.elf; do
    cp "isodir/boot/$file" "$stage/iso/apps/"
done
cp "${1:?DOOM WAD path required}" "$stage/iso/apps/doom1.wad"
(cd "$stage/initrd" && find . -mindepth 1 -maxdepth 1 -printf '%f\n' | sort |
    cpio -o -H newc) > "$stage/iso/boot/initrd.img"
cp cubit_kernel "$stage/iso/boot/cubit_kernel"
cp ../tests/hardware/grub-usb-live.cfg "$stage/iso/boot/grub/grub.cfg"
# Agreed ISO9660 identifier profile: preserve ASCII hyphens/lowercase/long
# names in the primary tree. The native reader does not depend on Rock Ridge.
grub-mkrescue -o "$stage/cubit_laptop_usb.iso" "$stage/iso" \
    -iso-level 3 -full-iso9660-filenames -allow-lowercase -allow-multidot -relaxed-filenames
python3 ../tests/usb-optical/check-image.py "$stage/cubit_laptop_usb.iso"
cp "$stage/cubit_laptop_usb.iso" cubit_laptop_usb.iso
printf 'USB live ISO: %s/kernel/cubit_laptop_usb.iso\nStaging: %s\n' "$root" "$stage"
