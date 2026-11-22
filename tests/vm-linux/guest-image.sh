#!/usr/bin/env bash
#  VMM-001 slice 1: the pinned test guest (docs/hypervisor.md 10.1, slice 1).
#  Linux 6.12 LTS (nixpkgs' bzImage, from the flake's locked nixpkgs) and a
#  reproducible busybox initramfs, written to firmware/vm-guest/ (never committed: .gitignore) where the
#  vmm component embeds them (userspace/apps/vmm/component.ccl pins both
#  digests). Run in the Nix shell or with nix on PATH.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$root/firmware/vm-guest
mkdir -p "$out"
paths=$(nix build --inputs-from "$root" nixpkgs#linuxPackages_6_12.kernel \
          nixpkgs#pkgsStatic.busybox --no-link --print-out-paths)
kernel=$(echo "$paths" | grep -- '-linux-' | head -1)
busybox=$(echo "$paths" | grep -- 'busybox' | head -1)
install -m 0644 "$kernel/bzImage" "$out/bzImage"
# The device node in a cpio needs no privilege: the archive only names it.
python3 "$here/make-initramfs.py" "$busybox/bin/busybox" "$out/initramfs.cpio"
sha256sum "$out/bzImage" "$out/initramfs.cpio"
