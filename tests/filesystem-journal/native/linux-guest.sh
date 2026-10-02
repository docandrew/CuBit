#!/usr/bin/env bash
# Boots a minimal Linux guest (nixpkgs' kernel, busybox, an initramfs, as
# tests/fs-bench/linux.sh) on IMAGE as its NVMe disk and runs GUEST_SCRIPT
# (a busybox sh script) as init, after loading the NVMe and ext4 drivers.
# The script ends the guest itself (poweroff -f, or a crash via sysrq).
# Serial output goes to LOG.
#
# Usage: linux-guest.sh IMAGE GUEST_SCRIPT LOG [--accel kvm|tcg]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
image=$1; script=$2; log=$3; shift 3
accel=kvm
while [ "$#" -gt 0 ]; do
    case "$1" in
        --accel) accel=$2; shift 2 ;;
        *) echo "linux-guest.sh: unknown option $1" >&2; exit 2 ;;
    esac
done
out=$here/build/linux
rm -rf "$out/root"
mkdir -p "$out/root"/{bin,dev,proc,sys,mnt,lib/mods}
export TMPDIR=$here/build/tmp
mkdir -p "$TMPDIR"

paths=$(nix build --no-link --print-out-paths --inputs-from "$root" \
    nixpkgs#linux nixpkgs#linux.modules nixpkgs#pkgsStatic.busybox)
kernel=$(echo "$paths" | grep -- '-linux-[0-9.]*$')
modules=$(echo "$paths" | grep -- '-modules$')
busybox=$(echo "$paths" | grep -- 'busybox')
cp "$busybox/bin/busybox" "$out/root/bin/busybox"

moddir=$(echo "$modules"/lib/modules/*)
load_order=$(python3 - "$moddir/modules.dep" nvme ext4 <<'PY'
import sys
deps = {}
for line in open(sys.argv[1]):
    name, _, rest = line.partition(":")
    deps[name.strip()] = rest.split()
by_base = {p.rsplit("/", 1)[-1].split(".ko")[0].replace("-", "_"): p for p in deps}
order, seen = [], set()
def visit(path):
    if path in seen:
        return
    seen.add(path)
    for d in deps.get(path, []):
        visit(d)
    order.append(path)
for want in sys.argv[2:]:
    if want in by_base:
        visit(by_base[want])
print(" ".join(order))
PY
)
names=""
for m in $load_order; do
    base=$(basename "$m"); base=${base%%.ko*}
    case "$m" in
        *.xz) xz -dc "$moddir/$m" > "$out/root/lib/mods/$base.ko" ;;
        *.zst) zstd -qdc "$moddir/$m" > "$out/root/lib/mods/$base.ko" ;;
        *) cp "$moddir/$m" "$out/root/lib/mods/$base.ko" ;;
    esac
    names="$names $base"
done
cp "$script" "$out/root/guest.sh"
cat > "$out/root/init" <<INIT
#!/bin/busybox sh
/bin/busybox --install -s /bin
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
for m in$names; do
    insmod /lib/mods/\$m.ko
done
for i in 1 2 3 4 5 6 7 8 9 10; do [ -b /dev/nvme0n1 ] && break; sleep 0.2; done
echo "guest: linux \$(uname -r)"
echo 1 > /proc/sys/kernel/sysrq
. /guest.sh
poweroff -f
INIT
chmod +x "$out/root/init"
(cd "$out/root" && find . | cpio -o -H newc --quiet | gzip -1) > "$out/initramfs.gz"

rm -f "$log"
timeout 300 qemu-system-x86_64 -accel "$accel" -machine q35 -cpu Broadwell \
    -smp 2 -m 512M -kernel "$kernel/bzImage" -initrd "$out/initramfs.gz" \
    -append "console=ttyS0 quiet panic=-1" -serial "file:$log" -display none \
    -drive "file=$image,if=none,id=nvme0,format=raw,cache=writethrough" \
    -device nvme,serial=cubitnvme,drive=nvme0 -no-reboot || true
grep -a "^guest:" "$log" || true
