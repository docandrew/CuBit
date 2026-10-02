#!/usr/bin/env bash
# The Linux reference for the filesystem benchmark: fs-bench.c, built static
# against musl, on a minimal Linux guest (nixpkgs' kernel, busybox, an
# initramfs) in the same QEMU configuration as CuBit's bench-fs case -
# machine, CPU model, vCPUs, memory, and an NVMe device backed by a raw
# ext2 image with the geometry of CuBit's test disk (kernel/nvme_disk.img:
# 384 MiB, 4 KiB blocks, 98,304 inodes of 256 bytes), made fresh each run.
# Linux mounts it with its ext2 driver (fs/ext2). Prints the guest's
# fs-bench lines.
#
# Usage: tests/fs-bench/linux.sh [--accel kvm|tcg] [--cpus N] [--memory SIZE]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
# ext3 (journaled, mounted by Linux's ext4 driver in data=ordered mode) by
# default, as CuBit's bench-fs disk; FS_BENCH_FS=ext2 for the unjournaled one.
fs_type=${FS_BENCH_FS:-ext3}
case $fs_type in
    ext2) fs_module=ext2 ;;
    ext3) fs_module=ext4 ;;
    *) echo "linux.sh: FS_BENCH_FS must be ext2 or ext3" >&2; exit 2 ;;
esac
root=$(cd "$here/../.." && pwd)
accel=kvm; cpus=4; memory=512M; timeout_s=600
disk_size=384M; block_bytes=4096; inode_count=98304; inode_bytes=256
while [ "$#" -gt 0 ]; do
    case "$1" in
        --accel) accel=$2; shift 2 ;;
        --cpus) cpus=$2; shift 2 ;;
        --memory) memory=$2; shift 2 ;;
        --timeout) timeout_s=$2; shift 2 ;;
        *) echo "linux.sh: unknown option $1" >&2; exit 2 ;;
    esac
done
out=$here/build/linux
rm -rf "$out/root"
mkdir -p "$out/root"/{bin,dev,proc,sys,mnt,lib/mods}
export TMPDIR=$here/build/tmp
mkdir -p "$TMPDIR"

paths=$(nix build --no-link --print-out-paths --inputs-from "$root" \
    nixpkgs#linux nixpkgs#linux.modules nixpkgs#pkgsStatic.busybox nixpkgs#pkgsStatic.stdenv.cc)
kernel=$(echo "$paths" | grep -- '-linux-[0-9.]*$')
modules=$(echo "$paths" | grep -- '-modules$')
busybox=$(echo "$paths" | grep -- 'busybox')
cc=$(echo "$paths" | grep -- 'gcc-wrapper-[0-9.]*$')/bin/x86_64-unknown-linux-musl-gcc

"$cc" -O2 -static ${FS_BENCH_CFLAGS:-} -o "$out/root/bin/fs-bench" "$here/fs-bench.c"
cp "$busybox/bin/busybox" "$out/root/bin/busybox"

# The NVMe and ext2 modules with their dependencies, in load
# order; whatever the kernel has built in is simply absent from modules.dep.
moddir=$(echo "$modules"/lib/modules/*)
load_order=$(python3 - "$moddir/modules.dep" nvme "$fs_module" <<'EOF'
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
EOF
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
cat > "$out/root/init" <<EOF
#!/bin/busybox sh
/bin/busybox --install -s /bin
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
for m in$names; do
    insmod /lib/mods/\$m.ko
done
for i in 1 2 3 4 5 6 7 8 9 10; do [ -b /dev/nvme0n1 ] && break; sleep 0.2; done
echo "linux: \$(uname -r) nproc=\$(nproc) mem=\$(awk '/MemTotal/ {print \$2}' /proc/meminfo)kB"
if ! mount -t $fs_type /dev/nvme0n1 /mnt; then
    echo "linux: mount failed"
    poweroff -f
fi
echo "linux: \$(grep nvme0n1 /proc/mounts)"
/bin/fs-bench /mnt/fs-bench
umount /mnt
poweroff -f
EOF
chmod +x "$out/root/init"
(cd "$out/root" && find . | cpio -o -H newc --quiet | gzip -1) > "$out/initramfs.gz"

disk=$out/nvme-$fs_type.img
rm -f "$disk"
mke2fs -q -t "$fs_type" -b "$block_bytes" -N "$inode_count" -I "$inode_bytes" -F "$disk" "$disk_size"

log=$out/serial.log
rm -f "$log"
timeout "$timeout_s" qemu-system-x86_64 -accel "$accel" -machine q35 -cpu Broadwell \
    -smp "$cpus" -m "$memory" -kernel "$kernel/bzImage" -initrd "$out/initramfs.gz" \
    -append "console=ttyS0 quiet panic=-1" -serial "file:$log" -display none \
    -drive "file=$disk,if=none,id=nvme0,format=raw" \
    -device nvme,serial=cubitnvme,drive=nvme0 -no-reboot || true
rm -f "$disk"
grep -a "^linux:\|^fs-bench:" "$log"
grep -aq "^fs-bench: done" "$log"
