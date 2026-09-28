#!/usr/bin/env bash
# The Linux reference for the network benchmark: net-bench.c, built static
# against musl, on a minimal Linux guest (nixpkgs' kernel, busybox, an
# initramfs) in the same QEMU configuration as CuBit's bench-net case -
# machine, CPU model, vCPUs, memory, virtio-net and user networking -
# against the same host server. Prints the guest's net-bench lines.
#
# Usage: tests/net-bench/linux.sh [--accel kvm|tcg] [--cpus N] [--memory SIZE]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
accel=kvm; cpus=4; memory=512M; timeout_s=300
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
mkdir -p "$out/root"/{bin,dev,proc,sys,lib/mods}
export TMPDIR=$here/build/tmp
mkdir -p "$TMPDIR"

paths=$(nix build --no-link --print-out-paths --inputs-from "$root" \
    nixpkgs#linux nixpkgs#linux.modules nixpkgs#pkgsStatic.busybox nixpkgs#pkgsStatic.stdenv.cc)
kernel=$(echo "$paths" | grep -- '-linux-[0-9.]*$')
modules=$(echo "$paths" | grep -- '-modules$')
busybox=$(echo "$paths" | grep -- 'busybox')
cc=$(echo "$paths" | grep -- 'gcc-wrapper-[0-9.]*$')/bin/x86_64-unknown-linux-musl-gcc

"$cc" -O2 -static ${NET_BENCH_CFLAGS:-} -o "$out/root/bin/net-bench" "$here/net-bench.c"
cp "$busybox/bin/busybox" "$out/root/bin/busybox"
mods=$(echo "$modules"/lib/modules/*/kernel)
for m in drivers/virtio/virtio_ring drivers/virtio/virtio \
         drivers/virtio/virtio_pci_legacy_dev drivers/virtio/virtio_pci_modern_dev \
         drivers/virtio/virtio_pci net/core/failover drivers/net/net_failover \
         drivers/net/virtio_net; do
    xz -dc "$mods/$m.ko.xz" > "$out/root/lib/mods/$(basename "$m").ko"
done
cat > "$out/root/init" <<'EOF'
#!/bin/busybox sh
/bin/busybox --install -s /bin
mount -t proc proc /proc
mount -t sysfs sys /sys
for m in virtio_ring virtio virtio_pci_legacy_dev virtio_pci_modern_dev virtio_pci \
         failover net_failover virtio_net; do
    insmod /lib/mods/$m.ko
done
ip link set lo up
ip link set eth0 up
ip addr add 10.0.2.15/24 dev eth0
ip route add default via 10.0.2.2
echo "linux: $(uname -r) nproc=$(nproc)"
/bin/net-bench
poweroff -f
EOF
chmod +x "$out/root/init"
(cd "$out/root" && find . | cpio -o -H newc --quiet | gzip -1) > "$out/initramfs.gz"

python3 "$here/server.py" "$timeout_s" &
server=$!
trap 'kill $server 2>/dev/null || true' EXIT
sleep 0.5
log=$out/serial.log
timeout "$timeout_s" qemu-system-x86_64 -accel "$accel" -machine q35 -cpu Broadwell \
    -smp "$cpus" -m "$memory" -kernel "$kernel/bzImage" -initrd "$out/initramfs.gz" \
    -append "console=ttyS0 quiet panic=-1" -serial "file:$log" -display none \
    -device virtio-net-pci,netdev=net0 \
    -netdev user,id=net0,hostfwd=tcp:127.0.0.1:18486-10.0.2.15:8080 -no-reboot || true
grep -a "^linux:\|^net-bench:" "$log"
grep -aq "^net-bench: done" "$log"
