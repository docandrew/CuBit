#!/usr/bin/env bash
# The Linux reference for the scheduler latency benchmark: sched-latency.c,
# built static against musl, as the only program on a minimal Linux guest
# (nixpkgs' kernel, busybox, an initramfs) in the same QEMU configuration as
# CuBit's bench-latency case: q35, Broadwell, 4 vCPUs, KVM. The io load
# writes to the initramfs (tmpfs-like rootfs, no device). Prints the guest's
# sched-latency lines. This is a Linux-hosted reference, not CuBit.
#
# Usage: tests/sched-latency/linux.sh [--accel kvm|tcg] [--cpus N] [--memory SIZE]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
accel=kvm; cpus=4; memory=256M; timeout_s=300
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
mkdir -p "$out/root"/{bin,dev,proc,sys,tmp}
export TMPDIR=${TMPDIR:-$here/build/tmp}
mkdir -p "$TMPDIR"

paths=$(nix build --no-link --print-out-paths --inputs-from "$root" \
    nixpkgs#linux nixpkgs#pkgsStatic.busybox nixpkgs#pkgsStatic.stdenv.cc)
kernel=$(echo "$paths" | grep -- '-linux-[0-9.]*$')
busybox=$(echo "$paths" | grep -- 'busybox')
cc=$(echo "$paths" | grep -- 'gcc-wrapper-[0-9.]*$')/bin/x86_64-unknown-linux-musl-gcc

"$cc" -O2 -static -pthread -Wall -Wextra -o "$out/root/bin/sched-latency" "$here/sched-latency.c"
cp "$busybox/bin/busybox" "$out/root/bin/busybox"
cat > "$out/root/init" <<'EOF'
#!/bin/busybox sh
/bin/busybox --install -s /bin
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
echo "linux: $(uname -r) nproc=$(nproc) mem=$(awk '/MemTotal/ {print $2}' /proc/meminfo)kB"
/bin/sched-latency /tmp/sched-latency-io.dat
poweroff -f
EOF
chmod +x "$out/root/init"
(cd "$out/root" && find . | cpio -o -H newc --quiet | gzip -1) > "$out/initramfs.gz"

log=$out/serial.log
rm -f "$log"
timeout "$timeout_s" qemu-system-x86_64 -accel "$accel" -machine q35 -cpu Broadwell \
    -smp "$cpus" -m "$memory" -kernel "$kernel/bzImage" -initrd "$out/initramfs.gz" \
    -append "console=ttyS0 quiet panic=-1" -serial "file:$log" -display none \
    -no-reboot || true
grep -a "^linux:\|^sched-latency:" "$log"
grep -aq "^sched-latency: done" "$log"
