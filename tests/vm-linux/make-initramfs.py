#!/usr/bin/env python3
"""VMM-001 slice 1: the test guest's initramfs, byte-for-byte reproducible.

A newc cpio (every mtime 0, uid/gid 0, sorted) holding a static busybox and
an /init that prints the guest's identity and its spec_store_bypass and
spectre_v2 lines, runs two commands so a screenshot shows a live guest, and
starts a shell on the serial console, its controlling terminal (setsid
cttyhack: job control and Ctrl+C work).

usage: make-initramfs.py BUSYBOX OUTPUT
"""
import sys

INIT = b"""#!/bin/busybox sh
/bin/busybox mkdir -p /proc /sys /dev /tmp
/bin/busybox --install -s /bin
mount -t proc proc /proc
mount -t sysfs sysfs /sys
mount -t devtmpfs devtmpfs /dev
# Kernel messages from here on stay in dmesg: on two CPUs a late printk
# (the clocksource switch) would otherwise interleave with these lines.
dmesg -n 1
echo
echo "CuBit guest: Linux is up under the CuBit hypervisor (guest serial console)"
echo "# uname -a"
uname -a
echo "# head -5 /proc/cpuinfo"
head -5 /proc/cpuinfo
echo "CuBit guest: $(nproc) CPUs online"
# The guest's own view of its speculation controls (docs/hypervisor.md
# 9.2): the kernel switches SPEC_CTRL per vCPU, so the guest mitigates.
for v in spec_store_bypass spectre_v2; do
    echo "CuBit guest: $v: $(cat /sys/devices/system/cpu/vulnerabilities/$v)"
done
echo "CuBit guest: shell ready"
# The shell in its own session, the serial port its controlling terminal
# (cttyhack opens the console's real tty): job control, and Ctrl+C typed
# in vmm's window interrupts the foreground command.
exec setsid cttyhack sh
"""

def entry(name, mode, data, ino, rdev=(0, 0)):
    namez = name.encode() + b"\0"
    hdr = b"070701" + b"".join(b"%08X" % v for v in (
        ino, mode, 0, 0, 1, 0, len(data), 0, 0, rdev[0], rdev[1], len(namez), 0))
    out = hdr + namez
    out += b"\0" * ((4 - len(out) % 4) % 4)
    out += data
    out += b"\0" * ((4 - len(out) % 4) % 4)
    return out

def main():
    busybox = open(sys.argv[1], "rb").read()
    items = [
        ("bin", 0o040755, b""),
        ("bin/busybox", 0o100755, busybox),
        ("dev", 0o040755, b""),
        ("dev/console", 0o020600, b""),   # character device 5:1
        ("init", 0o100755, INIT),
    ]
    out = b""
    for ino, (name, mode, data) in enumerate(sorted(items), start=1):
        out += entry(name, mode, data, ino, (5, 1) if name == "dev/console" else (0, 0))
    out += entry("TRAILER!!!", 0, b"", 0)
    out += b"\0" * ((512 - len(out) % 512) % 512)
    open(sys.argv[2], "wb").write(out)

main()
