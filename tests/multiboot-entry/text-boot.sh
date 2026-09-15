#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
run_dir="$(mktemp -d /tmp/cubit-entry-text.XXXXXX)"
mkdir -p "$run_dir/isodir/boot/grub"
cp "$test_dir/../../kernel/cubit_kernel" "$run_dir/isodir/boot/cubit_kernel"
cp "$test_dir/grub-text.cfg" "$run_dir/isodir/boot/grub/grub.cfg"
echo "Text-entry regression logs: $run_dir"
grub-mkrescue -o "$run_dir/entry.iso" "$run_dir/isodir" > "$run_dir/iso.log" 2>&1
result=0
timeout 20 qemu-system-x86_64 -accel kvm -cpu host -smp 2 -m 512 \
    -cdrom "$run_dir/entry.iso" -boot d -display none -nic none \
    -serial "file:$run_dir/serial.log" -no-reboot \
    > "$run_dir/qemu.log" 2>&1 || result=$?
if [ "$result" != 0 ] && [ "$result" != 124 ]; then
    echo "FAIL QEMU exit $result; inspect $run_dir" >&2
    exit 1
fi
if rg -q 'PANIC|EXCEPTION|Illegal memory access' "$run_dir/serial.log"; then
    echo "FAIL native fault; inspect $run_dir" >&2
    exit 1
fi
rg -q 'EARLY: Ada entered; boot information admitted' "$run_dir/serial.log"
rg -q 'EARLY: memory initialization complete' "$run_dir/serial.log"
rg -q 'ACPI tables loaded' "$run_dir/serial.log"
echo "PASS text framebuffer, memory initialization and legacy ACPI table discovery"
