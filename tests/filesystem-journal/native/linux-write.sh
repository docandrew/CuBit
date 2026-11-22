# Guest phase A: Linux writes through the ext3 journal, then crashes
# (sysrq reboot, no unmount) right after its commits, leaving a dirty
# journal with committed transactions for CuBit to replay.
fill() { head -c "$2" /dev/zero | tr '\0' "$1"; }
if ! mount -t ext3 /dev/nvme0n1 /mnt; then
    echo "guest: mount failed"; poweroff -f
fi
echo "guest: $(grep nvme0n1 /proc/mounts)"
mkdir -p /mnt/journal/linux-dir
fill L 65536 > /mnt/journal/linux-a
fill M 5000 > /mnt/journal/linux-dir/linux-b
fill V 20000 > /mnt/journal/linux-victim
fill G 3000 > /mnt/journal/linux-gone
sync
rm /mnt/journal/linux-gone
sync
echo "guest: phase-a synced"
echo b > /proc/sysrq-trigger
