# Guest phase B: Linux mounts the volume CuBit wrote (its ext3 driver
# replays CuBit's journal if needs_recovery is set), lists every file under
# /journal with size and md5, then unmounts cleanly.
if ! mount -t ext3 /dev/nvme0n1 /mnt; then
    echo "guest: mount failed"; poweroff -f
fi
echo "guest: $(grep nvme0n1 /proc/mounts)"
cd /mnt/journal
for f in $(find . | sort); do
    if [ -d "$f" ]; then
        echo "guest: entry $f dir"
    else
        echo "guest: entry $f $(wc -c < "$f") $(md5sum < "$f" | cut -d' ' -f1)"
    fi
done
cd /
umount /mnt && echo "guest: unmounted"
