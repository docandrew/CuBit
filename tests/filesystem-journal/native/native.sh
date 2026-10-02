#!/usr/bin/env bash
# Native ext3 interoperability between Linux and CuBit (live CuBit, real
# Linux kernel; both under QEMU with the same NVMe disk):
#   1. CuBit's test disk (kernel/nvme_disk.img) + journal-check.app, made
#      ext3 with tune2fs -j.
#   2. Linux mounts it as ext3, writes, commits (sync) and crashes without
#      unmounting: a dirty journal with committed transactions.
#   3. CuBit boots on it: admission replays Linux's journal; journal-check
#      verifies Linux's files, then unlinks, mkdirs, rmdirs, creates and
#      writes through CuBit's own journal; QEMU is then killed (no unmount).
#   4. e2fsck -E journal_only on a copy, then e2fsck -fn: clean.
#   5. Linux mounts the CuBit-written volume (its driver replays whatever
#      CuBit left), lists sizes and md5s, unmounts; e2fsck -fn: clean.
# Hold coordination/build.lock (it rebuilds services and boots CuBit).
# Usage: native.sh [--accel kvm|tcg]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
accel=kvm
[ "${1:-}" = "--accel" ] && accel=$2
out=$here/build
mkdir -p "$out/tmp"
export TMPDIR=$out/tmp

make -C "$root/kernel" filesystem libc ccl-manifest >/dev/null
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" \
    > "$out/manifest.S"
as --64 "$out/manifest.S" -o "$out/manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -DCUBIT -o "$out/journal-check.app" \
    "$here/journal-check.c" --manifest "$out/manifest.o"

disk=$out/ext3.img
cp "$root/kernel/nvme_disk.img" "$disk"
debugfs -R "cat init.ccl" "$disk" 2>/dev/null |
    sed '$ s/^)$/  (start "journal-check.app" (priority 3))\n)/' > "$out/init.ccl"
grep -q journal-check "$out/init.ccl"
debugfs -w -R "rm init.ccl" "$disk" >/dev/null 2>&1
debugfs -w -R "write $out/init.ccl init.ccl" "$disk" >/dev/null 2>&1
debugfs -w -R "write $out/journal-check.app journal-check.app" "$disk" >/dev/null 2>&1
tune2fs -O has_journal -J size=16 "$disk" >/dev/null
e2fsck -fn "$disk" >/dev/null

"$here/linux-guest.sh" "$disk" "$here/linux-write.sh" "$out/linux-write.log" --accel "$accel"
grep -aq "^guest: phase-a synced" "$out/linux-write.log"
dumpe2fs -h "$disk" 2>/dev/null | grep -q "needs_recovery" ||
    { echo "native: Linux left no dirty journal"; exit 1; }
echo "native: Linux crashed with a dirty journal ($(dumpe2fs -h "$disk" 2>/dev/null | grep '^Journal start'))"

# The development image's full service set plus the check needs more than
# run.sh's default 128 MiB (as bench-fs does).
QEMU_MEMORY=512M bash "$root/tests/headless/run.sh" --test boot-shell-nvme --disk "$disk" \
    --accel "$accel" --timeout 60 --serial "$out/cubit.log" --keep-logs >/dev/null || true
grep -a "journal-check:\|JOURNAL-CHECK" "$out/cubit.log" | sed 's/^/native: cubit: /'
grep -aq "JOURNAL-CHECK: PASS" "$out/cubit.log"

cp "$disk" "$out/e2fsck.img"
e2fsck -y -E journal_only "$out/e2fsck.img" >/dev/null 2>&1 || true
e2fsck -fn "$out/e2fsck.img" > "$out/e2fsck.log" 2>&1 ||
    { cat "$out/e2fsck.log"; echo "native: e2fsck after replay not clean"; exit 1; }
rm -f "$out/e2fsck.img"

"$here/linux-guest.sh" "$disk" "$here/linux-read.sh" "$out/linux-read.log" --accel "$accel"
grep -aq "^guest: unmounted" "$out/linux-read.log"
python3 - "$out/linux-read.log" <<'PY'
import hashlib, sys
def entry(size, *runs):
    data = b"".join(ch * n for ch, n in runs)
    assert len(data) == size
    return f"{size} {hashlib.md5(data).hexdigest()}"
expected = {
    ".": "dir",
    "./cubit-dir": "dir",
    "./cubit-dir/cubit-file": entry(300000, (b"C", 300000)),
    "./linux-a": entry(69632, (b"L", 65536), (b"N", 4096)),
    "./linux-dir": "dir",
    "./linux-dir/linux-b": entry(5000, (b"M", 5000)),
}
found = {}
for line in open(sys.argv[1], errors="replace"):
    if line.startswith("guest: entry "):
        name, _, rest = line[len("guest: entry "):].strip().partition(" ")
        found[name] = rest
if found != expected:
    print("native: Linux sees", found)
    sys.exit(1)
PY
e2fsck -fn "$disk" > "$out/e2fsck.log" 2>&1 ||
    { cat "$out/e2fsck.log"; echo "native: e2fsck after Linux mount not clean"; exit 1; }
rm -f "$disk"
echo "EXT3-NATIVE-INTEROP: PASS (Linux crash -> CuBit replay + namespace ops -> Linux mount, e2fsck clean)"
