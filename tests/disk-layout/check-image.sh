#!/usr/bin/env bash
# Checks an installer disk image with tools independent of CuBit: the GPT
# (check-gpt.py for any block size; sgdisk too for 512-byte blocks), the
# ESP (fsck.fat, mtools) and the root (e2fsck, debugfs).
#
# usage: check-image.sh IMAGE BLOCK_BYTES ESP_OFFSET ESP_BYTES ROOT_OFFSET
#                       [ESP_DIR ROOT_DIR]
# With the payload directories, every file must read back byte-identical
# from the image (mtools for the ESP, debugfs rdump for the root).
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
IMG="$1"; BLOCK="$2"; ESP_OFF="$3"; ESP_BYTES="$4"; ROOT_OFF="$5"
ESP_DIR="${6:-}"; ROOT_DIR="${7:-}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/check-image.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
MIB=$((1024 * 1024))
FAILED=0

pass() { echo "check-image: $1 ($BLOCK) PASS"; }
fail() { [ -f "$2" ] && cat "$2"; echo "check-image: $1 ($BLOCK) FAIL"; FAILED=1; }

python3 -I "$HERE/check-gpt.py" "$IMG" "$BLOCK" || FAILED=1
if [ "$BLOCK" = 512 ]; then
    if sgdisk -v "$IMG" > "$WORK/sgdisk.txt" 2>&1 && grep -q "No problems found" "$WORK/sgdisk.txt"; then
        pass "sgdisk -v"
    else
        fail "sgdisk -v" "$WORK/sgdisk.txt"
    fi
fi
dd if="$IMG" of="$WORK/esp.img" bs=1M skip=$((ESP_OFF / MIB)) count=$((ESP_BYTES / MIB)) \
   conv=sparse status=none
if fsck.fat -n -v "$WORK/esp.img" > "$WORK/fsck-fat.txt" 2>&1; then
    pass "fsck.fat"
else
    fail "fsck.fat" "$WORK/fsck-fat.txt"
fi
if mdir -i "$WORK/esp.img" :: > "$WORK/mdir.txt" 2>&1 && grep -q "CUBIT ESP" "$WORK/mdir.txt"; then
    pass "mtools reads the ESP"
else
    fail "mtools reads the ESP" "$WORK/mdir.txt"
fi
dd if="$IMG" of="$WORK/root.img" bs=1M skip=$((ROOT_OFF / MIB)) conv=sparse status=none
if e2fsck -fn "$WORK/root.img" > "$WORK/e2fsck.txt" 2>&1; then
    pass "e2fsck -fn"
else
    fail "e2fsck -fn" "$WORK/e2fsck.txt"
fi
if debugfs -R "ls -l /" "$WORK/root.img" 2>/dev/null | grep -q "lost+found"; then
    pass "debugfs lists lost+found"
else
    fail "debugfs lists lost+found" /dev/null
fi
if [ -n "$ESP_DIR" ]; then
    mkdir -p "$WORK/esp-out"
    if mcopy -s -n -i "$WORK/esp.img" ::EFI ::BOOT "$WORK/esp-out/" 2> "$WORK/mcopy.txt" \
       && diff -r "$ESP_DIR" "$WORK/esp-out" > "$WORK/esp-diff.txt" 2>&1; then
        pass "ESP files read back identical"
    else
        cat "$WORK/mcopy.txt"; fail "ESP files read back identical" "$WORK/esp-diff.txt"
    fi
fi
if [ -n "$ROOT_DIR" ]; then
    mkdir -p "$WORK/root-out"
    debugfs -R "rdump / $WORK/root-out" "$WORK/root.img" > /dev/null 2>&1
    rm -rf "$WORK/root-out/lost+found"
    if diff -r "$ROOT_DIR" "$WORK/root-out" > "$WORK/root-diff.txt" 2>&1; then
        pass "root files read back identical"
    else
        head -20 "$WORK/root-diff.txt"; fail "root files read back identical" /dev/null
    fi
fi
exit "$FAILED"
