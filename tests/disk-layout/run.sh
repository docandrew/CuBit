#!/usr/bin/env bash
# The installer's layout writer on Linux: writes images with 512- and
# 4096-byte blocks (the decoder self-check in main.adb), then checks them
# with independent tools (check-image.sh). Run inside nix develop.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/disk-layout.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
DISK_MIB=1024
FAILED=0

alr exec -- gprbuild -q -p -P "$HERE/disk_layout_tests.gpr"
python3 -I "$HERE/make-payload.py" "$WORK/esp" "$WORK/root"

field() { sed -n "s/.* $1= *\([0-9]*\).*/\1/p" "$2"; }

for BLOCK in 512 4096; do
    IMG="$WORK/disk-$BLOCK.img"
    "$HERE/build/main" "$IMG" "$BLOCK" "$DISK_MIB" "$WORK/esp" "$WORK/root" | tee "$WORK/out-$BLOCK.txt" || FAILED=1
    bash "$HERE/check-image.sh" "$IMG" "$BLOCK" \
         "$(field esp-offset "$WORK/out-$BLOCK.txt")" \
         "$(field esp-bytes "$WORK/out-$BLOCK.txt")" \
         "$(field root-offset "$WORK/out-$BLOCK.txt")" "$WORK/esp" "$WORK/root" || FAILED=1
    rm -f "$IMG"
done
if [ "$FAILED" -eq 0 ]; then echo "DISK-LAYOUT-TOOLS: PASS"; else echo "DISK-LAYOUT-TOOLS: FAIL"; exit 1; fi
