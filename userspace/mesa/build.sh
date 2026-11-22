#!/usr/bin/env bash
# Invoked from the repository Nix environment, holding coordination/build.lock
# (make -C kernel teapots). Build the CuBit teapot gallery against the
# verified combined Mesa (ANV) bundle the Desktop uses, stage it, and stage
# the Mesa notice bundle for the images that carry it. A CuBit ELF, not a
# Linux-hosted demonstration.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
build="$root/userspace/mesa/build"
mkdir -p "$build"
python3 tools/build_teapots.py --map "$build/teapots.map"
generation=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["generation"])' \
    "$build/combined/current.json")
mesa="$build/combined/$generation"
# Keep each notice snapshot immutable, including when only packaging changes.
# Staging refuses existing destinations; a fresh private directory avoids stale
# upstream license files lingering after a future source-version change.
notice_parent=$(mktemp -d "$build/notices.XXXXXX")
bash userspace/mesa/stage-notices.sh "$mesa/source" "$notice_parent/mesa"
cp "$build/teapots.map" "$notice_parent/mesa/LINK-MAP.txt"
python3 userspace/mesa/link-inventory.py "$build/teapots.map" "$mesa/native" "$mesa/source" \
    > "$notice_parent/mesa/LINKED-SOURCES.json"
sha256sum kernel/isodir/boot/teapots.app > "$notice_parent/mesa/ELF-SHA256.txt"
echo "Mesa notices staged: $notice_parent/mesa"
# Trusted image wrapper consumes this only after a successful build/stage.
printf '%s\n' "$notice_parent/mesa" > "$build/notice-path"
echo "Teapots staged: $root/kernel/isodir/boot/teapots.app"
