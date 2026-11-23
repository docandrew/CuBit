#!/usr/bin/env bash
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Builds usb-disk.app (the usb-disk headless test's program: the FAT32
# suite of tests/filesystem-fat/native/fat-volume.c on a USB stick,
# @usb:0, the boot medium beside it read only, @cd:0, then the removal of
# a second stick, @usb:1) into
# kernel/isodir/boot. Needs the libc and ccl-manifest built (make -C
# kernel libc ccl-manifest); hold coordination/build.lock.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/manifest.S"
as --64 "$out/manifest.S" -o "$out/manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -Wall -DCUBIT '-DVOLUME="@usb:0"' '-DREMOVAL_VOLUME="@usb:1"' \
    '-DLIVE_VOLUME="@cd:0"' \
    -o "$root/kernel/isodir/boot/usb-disk.app" \
    "$root/tests/filesystem-fat/native/fat-volume.c" --manifest "$out/manifest.o"
# Nothing unsigned runs (SIGN-001).
make -C "$root/kernel" sign-programs
