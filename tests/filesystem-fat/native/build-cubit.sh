#!/usr/bin/env bash
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Builds fat-volume.app (the fat32-volume headless test's program) into
# kernel/isodir/boot. Needs the libc and ccl-manifest built (make -C kernel
# libc ccl-manifest); hold coordination/build.lock.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/manifest.S"
as --64 "$out/manifest.S" -o "$out/manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -Wall -DCUBIT -o "$root/kernel/isodir/boot/fat-volume.app" \
    "$here/fat-volume.c" --manifest "$out/manifest.o"
# Nothing unsigned runs (SIGN-001).
make -C "$root/kernel" sign-programs
