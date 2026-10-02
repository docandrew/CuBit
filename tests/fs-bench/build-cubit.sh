#!/usr/bin/env bash
# Builds fs-bench.app for CuBit (musl over filesystem.svc) into
# kernel/isodir/boot. Needs the libc and ccl-manifest built
# (make -C kernel libc ccl-manifest); hold coordination/build.lock.
# FS_BENCH_CFLAGS adds compiler flags (e.g. -DFILE_MIB=16).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/fs-bench-manifest.S"
as --64 "$out/fs-bench-manifest.S" -o "$out/fs-bench-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -DCUBIT ${FS_BENCH_CFLAGS:-} -o "$root/kernel/isodir/boot/fs-bench.app" \
    "$here/fs-bench.c" --manifest "$out/fs-bench-manifest.o"
