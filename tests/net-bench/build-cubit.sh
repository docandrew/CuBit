#!/usr/bin/env bash
# Builds net-bench.app for CuBit (musl over netstack) into
# kernel/isodir/boot. Needs the libc and ccl-manifest built
# (make -C kernel libc ccl-manifest); hold coordination/build.lock.
# NET_BENCH_CFLAGS adds compiler flags (e.g. -DCONNECTS=3000 for a stress).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/net-bench-manifest.S"
as --64 "$out/net-bench-manifest.S" -o "$out/net-bench-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -DCUBIT ${NET_BENCH_CFLAGS:-} -o "$root/kernel/isodir/boot/net-bench.app" \
    "$here/net-bench.c" --manifest "$out/net-bench-manifest.o"
