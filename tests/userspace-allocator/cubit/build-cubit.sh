#!/usr/bin/env bash
# Builds cualloc-bench.app for CuBit (the libc malloc family over CuAlloc)
# into kernel/isodir/boot. Needs the libc and ccl-manifest built
# (make -C kernel libc ccl-manifest); hold coordination/build.lock.
# Run: tests/headless/run.sh --test bench-cualloc --accel kvm
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" \
    --schema "$root/userspace/ccl/interfaces/executable-manifest.ccl" > "$out/cualloc-bench-manifest.S"
as --64 "$out/cualloc-bench-manifest.S" -o "$out/cualloc-bench-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -pthread -o "$root/kernel/isodir/boot/cualloc-bench.app" \
    "$here/cualloc-bench.c" --manifest "$out/cualloc-bench-manifest.o"
