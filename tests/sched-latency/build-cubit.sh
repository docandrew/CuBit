#!/usr/bin/env bash
# Builds sched-latency.app for CuBit (musl over CuBit syscalls) into
# kernel/isodir/boot. Needs the libc, ccl-manifest and bench-ipc-server
# (make -C kernel libc ccl-manifest bench-ipc-server); hold
# coordination/build.lock.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/sched-latency-manifest.S"
as --64 "$out/sched-latency-manifest.S" -o "$out/sched-latency-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -pthread -DCUBIT -Wall -Wextra -o "$root/kernel/isodir/boot/sched-latency.app" \
    "$here/sched-latency.c" --manifest "$out/sched-latency-manifest.o"
