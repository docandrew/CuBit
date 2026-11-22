#!/usr/bin/env bash
# Builds kern014-bench.app and kern014-child.app into kernel/isodir/boot.
# Needs libc and ccl-manifest built (make -C kernel libc ccl-manifest).
# KERN-014 A/B benchmark: tests/headless/run.sh --test kern014-bench --accel kvm
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$here/build; mkdir -p "$out"
cat_=$root/userspace/ccl/catalogs/native-runtime-services.ccl
m=$root/userspace/ccl/build/manifest/ccl-manifest
"$m" "$cat_" "$here/manifest-bench.ccl" > "$out/bench-manifest.S"
"$m" "$cat_" "$here/manifest-child.ccl" > "$out/child-manifest.S"
as --64 "$out/bench-manifest.S" -o "$out/bench-manifest.o"
as --64 "$out/child-manifest.S" -o "$out/child-manifest.o"
python3 "$root/tests/process-spawn/launch-table.py" kern014-child.app > "$out/launch.S"
as --64 "$out/launch.S" -o "$out/launch.o"
"$root/userspace/libc/cubit-cc" -O2 -g -Wall -o "$root/kernel/isodir/boot/kern014-bench.app" \
    "$here/kern014-bench.c" "$out/launch.o" --manifest "$out/bench-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -g -Wall -o "$root/kernel/isodir/boot/kern014-child.app" \
    "$here/kern014-child.c" --manifest "$out/child-manifest.o"
echo built
