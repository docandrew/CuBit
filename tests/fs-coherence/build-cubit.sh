#!/usr/bin/env bash
# Builds fs-coherence-CASE.app for every case in cases.txt into
# kernel/isodir/boot (one program per case: a startup profile passes no
# arguments). Needs the libc and ccl-manifest built (make -C kernel libc
# ccl-manifest); hold coordination/build.lock.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$here/build
mkdir -p "$out"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" > "$out/manifest.S"
as --64 "$out/manifest.S" -o "$out/manifest.o"
while read -r name <&3; do
    program=fs-coherence-$name.app
    # It may start only itself. INTERIM launch table: see
    # tests/process-spawn/launch-table.py.
    python3 "$root/tests/process-spawn/launch-table.py" "$program" > "$out/$name-launch.S"
    as --64 "$out/$name-launch.S" -o "$out/$name-launch.o"
    "$root/userspace/libc/cubit-cc" -O2 -g -Wall -DCUBIT "-DCASE=\"$name\"" "-DPROGRAM=\"$program\"" \
        -o "$root/kernel/isodir/boot/$program" "$here/fs-coherence.c" "$out/$name-launch.o" \
        --manifest "$out/manifest.o"
done 3< "$here/cases.txt"
