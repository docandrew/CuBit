#!/usr/bin/env bash
# The control-events guest test (docs/data-plane.md): control-check.app,
# control-child.app and control-producer.app into $1 (default kernel/isodir/boot). Run in the Nix
# shell, under the shared build lock, after
# `make -C kernel user_runtime ccl-manifest`.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
mkdir -p "$out"
for part in check child producer; do
    mkdir -p "$here/$part/build/generated"
    "$repo/userspace/ccl/build/manifest/ccl-manifest" \
        "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/$part/manifest.ccl" \
        --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
        --ada-output "$here/$part/build/generated/ccl_manifest_bindings.ads" > "$here/$part/build/manifest.S"
    as --64 "$here/$part/build/manifest.S" -o "$here/$part/build/manifest.o"
    rm -f "$here/$part/build/control-$part.app"
    (cd "$repo/kernel" && alr exec -- gprbuild -q -P "$here/$part/control_$part.gpr")
    cp "$here/$part/build/control-$part.app" "$out/control-$part.app"
done
