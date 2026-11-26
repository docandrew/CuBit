#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# The image-share guest test (KERN-006 layer 1): image-share-child.app (an
# 8 MiB read-only blob), image-share-twin.app (the same segments, another
# fill) and the launcher, image-share-check.app, into $1 (default
# kernel/isodir/boot). Run in the Nix shell, under the shared build lock,
# after `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/child-manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "$b/child.S"
as --64 "$b/child.S" -o "$b/child.o"
"$repo/userspace/libc/cubit-cc" -O2 -Wall -Wextra -Werror -o "$out/image-share-child.app" \
    "$here/child.c" --manifest "$b/child.o"
"$repo/userspace/libc/cubit-cc" -O2 -Wall -Wextra -Werror -DIMAGE_SHARE_TWIN -o "$out/image-share-twin.app" \
    "$here/child.c" --manifest "$b/child.o"
# The twin is only a test of the byte comparison if the kernel's key (the
# entry point and the read-only segments) is the same as the copies'.
segments() { readelf -lW "$1" | grep -E '^ +LOAD|Entry point'; }
if ! diff <(segments "$out/image-share-child.app") <(segments "$out/image-share-twin.app") >/dev/null; then
    echo "image-share test: the twin's segments differ from the child's" >&2
    exit 1
fi
mkdir -p "$here/check/build/generated"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/check/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
    --startup-output "$here/check/build/generated/startup.ads" > "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/image-share-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$here/check/image_share_check.gpr")
cp "$here/check/build/image-share-check.app" "$out/image-share-check.app"
echo "image-share test: $out"
