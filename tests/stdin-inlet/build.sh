#!/usr/bin/env bash
# The stdin-inlet guest test: stdin-check.app (the unix.stdin inlet on
# descriptor 0), stdin-none.app (no inlet) and the launcher into $1
# (default kernel/isodir/boot). Run in the Nix shell, under the shared build
# lock, after `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"
manifest() {
    "$repo/userspace/ccl/build/manifest/ccl-manifest" \
        "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$1" \
        --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" "${@:3}" > "$b/$2.S"
    as --64 "$b/$2.S" -o "$b/$2.o"
}
manifest "$here/manifest.ccl" stdin-check
"$repo/userspace/libc/cubit-cc" -O2 -Wall -Wextra -Werror -o "$out/stdin-check.app" \
    "$here/stdin-check.c" --manifest "$b/stdin-check.o"
manifest "$here/none-manifest.ccl" stdin-none
"$repo/userspace/libc/cubit-cc" -O2 -Wall -Wextra -Werror -DNO_INLET -o "$out/stdin-none.app" \
    "$here/stdin-check.c" --manifest "$b/stdin-none.o"
mkdir -p "$here/check/build/generated"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/check/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
    --startup-output "$here/check/build/generated/startup.ads" > "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/stdin-inlet-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$here/check/stdin_inlet_check.gpr")
cp "$here/check/build/stdin-inlet-check.app" "$out/stdin-inlet-check.app"
echo "stdin-inlet test: $out"
