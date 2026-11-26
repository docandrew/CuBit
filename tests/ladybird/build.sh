#!/usr/bin/env bash
# The ladybird-js guest test (WEB-006 phase a): the test launcher into $1
# (default kernel/isodir/boot), with Ladybird's js as the Ladybird port
# repository stages it (cubit-ladybird: scripts/stage.sh
# kernel/build/ladybird, or LADYBIRD_STAGE): ladybird-js.app, built
# unmodified as a Linux/musl program and signed, beside it; and the files
# the test reads from @nvme:0/ladybird (check.js, ICU's data file) into
# tests/ladybird/build/disk. Without a staged js the test cannot run. Run in
# the Nix shell, under the shared build lock, after
# `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
stage=${LADYBIRD_STAGE:-$repo/kernel/build/ladybird}
if [ ! -f "$stage/ladybird-js.app" ]; then
    echo "ladybird-js test: no staged $stage/ladybird-js.app (cubit-ladybird: scripts/stage.sh $stage)" >&2
    exit 1
fi
mkdir -p "$out" "$here/build/disk"
cp "$stage/ladybird-js.app" "$out/ladybird-js.app"
cp "$here/check.js" "$here/build/disk/check.js"
cp "$stage/data/"icudt*l.dat "$here/build/disk/"

mkdir -p "$here/check/build/generated"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/check/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
    --startup-output "$here/check/build/generated/startup.ads" > "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/ladybird-js-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$here/check/ladybird_check.gpr")
cp "$here/check/build/ladybird-js-check.app" "$out/ladybird-js-check.app"
echo "ladybird-js test: $out, disk files in $here/build/disk"
