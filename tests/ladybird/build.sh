#!/usr/bin/env bash
# The ladybird-js and ladybird-test262 guest tests (WEB-006 phase a): the
# test launchers into $1 (default kernel/isodir/boot), with Ladybird's
# programs as the Ladybird port repository stages them (cubit-ladybird:
# scripts/stage.sh kernel/build/ladybird, or LADYBIRD_STAGE):
# ladybird-js.app and ladybird-test262.app, built unmodified as Linux/musl
# programs and signed, beside them (test262 itself stays in the stage,
# test262/); and the files the js test reads from @nvme:0/ladybird
# (check.js, ICU's data file) into tests/ladybird/build/disk. Without a
# staged js the tests cannot run. Run in the Nix shell, under the shared
# build lock, after `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
stage=${LADYBIRD_STAGE:-$repo/kernel/build/ladybird}
if [ ! -f "$stage/ladybird-js.app" ]; then
    echo "ladybird test: no staged $stage/ladybird-js.app (cubit-ladybird: scripts/stage.sh $stage)" >&2
    exit 1
fi
mkdir -p "$out" "$here/build/disk"
cp "$stage/ladybird-js.app" "$out/ladybird-js.app"
cp "$here/check.js" "$here/build/disk/check.js"
cp "$stage/data/"icudt*l.dat "$here/build/disk/"

# A launcher: its manifest (and generated Startup package), then gprbuild.
launcher() {
    local dir=$1 project=$2 app=$3
    mkdir -p "$dir/build/generated"
    "$repo/userspace/ccl/build/manifest/ccl-manifest" \
        "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$dir/manifest.ccl" \
        --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
        --startup-output "$dir/build/generated/startup.ads" > "$dir/build/manifest.S"
    as --64 "$dir/build/manifest.S" -o "$dir/build/manifest.o"
    rm -f "$dir/build/$app"
    (cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$dir/$project")
    cp "$dir/build/$app" "$out/$app"
}
launcher "$here/check" ladybird_check.gpr ladybird-js-check.app
if [ -f "$stage/ladybird-test262.app" ]; then
    cp "$stage/ladybird-test262.app" "$out/ladybird-test262.app"
    # The list of tests, linked into the launcher, which feeds it to the
    # runner's unix.stdin inlet a batch at a time.
    mkdir -p "$here/test262/build"
    printf '\t.section .rodata\n\t.globl ladybird_test262_list_start\n\t.globl ladybird_test262_list_end\nladybird_test262_list_start:\n\t.incbin "%s"\nladybird_test262_list_end:\n\t.section .note.GNU-stack,"",@progbits\n' \
        "$stage/test262/list.txt" > "$here/test262/build/list.S"
    as --64 "$here/test262/build/list.S" -o "$here/test262/build/list.o"
    launcher "$here/test262" ladybird_test262_check.gpr ladybird-test262-check.app
fi
echo "ladybird tests: $out, disk files in $here/build/disk and $stage/test262"
