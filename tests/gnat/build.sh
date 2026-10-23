#!/usr/bin/env bash
# The GNAT guest test (userspace/ports/gcc, plan steps 4 and 5): build the
# toolchain (gcc, gnat1, gnatbind, libgnat), the test launcher, and the Linux
# references into $1 (default kernel/isodir/boot). binutils-compare.app and
# as.app come from tests/binutils/build.sh. Run in the Nix shell, under the
# shared build lock, after `make -C kernel libc user_runtime ccl-manifest`.
#
# References, both on Linux from the same input:
# - the same GNAT 15.3.0 (nixpkgs' musl cross GNAT, decision D1) and
#   binutils, with the toolchain tree's specs and Ada run-time library:
#   hello.o, b~hello.o and the linked program, byte for byte;
# - the program built by the development shell's native GNAT and run on
#   Linux: its transcript.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"

bash "$repo/userspace/ports/gcc/build.sh" "$out"

manifest() {  # manifest SOURCE ASSEMBLY
    "$repo/userspace/ccl/build/manifest/ccl-manifest" \
        "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$1" \
        --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "$2"
}
mkdir -p "$here/check/build"
manifest "$here/check/manifest.ccl" "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/gnat-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$here/check/gnat_check.gpr")
cp "$here/check/build/gnat-check.app" "$out/gnat-check.app"
manifest "$here/hello-manifest.ccl" "$b/hello-manifest.s"

# The reference toolchain: the musl cross GNAT's own driver (unwrapped),
# with the CuBit specs, the toolchain tree's run-time library and support
# files, and nixpkgs' binutils.
gnat=$(nix build --inputs-from "$repo" "nixpkgs#pkgsCross.musl64.buildPackages.gnat15.cc" \
       --no-link --print-out-paths | grep -v -- "-man$\|-info$\|-lib$" | head -1)
binutils=$(nix build --inputs-from "$repo" nixpkgs#binutils-unwrapped \
           --no-link --print-out-paths | grep -v -- "-man$\|-info$\|-dev$\|-lib$" | head -1)
tree=$repo/userspace/ports/gcc/build/toolchain
library=$tree/lib/gcc/x86_64-linux-musl/15.3.0
reference=$(mktemp -d "${TMPDIR:-/tmp}/gnat-reference.XXXXXX")
# The same names as on CuBit, run in the work place on relative names, as
# gnat-check runs the tools there.
work="$reference/@nvme:0/work"
mkdir -p "$work"
cp "$here/hello.adb" "$b/hello-manifest.s" "$work/"
reference_gcc() {
    (cd "$work" && env -u LIBRARY_PATH -u COMPILER_PATH -u ADA_INCLUDE_PATH -u ADA_OBJECTS_PATH \
        "$gnat/bin/x86_64-unknown-linux-musl-gcc" \
        -specs="$library/specs" -B"$binutils/bin/" -B"$library/" -B"$tree/lib/" \
        --RTS="$library" -no-pie -fno-use-linker-plugin "$@")
}
reference_gcc -O2 -fno-pie -c hello.adb
(cd "$work" && "$gnat/bin/x86_64-unknown-linux-musl-gnatbind" --RTS="$library" -x hello.ali)
reference_gcc -O2 -fno-pie -c -gnatA -gnatWb -gnatiw -gnatws b~hello.adb
reference_gcc -O2 -fno-pie b~hello.o hello.o hello-manifest.s -L"$library/adalib" -lgnat -o hello
cp "$work/hello.o" "$b/expected-hello.o"
cp "$work/b~hello.o" "$b/expected-b~hello.o"
cp "$work/hello" "$b/expected-hello"

# The same program built and run on Linux by the native GNAT.
native="$reference/native"
mkdir -p "$native"
cp "$here/hello.adb" "$native/"
(cd "$native" && gnatmake -q -O2 hello.adb)
# hello exits with 7 (Ada.Command_Line.Set_Exit_Status).
status=0
(cd "$native" && ./hello > /dev/null) || status=$?
[ "$status" = 7 ]
cp "$native/transcript.txt" "$b/expected-transcript.txt"
rm -rf "$reference"
echo "gnat test: $out, references in $b"
