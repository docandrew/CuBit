#!/usr/bin/env bash
# The gcc guest test (userspace/ports/gcc, stages 2 and 3): build cc1 and
# the driver, the test launcher, and the Linux references (the same GCC 15.3.0
# cc1 and binutils, from the same input) into $1 (default kernel/isodir/boot).
# as.app and binutils-compare.app come from tests/binutils/build.sh. Run in
# the Nix shell, under the shared build lock, after
# `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"

bash "$repo/userspace/ports/gcc/build.sh" "$out"

mkdir -p "$here/check/build"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/check/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/gcc-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild -q -P "$here/check/gcc_check.gpr")
cp "$here/check/build/gcc-check.app" "$out/gcc-check.app"
# The archive-walk diagnostic (C).
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/walk/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "$b/walk-manifest.S"
as --64 "$b/walk-manifest.S" -o "$b/walk-manifest.o"
"$repo/userspace/libc/cubit-cc" -O2 -Wall -o "$out/archive-walk.app" \
    "$here/walk/walk.c" --manifest "$b/walk-manifest.o"

# The references: the same cc1 (GCC 15.3.0 from the pinned nixpkgs) and as
# on Linux, with the options gcc-check passes.
cc1=$(nix build --inputs-from "$repo" nixpkgs#gcc15.cc --no-link --print-out-paths |
      while read -r output; do
          [ -d "$output/libexec/gcc" ] && find "$output/libexec/gcc" -name cc1 -type f
      done | head -1)
binutils=$(nix build --inputs-from "$repo" nixpkgs#binutils-unwrapped \
           --no-link --print-out-paths | grep -v -- "-man$\|-info$\|-dev$\|-lib$" | head -1)
reference=$(mktemp -d "${TMPDIR:-/tmp}/gcc-reference.XXXXXX")
# The same names as on CuBit: the output records the input's name (cc1
# runs in the work place; as gets CuBit names, as as.app's parameters do).
work="@nvme:0/work"
mkdir -p "$reference/$work"
cp "$here/hello.c" "$reference/$work/hello.c"
(cd "$reference/$work" && "$cc1" -quiet -O2 -fno-pie hello.c -o hello.s)
(cd "$reference" && "$binutils/bin/as" --64 -o "$work/hello.o" "$work/hello.s")
# Stage 3: the driver, run in the work place on relative names, as gcc-check
# runs it there: the unwrapped driver with the same as and ld, and the
# toolchain tree's specs and files (-B), less what this driver adds by
# default and the CuBit one is configured without (PIE, the LTO plugin).
gcc_driver=$(nix build --inputs-from "$repo" nixpkgs#gcc15.cc --no-link --print-out-paths |
             while read -r output; do
                 [ -x "$output/bin/gcc" ] && echo "$output/bin/gcc"
             done | head -1)
tree=$repo/userspace/ports/gcc/build/toolchain
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/hello-manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "$b/hello-manifest.s"
cp "$b/hello-manifest.s" "$reference/$work/"
reference_gcc() {
    (cd "$reference/$work" && env -u LIBRARY_PATH -u COMPILER_PATH "$gcc_driver" \
        -specs="$tree/lib/gcc/x86_64-linux-musl/15.3.0/specs" -B"$binutils/bin/" \
        -B"$tree/lib/gcc/x86_64-linux-musl/15.3.0/" -B"$tree/lib/" \
        -no-pie -fno-use-linker-plugin "$@")
}
reference_gcc -O2 -fno-pie -c hello.c -o hello-driver.o
reference_gcc -O2 -fno-pie hello.c hello-manifest.s -o hello
cp "$reference/$work/hello.s" "$b/expected-hello.s"
cp "$reference/$work/hello.o" "$b/expected-hello.o"
cp "$reference/$work/hello-driver.o" "$b/expected-hello-driver.o"
cp "$reference/$work/hello" "$b/expected-hello"
rm -rf "$reference"
echo "gcc test: $out, references in $b"
