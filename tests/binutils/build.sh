#!/usr/bin/env bash
# The binutils guest test (userspace/ports/binutils): build the tools, the
# test launcher and comparer, and the reference outputs, into $1 (default kernel/isodir/boot).
# Reference outputs come from nixpkgs' unwrapped binutils of the same
# version, on Linux, from the same input. Run in the Nix shell, under the
# shared build lock, after `make -C kernel libc user_runtime ccl-manifest`.
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
b=$here/build
mkdir -p "$b" "$out"

bash "$repo/userspace/ports/binutils/build.sh" "$out"

manifest() {  # manifest SOURCE OBJECT
    "$repo/userspace/ccl/build/manifest/ccl-manifest" \
        "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$1" \
        --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" > "${2%.o}.S"
    as --64 "${2%.o}.S" -o "$2"
}
# The launcher (Ada): typed parameters through CuBit.Program_Parameters.
mkdir -p "$here/check/build"
manifest "$here/check/manifest.ccl" "$here/check/build/manifest.o"
rm -f "$here/check/build/binutils-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild -q -P "$here/check/binutils_check.gpr")
cp "$here/check/build/binutils-check.app" "$out/binutils-check.app"
# The comparer (C).
manifest "$here/compare.ccl" "$b/compare-manifest.o"
"$repo/userspace/libc/cubit-cc" -O2 -Wall -o "$out/binutils-compare.app" \
    "$here/compare.c" --manifest "$b/compare-manifest.o"

linux=$(nix build --inputs-from "$repo" nixpkgs#binutils-unwrapped \
        --no-link --print-out-paths | grep -v -- "-man$\|-info$\|-dev$\|-lib$" | head -1)
# Same file names as on CuBit: ld records its inputs' names.
reference=$(mktemp -d "${TMPDIR:-/tmp}/binutils-reference.XXXXXX")
cp "$here/hello.s" "$reference/hello.s"
(cd "$reference" && "$linux/bin/as" --64 -o hello.o hello.s &&
 "$linux/bin/ld" -static -o hello.elf hello.o)
cp "$reference/hello.o" "$b/expected-hello.o"
cp "$reference/hello.elf" "$b/expected-hello.elf"
rm -rf "$reference"
echo "binutils test: $out, references in $b"
