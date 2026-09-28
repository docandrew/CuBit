#!/usr/bin/env bash
#  Host check of the runtime's memmove/memcpy/memset/memcmp. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
rt=$here/../../userspace/runtime/gnat
build=$here/build
mkdir -p "$build"
gnat_gcc=$(dirname "$(command -v gnat)")/gcc
# Nix hardening would route memmove through glibc's fortified __memmove_chk.
export NIX_HARDENING_ENABLE=""
(cd "$build" && "$gnat_gcc" -c -O2 -gnatp -I"$rt" "$rt/cubit-string.adb")
gcc -O1 -fno-builtin -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -o "$build/check" "$here/check.c" "$build/cubit-string.o"
"$build/check"
