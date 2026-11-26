#!/usr/bin/env bash
# A program whose main is in an archive links (Ladybird's LibMain keeps
# main in liblagom-main.a): the start object (crt/crt1.S) names main, as
# musl's and glibc's crt1 do. Without that, the libc's own reference comes
# after the archive in the link order and the link fails ("undefined
# reference to `main'"). Run in the Nix shell after `make -C kernel libc`.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/main-in-archive.XXXXXX")
trap 'rm -rf "$work"' EXIT
printf 'int main(void) { return 0; }\n' > "$work/main.c"
"$here/../cubit-cc" -O2 -c "$work/main.c" -o "$work/main.o"
ar rcs "$work/libmain.a" "$work/main.o"
printf 'int unused_unit(void) { return 1; }\n' > "$work/unit.c"
"$here/../cubit-cc" -O2 -c "$work/unit.c" -o "$work/unit.o"
"$here/../cubit-cc" -o "$work/program" "$work/unit.o" "$work/libmain.a"
nm "$work/program" | grep -q ' T main$'
echo "PASS main in an archive links"
