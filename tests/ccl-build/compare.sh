#!/usr/bin/env bash
# Byte comparison of programs built by ccl-build against the same programs
# built by gprbuild through make, both with GNAT 15.3 (docs/ccl-build.md,
# "Migration from make"). Run inside `nix develop`, after make has built the
# targets with gprbuild:
#   TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/ccl-build/compare.sh \
#       userspace/apps/futex-check [more program directories...]
# Compared: the executable and every object ccl-build produced (manifest.o,
# each unit's .o, b__main.o) and the manifest assembly and startup bindings.
# .ali files and b__main.ad[sb] record paths and are not compared.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
: "${TMPDIR:?set TMPDIR outside /tmp}"
: "${CCL_BUILD_STORE:?set CCL_BUILD_STORE (the Storehouse directory)}"
tool=$repo/userspace/ccl/build/ccl-build/ccl-build
scratch=$(mktemp -d "$TMPDIR/ccl-build-compare.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

same=0; different=0
for program in "$@"; do
    out=$scratch/$(basename "$program")
    if ! "$tool" build --store "$CCL_BUILD_STORE" --source-root "$repo" \
         --import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS" \
         --out "$out" "$repo/$program/component.ccl" >"$out.log" 2>&1; then
        echo "FAIL  $program: ccl-build failed"; tail -5 "$out.log"; different=$((different + 1)); continue
    fi
    result=SAME
    while IFS= read -r -d '' file; do
        relative=${file#"$out/"}
        case $relative in *.ali|*/b__*.adb|*/b__*.ads) continue ;; esac
        if ! cmp -s "$file" "$repo/$relative"; then
            result=DIFFERENT; echo "      differs: $relative"
        fi
    done < <(find "$out" -type f -print0)
    ran=$(grep -Eo '^ccl-build: [0-9]+ ran' "$out.log" | grep -Eo '[0-9]+')
    steps=$(grep -Ec '^(ran|hit)  ' "$out.log")
    echo "$result  $program ($steps steps, $ran ran)"
    if [[ $result == SAME ]]; then same=$((same + 1)); else different=$((different + 1)); fi
done
echo "compare: $same identical, $different different or failed"
[[ $different == 0 ]]
