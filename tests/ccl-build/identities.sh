#!/usr/bin/env bash
# Every component's output identities, for comparing two ccl-builds, two
# stores or two checkouts (docs/ccl-build.md, "Identity reports"):
#
#   identities.sh TOOL STORE SOURCE-ROOT OUT [COMPONENT.ccl...]
#
# Builds each component (default: every component.ccl of the apps, services
# and CCL services in SOURCE-ROOT) with TOOL into STORE, and writes
# OUT/<executable>.products: every step output's path, kind and identity, and
# the pinned trees, read from the build's root record (store_driver products).
# OUT/summary has one line per component: steps run, steps answered, or the
# failure. Compare two OUT directories with diff -r. Run inside `nix develop`
# (CUBIT_GNAT, CUBIT_BINUTILS) with TMPDIR set.
set -uo pipefail

tool=$1; store=$2; root=$3; out=$4; shift 4
here=$(cd "$(dirname "$0")" && pwd)
driver=$here/build/store_driver
: "${CUBIT_GNAT:?run inside nix develop}" "${CUBIT_BINUTILS:?run inside nix develop}"
imports=(--import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS")
mkdir -p "$out" "$store"
components=("$@")
if [[ ${#components[@]} == 0 ]]; then
    mapfile -t components < <(cd "$root" && ls userspace/apps/*/component.ccl userspace/services/*/component.ccl \
        userspace/ccl/services/*/component.ccl)
fi
: >"$out/summary"
started=$SECONDS
for c in "${components[@]}"; do
    marker=$out/.marker
    touch "$marker"; sleep 0.01
    log=$out/$(echo "$c" | tr / _).log
    if "$tool" build --store "$store" --source-root "$root" "${imports[@]}" "$root/$c" >"$log" 2>&1; then
        record=$(find "$store/roots" -type f ! -name checkout -newer "$marker" -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)
        name=$(basename "$record")
        "$driver" products "$record" >"$out/$name.products"
        echo "$c $name $(grep -Eo '^ccl-build: [0-9]+ ran, [0-9]+ answered' "$log" | tail -1)" >>"$out/summary"
    else
        echo "$c FAILED: $(tail -1 "$log")" >>"$out/summary"
    fi
done
rm -f "$out/.marker"
echo "identities: $(grep -vc FAILED "$out/summary") built, $(grep -c FAILED "$out/summary") failed, in $((SECONDS - started)) s"
