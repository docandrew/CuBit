#!/usr/bin/env bash
#  The authority delegation model (CAP-012, docs/authority-model.md).
#  Run in the Nix shell, under a memory cap, for example:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      nix develop -c bash tests/authority-model/run.sh --prove
#  Builds and runs the CCL inventory check and the exhaustive check
#  (depth 5 by default, about a minute; --deep searches depth 6 with up to
#  40 M states); --prove also runs gnatprove on the SPARK model at level 1.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
depth=5
states=3000000
prove=0
for arg in "$@"; do
    case $arg in
        --prove) prove=1 ;;
        --deep) depth=6; states=40000000 ;;
        *) echo "usage: $0 [--prove] [--deep]" >&2; exit 2 ;;
    esac
done
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/model.gpr"
"$here/build/inventory_check" "$here"
"$here/build/exhaustive" "$depth" "$states"
if [[ $prove == 1 ]]; then
    alr exec -- gnatprove -P "$here/prove.gpr" --level=1 -j2 \
        --checks-as-errors=on --report=fail
    out="$here/build/prove/gnatprove/gnatprove.out"
    grep -E '^Total' "$out"
    if grep -Eq ': (low|medium|high):|unproved' "$here/build/prove/gnatprove"/*.spark 2>/dev/null \
       || ! grep -Eq '^Total .* \. +\. *$' "$out"; then
        echo "authority model: proof incomplete" >&2
        exit 1
    fi
fi
