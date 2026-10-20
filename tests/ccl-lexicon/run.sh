#!/usr/bin/env bash
# Hosted CCL lexicon binding tests (docs/lexicon.md). Run inside nix develop
# with TMPDIR outside /tmp. Builds the lexicon image (tests/lexicon) first.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
image="$root/tests/lexicon/build/english.lexicon"
mkdir -p "$root/tests/lexicon/build" "$here/build"
if [ ! -s "$image" ] || [ "$root/tools/build_lexicon.py" -nt "$image" ]; then
  python3 -I "$root/tools/build_lexicon.py" "$image" --notice "$root/tests/lexicon/build/LEXICON-NOTICE"
fi
(cd "$root/kernel" && alr exec -- gprbuild -q -p -j8 -P "$here/ccl_lexicon_tests.gpr")
cd "$here" && CCL_LEXICON_IMAGE="$image" ./build/main "$@"
