#!/usr/bin/env bash
# Hosted CCL Bible binding tests (APP-005). Run inside nix develop with
# TMPDIR outside /tmp. Builds the Bible images (tests/bible) first.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
images="$root/tests/bible/build/bible"
mkdir -p "$images" "$here/build"
if [ ! -s "$images/web.bible" ] || [ "$root/tools/build_bible.py" -nt "$images/web.bible" ]; then
  python3 -I "$root/tools/build_bible.py" "$images"
fi
(cd "$root/kernel" && alr exec -- gprbuild -q -p -j8 -P "$here/ccl_bible_tests.gpr")
cd "$here" && CCL_BIBLE_DIR="$images" ./build/main "$@"
